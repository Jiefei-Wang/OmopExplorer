#######################################
# utilities
#######################################
normalize_search_params <- function(params) {
    if (is.null(params) || length(params) == 0) {
        return(list())
    }

    is_item <- function(x) is.list(x) && !is.null(x$var)
    is_group <- function(x) is.list(x) && !is.null(x$relation) && !is.null(x$items)

    # Named list shorthand: list(col1 = value1, col2 = value2)
    if (!is.null(names(params)) && any(nzchar(names(params)))) {
        items <- lapply(names(params), function(nm) {
            list(var = nm, operator = "=", value = params[[nm]])
        })
        return(list(list(relation = "AND", items = items)))
    }

    if (all(vapply(params, is_group, logical(1)))) {
        return(params)
    }

    if (all(vapply(params, is_item, logical(1)))) {
        return(list(list(relation = "AND", items = params)))
    }

    list()
}

build_like_expr <- function(var_sym, value, case_insensitive = TRUE) {
    val <- as.character(value)
    if (!grepl("[%_]", val)) {
        val <- paste0("%", val, "%")
    }
    if (case_insensitive) {
        rlang::quo(stringr::str_like(tolower(!!var_sym), tolower(!!val)))
    } else {
        rlang::quo(stringr::str_like(!!var_sym, !!val))
    }
}

numeric_var_expr <- function(var_sym) {
    rlang::expr(as.numeric(
        dplyr::if_else(
            stringr::str_detect(!!var_sym, "\\\\([0-9.\\-]+\\\\)"),
            stringr::str_replace_all(!!var_sym, "^.*\\\\(|\\\\).*", ""),
            NA_character_
        )
    ))
}

build_item_expr <- function(item, col_types) {
    var_sym <- rlang::sym(item$var)
    op <- toupper(item$operator %||% "=")
    val <- item$value
    col_type <- col_types[[item$var]] %||% NA_character_
    needs_numeric_cast <- is.numeric(val) && !is.na(col_type) && col_type %in% c("character")
    var_expr <- if (needs_numeric_cast) numeric_var_expr(var_sym) else var_sym

    if (needs_numeric_cast && op %in% c(">", ">=") && val >= 0 && val < 1) {
        return(rlang::quo(!stringr::str_like(!!var_sym, "%(0)%")))
    }
    if (needs_numeric_cast && op %in% c("<", "<=") && val > 0 && val <= 1) {
        return(rlang::quo(stringr::str_like(!!var_sym, "%(0)%")))
    }

    if (op == "=") return(rlang::quo(!!var_expr == !!val))
    if (op == "!=") return(rlang::quo(!!var_expr != !!val))
    if (op == ">") return(rlang::quo(!!var_expr > !!val))
    if (op == ">=") return(rlang::quo(!!var_expr >= !!val))
    if (op == "<") return(rlang::quo(!!var_expr < !!val))
    if (op == "<=") return(rlang::quo(!!var_expr <= !!val))
    if (op == "IN") return(rlang::quo(!!var_expr %in% !!val))
    if (op == "NOT IN") return(rlang::quo(!(!!var_expr %in% !!val)))
    if (op == "BETWEEN") {
        return(rlang::quo(!!var_expr >= !!val[[1]] & !!var_expr <= !!val[[2]]))
    }
    if (op == "LIKE") return(build_like_expr(var_sym, val, case_insensitive = FALSE))
    if (op == "ILIKE") return(build_like_expr(var_sym, val, case_insensitive = TRUE))

    rlang::quo(!!var_expr == !!val)
}

apply_filters <- function(q, params, col_types) {
    groups <- normalize_search_params(params)
    if (length(groups) == 0) {
        return(q)
    }

    combine_exprs <- function(exprs, rel) {
        Reduce(function(a, b) {
            if (rel == "OR") {
                rlang::quo((!!a) | (!!b))
            } else {
                rlang::quo((!!a) & (!!b))
            }
        }, exprs)
    }

    group_exprs <- lapply(groups, function(group) {
        rel <- toupper(group$relation %||% "AND")
        items <- group$items %||% list()
        item_exprs <- lapply(items, build_item_expr, col_types = col_types)
        if (length(item_exprs) == 0) {
            return(NULL)
        }
        combine_exprs(item_exprs, rel)
    })
    group_exprs <- Filter(Negate(is.null), group_exprs)
    if (length(group_exprs) == 0) {
        return(q)
    }

    dplyr::filter(q, !!!group_exprs)
}

build_order_exprs <- function(order_params) {
    if (is.null(order_params) || length(order_params) == 0) {
        return(list())
    }
    lapply(names(order_params), function(nm) {
        sym <- rlang::sym(nm)
        if (isTRUE(order_params[[nm]])) {
            rlang::quo(!!sym)
        } else {
            rlang::quo(dplyr::desc(!!sym))
        }
    })
}

collect_search_vars <- function(params) {
    groups <- normalize_search_params(params)
    vars <- unlist(lapply(groups, function(group) {
        items <- group$items %||% list()
        vapply(items, function(item) item$var %||% NA_character_, character(1))
    }))
    unique(vars[!is.na(vars) & nzchar(vars)])
}

#######################################
# filter_data
#######################################
# the maximum row index to fetch using head()
head_max_limit <- 2000

# The function to filter, order, and paginate data.
#
# Inputs:
#   query: a database connection
#   show_columns: a character vector of columns to show
#   params_search: a list of groups combined with AND, each group is a list:
#     - relation: "AND" or "OR"
#     - items: list of items, each item is a list with:
#       - var: column name in the query
#       - operator: one of "=", "!=", ">", ">=", "<", "<=", "LIKE", "ILIKE", "IN",
#                   "NOT IN", "BETWEEN"
#       - value: value to compare against
#     Example item: list(var = "age", operator = ">=", value = 30)
#     Example date item: list(var = "birth_date", operator = "BETWEEN",
#                             value = c("1980-01-01", "1990-12-31"))
#   Note: if the group is a single item, the item can be directly in params_search, e.g. params_search = list(list(var = "age", operator = ">", value = 30))
#   params_order: a named list of order parameters (TRUE for ascending,
#                 FALSE for descending)
#   row_start: integer, starting row index (0-based)
#   row_length: integer, number of rows to return
#
# Returns: The required data
filter_data <- function(query, show_columns, params_search, params_order = NULL, row_start = 0, row_length = 100) {
    col_types <- get_column_type(query)
    `%||%` <- function(x, y) if (is.null(x)) y else x

    search_vars <- collect_search_vars(params_search)
    order_vars <- names(params_order %||% list())
    needed_cols <- unique(c(show_columns, order_vars, search_vars))
    if (length(needed_cols) > 0) {
        query <- dplyr::select(query, dplyr::all_of(needed_cols))
    }

    q <- apply_filters(query, params_search, col_types)

    order_exprs <- build_order_exprs(params_order)

    q <- q |> mutate(shiny_row_count = n())
    
    if (length(order_exprs) > 0) {
        q <- dplyr::arrange(q, !!!order_exprs)
    }
    start <- row_start %||% 0
    n_fetch <- start + row_length
    select_cols <- unique(c(show_columns, order_vars))
    if (n_fetch < head_max_limit) {
        q |>
        head(n_fetch)|>
        select(shiny_row_count, all_of(select_cols))->
        q
        
        out <- q |> collect()
        # cut to the required rows
        if (nrow(out) > 0){
            end_row <- min(n_fetch, nrow(out))
            out <- out[(start + 1):end_row, , drop = FALSE]
        }
    } else {
        q |>
        mutate(nr = row_number()) |>
        filter(nr > start & nr <= n_fetch) |>
        select(shiny_row_count, all_of(select_cols)) ->
        q

        out <- q |> collect()
    }

    if (nrow(out) > 0){
        recordsFiltered <- out$shiny_row_count[1]
        out$shiny_row_count <- NULL
    }else{
        # keep one here so DT should be happy
        recordsFiltered <- 1
    }
    dt <- out |> select(all_of(show_columns))

    list(
        data = dt,
        recordsFiltered = recordsFiltered
    )
}

get_column_type <- function(query) {
    tbl <- query |> head(0) |> collect()
    col_types <- lapply(tbl, function(x) class(x)[1])
    col_types
}

