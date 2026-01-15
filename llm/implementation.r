source("llm\\utils.r")





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
# Returns: The required data frame after collect().
filter_data <- function(query, show_columns, params_search, params_order, row_start, row_length) {
    `%||%` <- function(x, y) if (is.null(x)) y else x
    col_types <- get_column_type(query)

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

    build_item_expr <- function(item) {
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

    apply_filters <- function(q, params) {
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
            item_exprs <- lapply(items, build_item_expr)
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

    search_vars <- collect_search_vars(params_search)
    order_vars <- names(params_order %||% list())
    needed_cols <- unique(c(show_columns, order_vars, search_vars))
    if (length(needed_cols) > 0) {
        query <- dplyr::select(query, dplyr::all_of(needed_cols))
    }

    q <- apply_filters(query, params_search)

    order_exprs <- build_order_exprs(params_order)

    if (!is.null(row_length)) {
        if (length(order_exprs) > 0) {
            q <- dplyr::arrange(q, !!!order_exprs)
        }
        start <- row_start %||% 0
        if (row_length <= 0) {
            empty <- query |> head(0) |> collect()
            return(empty[, show_columns, drop = FALSE])
        }

        n_fetch <- start + row_length
        q <- head(q, n_fetch)

        select_cols <- unique(c(show_columns, order_vars))
        q <- dplyr::select(q, dplyr::all_of(select_cols))
        out <- q |> collect()
        if (start >= nrow(out)) {
            return(out[0, show_columns, drop = FALSE])
        }
        end_row <- min(n_fetch, nrow(out))
        out <- out[(start + 1):end_row, , drop = FALSE]
        return(dplyr::select(out, dplyr::all_of(show_columns)))
    }

    if (length(order_exprs) > 0) {
        q <- dplyr::arrange(q, !!!order_exprs)
    }

    q <- dplyr::select(q, dplyr::all_of(show_columns))

    q |> collect()
}



concept_id_to_concept_name <- function(query, con, table_name, tbl_all_cols) {
    omop_concept_col <- omop_concept_id_columns[[table_name]]
    omop_source_map <- omop_concept_id_source_value_map[[table_name]]
    concept_col_list <- intersect(tbl_all_cols, omop_concept_col)

    if (length(concept_col_list) == 0) return(query)

    concept_tbl <- NULL
    if (!is.null(con$concept)) {
        concept_tbl <- dbplyr::remote_name(con$concept)
    } else if (!is.null(con$dbcon) && "concept" %in% DBI::dbListTables(con$dbcon)) {
        concept_tbl <- dbplyr::remote_name(dplyr::tbl(con$dbcon, "concept"))
    } else {
        stop("concept table is not available in the connection.")
    }

    mutation_list <- list()
    for (col in concept_col_list) {
        target_col <- paste0("shiny_", col)
        mapped_source_value <- omop_source_map[[col]]
        subquery <- paste0("(SELECT concept_name FROM ", concept_tbl, " WHERE concept_id = ", col, ")")

        if (is.null(mapped_source_value)) {
            mutation_list[[target_col]] <- dbplyr::sql(paste0(
                "CASE WHEN (", subquery, ") IS NULL ",
                "THEN NULL ",
                "ELSE CONCAT(", subquery, ", ' (', CAST(", col, " AS VARCHAR), ')') END"
            ))
        } else {
            mutation_list[[target_col]] <- dbplyr::sql(paste0(
                "CASE WHEN (", col, " IS NULL OR ", col, " = 0) ",
                "THEN CONCAT(COALESCE(", mapped_source_value, ", ''), ' (0)') ",
                "WHEN (", subquery, ") IS NULL THEN NULL ",
                "ELSE CONCAT(", subquery, ", ' (', CAST(", col, " AS VARCHAR), ')') END"
            ))
        }
    }

    query |> dplyr::mutate(!!!mutation_list)
}
