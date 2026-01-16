#######################################
# Utility functions for search_deparser
#######################################

normalize_range_token <- function(value, is_date) {
    trimmed <- trimws(value %||% "")
    if (trimmed == "") return(trimmed)
    if (grepl("~", trimmed, fixed = TRUE)) return(trimmed)

    if (!is_date) {
        match <- regexec("^\\s*(-?\\d+(?:\\.\\d+)?)\\s*-\\s*(-?\\d+(?:\\.\\d+)?)\\s*$", trimmed)
        parts <- regmatches(trimmed, match)[[1]]
        if (length(parts) == 3) {
            return(paste0(parts[2], "~", parts[3]))
        }
    }

    trimmed
}

parse_inequality <- function(value) {
    trimmed <- trimws(value)
    if (trimmed == "") return(NULL)

    if (grepl("^(<=|>=|<|>)", trimmed)) {
        op <- trimws(sub("^([<>]=?).*$", "\\1", trimmed))
        remainder <- trimws(sub("^([<>]=?)\\s*(.*)$", "\\2", trimmed))
        return(list(operator = 'inequal', sign = op, value = remainder))
    }

    if (grepl("~", trimmed, fixed = TRUE)) {
        parts <- strsplit(trimmed, "~", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
            return(list(operator = "~", start = trimws(parts[1]), end = trimws(parts[2])))
        }
    }

    return(list(operator = "=", value = trimmed))
}

parse_numeric_filter <- function(value) {
    val <- normalize_range_token(value, is_date = FALSE)
    parsed <- parse_inequality(val)
    if (is.null(parsed)) return(NULL)

    if (parsed$operator == "=") {
        parsed_val <- toNum(parsed$value)
        if (is.na(parsed_val)) return(NULL)
        return(list(operator = "=", value = parsed_val))
    }

    if (parsed$operator == "~") {
        start <- toNum(parsed$start)
        end <- toNum(parsed$end)
        if (is.na(start) || is.na(end)) return(NULL)
        return(list(operator = "BETWEEN", value = c(start, end)))
    }

    if (parsed$operator == "inequal") {
        parsed_val <- toNum(parsed$value)
        if (is.na(parsed_val)) return(NULL)
        return(list(operator = parsed$sign, value = parsed_val))
    }

    NULL
}

parse_date_filter <- function(value, ctype) {
    val <- normalize_range_token(value, is_date = TRUE)
    parsed <- parse_inequality(val)
    if (is.null(parsed)) return(NULL)

    if (parsed$operator == "=") {
        parsed_val <- parse_partial_date(parsed$value, ctype)
        if (is.null(parsed_val)) return(NULL)
        if (parsed_val$type == "single") {
            return(list(operator = "=", value = parsed_val$value))
        }
        parts <- strsplit(parsed_val$value, "~", fixed = TRUE)[[1]]
        return(list(operator = "BETWEEN", value = c(parts[1], parts[2])))
    }

    if (parsed$operator == "~") {
        start_parsed <- parse_partial_date(parsed$start, ctype)
        end_parsed <- parse_partial_date(parsed$end, ctype)
        if (is.null(start_parsed) || is.null(end_parsed)) return(NULL)

        start_val <- if (start_parsed$type == "range") {
            strsplit(start_parsed$value, "~", fixed = TRUE)[[1]][1]
        } else {
            start_parsed$value
        }
        end_val <- if (end_parsed$type == "range") {
            strsplit(end_parsed$value, "~", fixed = TRUE)[[1]][2]
        } else {
            end_parsed$value
        }
        return(list(operator = "BETWEEN", value = c(start_val, end_val)))
    }

    if (parsed$operator == "inequal") {
        parsed_val <- as_date_value(parsed$value, ctype)
        if (is.null(parsed_val)) return(NULL)
        return(list(operator = parsed$sign, value = as.character(parsed_val)))
    }

    NULL
}

build_items_for_column <- function(col, value, col_types) {
    if (is.null(value) || !nzchar(trimws(value))) return(list())
    ctype <- col_types[[col]]
    if (is.null(ctype)) ctype <- NA_character_

    if (is_date_type(ctype)) {
        parsed <- parse_date_filter(value, ctype)
        if (is.null(parsed)) return(list())
        return(list(list(var = col, operator = parsed$operator, value = parsed$value)))
    }

    if (!is.na(ctype) && ctype %in% c("integer", "double", "numeric")) {
        parsed <- parse_numeric_filter(value)
        if (is.null(parsed)) return(list())
        return(list(list(var = col, operator = parsed$operator, value = parsed$value)))
    }

    if (!is.na(ctype) && ctype == "logical") {
        parsed_val <- as.logical(value)
        if (is.na(parsed_val)) return(list())
        return(list(list(var = col, operator = "=", value = parsed_val)))
    }

    list(list(var = col, operator = "ILIKE", value = value))
}

split_search_values <- function(value) {
    if (is.null(value)) return(character(0))
    parts <- unlist(strsplit(as.character(value), ",", fixed = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    parts[nzchar(parts)]
}

build_concept_items <- function(col, value, col_types, available_cols, concept_source_map) {
    items <- build_items_for_column(col, value, col_types)
    is_pure_number <- function(x) {
        if (is.null(x)) return(FALSE)
        trimmed <- trimws(x)
        if (!nzchar(trimmed)) return(FALSE)
        grepl("^-?\\d+(?:\\.\\d+)?$", trimmed)
    }
    if (is_pure_number(value)) {
        return(items)
    }
    source_col <- concept_source_map[[col]]
    if (is.null(source_col) || !(source_col %in% available_cols)) {
        candidates <- character(0)
        if (grepl("_concept_id$", col)) {
            candidates <- c(
                sub("_concept_id$", "_concept_source_value", col),
                sub("_concept_id$", "_source_value", col)
            )
        }
        candidates <- c(candidates, paste0(col, "_source_value"))
        candidates <- unique(candidates)
        for (candidate in candidates) {
            if (candidate %in% available_cols) {
                source_col <- candidate
                break
            }
        }
    }
    if (!is.null(source_col) && source_col %in% available_cols &&
        !is.null(value) && nzchar(trimws(value))) {
        items <- c(items, list(list(
            var = source_col,
            operator = "ILIKE",
            value = value
        )))
    }
    items
}


# Reduce the complexity of search parameters
# E.g. list(list(relation="AND", items=list(...)), list(relation="AND", items=list(...))) -> list(c(list(...), list(...)))
search_params_reducer <- function(params){
    if (is.null(params) || length(params) == 0) {
        return(list())
    }

    is_group <- function(x) is.list(x) && !is.null(x$items)
    groups <- Filter(is_group, params)
    if (length(groups) == 0) {
        return(list())
    }

    and_items <- list()
    other_groups <- list()
    for (group in groups) {
        rel <- toupper(if (is.null(group$relation)) "AND" else group$relation)
        items <- if (is.null(group$items)) list() else group$items
        if (length(items) == 0) {
            next
        }
        if (rel == "AND") {
            and_items <- c(and_items, items)
        } else {
            other_groups <- c(other_groups, list(list(relation = rel, items = items)))
        }
    }

    if (length(other_groups) == 0) {
        if (length(and_items) > 0) {
            return(and_items)
        }
        return(list())
    }

    out <- list()
    if (length(and_items) > 0) {
        out <- c(out, list(list(relation = "AND", items = and_items)))
    }
    out <- c(out, other_groups)
    out
}

#######################################
# search_deparser
#######################################

#' Deparse sidebar search input into filter_data parameters
#'
#' Translate sidebar search inputs into the `params_search` structure expected
#' by `filter_data`.
#'
#' Parsing rules:
#' - Inequality operators: `>`, `>=`, `<`, `<=`.
#' - Range operator: `~` (for dates and numbers).
#' - Comma-separated values (e.g. `1,2,3`) are treated as multiple values
#'   combined with OR.
#' - Date inputs support partial dates via `parse_partial_date()`.
#' - Character columns always use `ILIKE`.
#' - Global search scans all non-`shiny_` columns in the table.
#' - Concept id columns also search the mapped source value column when
#'   available. If the value is non-numeric, only the source value column is
#'   used. If the value is a pure number, the source value column is skipped.
#'
#' @param con List. Database connection created by `duckdbCon()` or `mockCon()`.
#' @param table_name Character scalar. Table name.
#' @param search_columns Character vector of columns to search. Can include
#'   a global name such as `search_anything`/`shiny_search_anything`.
#' @param search_value Character vector of search values.
#' @return List of grouped search items for `params_search`.
#' @keywords internal
search_deparser <- function(con, table_info, table_name, search_values) {
    `%||%` <- function(x, y) if (is.null(x)) y else x
    if (is.null(search_values) || length(search_values) == 0) return(list())

    query <- con[[table_name]]
    if (is.null(query)) return(list())

    col_types <- table_info[[table_name]]$column_types
    
    available_cols <- names(col_types)
    if (length(available_cols) == 0) return(list())

    concept_cols <- omop_concept_id_columns[[table_name]] %||% character(0)
    concept_source_map <- omop_concept_id_source_value_map[[table_name]] %||% list()


    groups <- list()
    for (i in seq_along(search_values)) {
        col <- names(search_values)[[i]]
        value <- search_values[[i]]
        if (is.na(value) || is.null(value) || !nzchar(trimws(value))) {
            next
        }

        items <- list()
        token_values <- split_search_values(value)
        if (length(token_values) == 0) {
            next
        }
        
        if (col %in% concept_cols) {
            for (token in token_values) {
                items <- c(items, build_concept_items(col, token, col_types, available_cols, concept_source_map))
            }
        } else {
            for (token in token_values) {
                items <- c(items, build_items_for_column(col, token, col_types))
            }
        }

        if (length(items) == 0) {
            next
        }

        relation <- if (length(items) > 1) "OR" else "AND"
        groups <- c(groups, list(list(relation = relation, items = items)))
    }

    search_params_reducer(groups)
}
