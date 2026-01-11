
sql_date_types <- function(tbl) {
    lapply(as.data.frame(head(tbl, 1)), function(x) class(x)[1])
}

is_date_type <- function(ctype) {
    !is.null(ctype) && ctype %in% c("Date", "POSIXct", "POSIXt")
}

parse_date_value <- function(value, ctype) {
    if (is.null(value)) return(NA)
    trimmed <- trimws(value)
    if (trimmed == "") return(NA)

    parsed <- suppressWarnings(lubridate::ymd_hms(trimmed, quiet = TRUE))
    if (!is.na(parsed)) {
        return(if (ctype == "Date") as.Date(parsed) else parsed)
    }

    parsed <- suppressWarnings(lubridate::ymd(trimmed, quiet = TRUE))
    if (!is.na(parsed)) {
        return(if (ctype == "Date") as.Date(parsed) else as.POSIXct(parsed))
    }

    parsed <- suppressWarnings(as.Date(trimmed))
    if (!is.na(parsed)) {
        return(if (ctype == "Date") parsed else as.POSIXct(parsed))
    }

    NA
}

build_date_filter_expr <- function(sym_col, search_value, ctype) {
    sv <- trimws(search_value)
    if (sv == "") return(NULL)

    if (grepl("~", sv, fixed = TRUE)) {
        parts <- strsplit(sv, "~", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
            start_val <- parse_date_value(parts[1], ctype)
            end_val <- parse_date_value(parts[2], ctype)
            if (!is.na(start_val) && !is.na(end_val)) {
                return(rlang::expr(!!sym_col >= !!start_val & !!sym_col <= !!end_val))
            }
        }
    }

    if (grepl("^(<=|>=|<|>)", sv)) {
        op <- sub("^([<>]=?).*$", "\\1", sv)
        remainder <- sub("^([<>]=?)\\s*(.*)$", "\\2", sv)
        parsed_val <- parse_date_value(remainder, ctype)
        if (!is.na(parsed_val)) {
            if (op == "<") return(rlang::expr(!!sym_col < !!parsed_val))
            if (op == "<=") return(rlang::expr(!!sym_col <= !!parsed_val))
            if (op == ">") return(rlang::expr(!!sym_col > !!parsed_val))
            if (op == ">=") return(rlang::expr(!!sym_col >= !!parsed_val))
        }
    }

    parsed_val <- parse_date_value(sv, ctype)
    if (!is.na(parsed_val)) {
        return(rlang::expr(!!sym_col == !!parsed_val))
    }

    NULL
}

# search all concept_id columns, return list of SQL filter expressions
sql_search_concept_cols <- function(con, concept_cols, concept_cols_values) {
    if (length(concept_cols) == 0) return(list())
    
    concept_tbl <- dbplyr::remote_name(con$concept)
    
    # Build IN clauses for each concept column with its specific search value
    filter_exprs <- lapply(seq_along(concept_cols), function(i) {
        col <- concept_cols[[i]]
        search_value <- concept_cols_values[[i]]
        
        search_term <- paste0("%", search_value, "%")
        search_value_num <- toNum(search_value)
        
        # Build the concept subquery for this specific search value
        concept_where_parts <- paste0("concept_name ILIKE '", search_term, "'")
        if (!is.na(search_value_num)) {
            concept_where_parts <- paste0(concept_where_parts, " OR concept_id = ", search_value_num)
        }
        
        concept_subquery <- paste0("SELECT concept_id FROM ", concept_tbl, " WHERE ", concept_where_parts)
        
        dbplyr::sql(paste0(col, " IN (", concept_subquery, ")"))
    })
    
    filter_exprs
}

# search all non-concept_id columns, return list of filter expressions
sql_search_regular_cols <- function(regular_cols, col_types, regular_cols_values) {
    if (length(regular_cols) == 0) return(list())
    
    filter_exprs <- list()
    
    for (i in seq_along(regular_cols)) {
        col <- regular_cols[[i]]
        search_value <- regular_cols_values[[i]]
        ctype <- col_types[[i]]
        sym_col <- rlang::sym(col)
        
        search_term <- paste0("%", search_value, "%")
        search_value_num <- toNum(search_value)

        if (is.null(search_value) || search_value == "") {
            next
        }

        if (is_date_type(ctype)) {
            date_expr <- build_date_filter_expr(sym_col, search_value, ctype)
            if (!is.null(date_expr)) {
                filter_exprs <- c(filter_exprs, list(date_expr))
                next
            }
        }
        
        if (!is.null(ctype) && ctype %in% c("character", "logical")) {
            # Character columns: use LIKE
            filter_exprs <- c(
                filter_exprs,
                list(rlang::expr(!!sym_col %like% !!search_term))
            )
        } else if (!is.null(ctype) && ctype %in% c("double", "integer", "numeric")) {
            # Numeric columns: Only use numeric equality if column name doesn't suggest it's a date
            if (!is.na(search_value_num)) {
                # Pure numeric column with numeric search: add exact match
                filter_exprs <- c(
                    filter_exprs,
                    list(rlang::expr(!!sym_col == !!search_value_num))
                )
            } else {
                filter_exprs <- c(
                    filter_exprs,
                    list(rlang::expr(as.character(!!sym_col) %like% !!search_term))
                )
            }
        } else {
            # Unknown type (including DATE, TIMESTAMP): cast to TEXT and use LIKE
            filter_exprs <- c(
                filter_exprs,
                list(rlang::expr(as.character(!!sym_col) %like% !!search_term))
            )
        }
    }
    
    filter_exprs
}

# query <- con$person
sql_search <- function(con, query, table_name, tbl_search_cols, search_values, relation = "OR", table_info = NULL) {
    if (is.null(search_values)) search_values <- ""
    if (length(tbl_search_cols) == 0) return(query)

    search_values <- rep(search_values, length.out = length(tbl_search_cols))
    keep <- !(is.na(search_values)) & nzchar(search_values)
    tbl_search_cols <- tbl_search_cols[keep]
    search_values <- search_values[keep]
    if (length(tbl_search_cols) == 0) return(query)

    if (is.null(table_info)) {
        tbl_col_types <- sql_date_types(con[[table_name]])
        concept_id_cols <- omop_concept_id_columns[[table_name]]
    } else {
        tbl_col_types <- table_info$column_types
        concept_id_cols <- table_info$concept_columns
    }

    if (is.null(tbl_col_types)) tbl_col_types <- list()
    if (is.null(concept_id_cols)) concept_id_cols <- character(0)

    available_cols <- intersect(tbl_search_cols, names(tbl_col_types))
    tbl_search_cols <- available_cols
    
    # Separate concept_id columns from regular columns
    concept_cols_to_search <- intersect(tbl_search_cols, concept_id_cols)
    concept_cols_values <- search_values[match(concept_cols_to_search, tbl_search_cols)]

    regular_cols_to_search <- setdiff(tbl_search_cols, concept_id_cols)
    regular_cols_values <- search_values[match(regular_cols_to_search, tbl_search_cols)]

    
    filter_exprs <- list()
    
    # Handle concept_id columns - get list of filter expressions
    if (length(concept_cols_to_search) > 0) {
        concept_exprs <- sql_search_concept_cols(con, concept_cols_to_search, concept_cols_values)
        filter_exprs <- c(filter_exprs, concept_exprs)
    }
    
    # Handle regular columns - get list of filter expressions
    if (length(regular_cols_to_search) > 0) {
        col_types <- tbl_col_types[regular_cols_to_search]
        regular_exprs <- sql_search_regular_cols(regular_cols_to_search, col_types, regular_cols_values)
        filter_exprs <- c(filter_exprs, regular_exprs)
    }
    
    # Combine all expressions with specified relation
    if (length(filter_exprs) > 0) {
        if (relation == "AND") {
            combined_filter <- Reduce(\(x, y) rlang::expr((!!x) & (!!y)), filter_exprs)
        } else {
            combined_filter <- Reduce(\(x, y) rlang::expr((!!x) | (!!y)), filter_exprs)
        }
        query |> filter(!!combined_filter)
    } else {
        query
    }
}


pack_meta_info <- function(table_name, person_id, row_id) {
    paste0(table_name, "|", person_id, "|", row_id)
}

unpack_meta_info <- function(meta_string) {
    parts <- strsplit(meta_string, "|", fixed = TRUE)[[1]]
    list(
        table_name = parts[1],
        person_id = as.numeric(parts[2]),
        row_id = as.numeric(parts[3])
    )
}

concept_id_to_concept_name <- function(query, con, table_name, tbl_all_cols) {
    omop_concept_col <- omop_concept_id_columns[[table_name]]
    omop_source_map <- omop_concept_id_source_value_map[[table_name]]
    # Use scalar subqueries instead of LEFT JOIN for better performance
    # This ensures concept lookups only happen AFTER row filtering
    concept_col_list <- intersect(tbl_all_cols, omop_concept_col)
    
    if (length(concept_col_list) == 0) return(query)
    
    concept_tbl <- dbplyr::remote_name(con$concept)
    # Build all mutations using SQL subqueries
    mutation_list <- list()
    for (col in concept_col_list) {
        mapped_source_value <- omop_source_map[[col]]
        
        # Create scalar subquery for concept name lookup
        subquery <- paste0("(SELECT concept_name FROM ", concept_tbl, " WHERE concept_id = ", col, ")")
        
        if (is.null(mapped_source_value)) {
            mutation_list[[col]] <- dbplyr::sql(paste0(
                "CONCAT(", subquery, ", ' (', CAST(", col, " AS VARCHAR), ')')"
            ))
        } else {
            mutation_list[[col]] <- dbplyr::sql(paste0(
                "CASE WHEN (", col, " IS NULL OR ", col, " = 0) ",
                "THEN CONCAT(", mapped_source_value, ", ' (0)') ",
                "ELSE CONCAT(", subquery, ", ' (', CAST(", col, " AS VARCHAR), ')') END"
            ))
        }
    }
    
    query |> mutate(!!!mutation_list)
}
