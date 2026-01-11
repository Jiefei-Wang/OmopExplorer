
sql_date_types <- function(tbl) {
    lapply(as.data.frame(head(tbl, 1)), typeof)
}


sql_search <- function(tbl, cols, term){
    col_types <- sql_date_types(tbl)
    search_term <- paste0("%", term, "%")
    term_num <- toNum(term)
    like_exprs <- lapply(cols, function(col) {
        sym_col <- rlang::sym(col)
        ctype <- col_types[[col]]
        if (!is.null(ctype) && ctype %in% c("character", "logical")) {
            rlang::expr(!!sym_col %like% !!search_term)
        } else if (!is.null(ctype) && ctype %in% c("double", "integer", "numeric") && !is.na(term_num)) {
            rlang::expr(!!sym_col == !!term_num)
        } else {
            rlang::expr(as.character(!!sym_col) %like% !!search_term)
        }
    })

    combined_like <- Reduce(\(x, y) rlang::expr((!!x) | (!!y)), like_exprs)
    tbl |> filter(!!combined_like)
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

concept_id_to_concept_name <- function(query, concept_name_tbl, dt_col_names) {
    # Use scalar subqueries instead of LEFT JOIN for better performance
    # This ensures concept lookups only happen AFTER row filtering
    concept_col_list <- intersect(dt_col_names, names(concept_id_source_value_map))
    
    if (length(concept_col_list) == 0) return(query)
    
    # Build all mutations using SQL subqueries
    mutation_list <- list()
    
    for (col in concept_col_list) {
        mapped_source_value <- concept_id_source_value_map[[col]]
        
        # Create scalar subquery for concept name lookup
        subquery <- paste0("(SELECT concept_name FROM concept WHERE concept_id = ", col, ")")
        
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