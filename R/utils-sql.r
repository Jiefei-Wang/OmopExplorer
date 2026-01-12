
sql_date_types <- function(tbl) {
    lapply(as.data.frame(head(tbl, 1)), function(x) class(x)[1])
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
