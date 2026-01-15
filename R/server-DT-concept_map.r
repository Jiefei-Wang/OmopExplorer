
sql_date_types <- function(tbl) {
    lapply(as.data.frame(head(tbl, 0)), function(x) class(x)[1])
}

get_mapped_concept_id_col <- function(col_name) {
    paste0("shiny_", col_name)
}

strip_mapped_concept_id_col <- function(col_name) {
    sub("^shiny_", "", col_name)
}

concept_id_to_concept_name <- function(query, con, table_name, concept_columns) {
    omop_source_map <- omop_concept_id_source_value_map[[table_name]]

    if (length(concept_columns) == 0) return(query)

    concept_tbl <- NULL
    if (!is.null(con$concept)) {
        concept_tbl <- dbplyr::remote_name(con$concept)
    } else if (!is.null(con$dbcon) && "concept" %in% DBI::dbListTables(con$dbcon)) {
        concept_tbl <- dbplyr::remote_name(dplyr::tbl(con$dbcon, "concept"))
    } else {
        stop("concept table is not available in the connection.")
    }

    mutation_list <- list()
    for (col in concept_columns) {
        target_col <- get_mapped_concept_id_col(col)
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
