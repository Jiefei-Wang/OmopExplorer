
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


concept_id_to_concept_name <- function(query, concept_name, dt_col_names) {
    # map concept_id to its name
    concept_col_list <- intersect(dt_col_names, names(concept_id_source_value_map))
    for (col in concept_col_list) {
        mapped_source_value <- concept_id_source_value_map[[col]]
        by <- structure("concept_id", names = col)
        query <- query |>
            left_join(concept_name, by = by) 

        if (is.null(mapped_source_value)){
            query <- query |> 
                mutate(!!sym(col) := paste0(concept_name, " (", !!sym(col), ")")) 
        } else {
            query <- query |>
            mutate(!!sym(col) := case_when(
                (is.na(!!sym(col)) | !!sym(col) == 0) ~ paste0(!!sym(mapped_source_value), " (0)"),
                TRUE ~ paste0(concept_name, " (", !!sym(col), ")")
            ))
        }
        query <- query |> select(-concept_name)
    }
    query
}