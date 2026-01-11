
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


concept_id_to_concept_name1 <- function(query, concept_name, dt_col_names) {
    # map concept_id to its name
    concept_col_list <- intersect(dt_col_names, names(concept_id_source_value_map))
    for (col in concept_col_list) {
        mapped_source_value <- concept_id_source_value_map[[col]]
        by <- structure("concept_id", names = col)
        
        # Create a unique alias for this join
        concept_name_alias <- paste0(col, "_concept_name")
        
        query <- query |>
            left_join(
                concept_name |> rename(!!sym(concept_name_alias) := concept_name), 
                by = by
            ) 

        if (is.null(mapped_source_value)){
            query <- query |> 
                mutate(!!sym(col) := paste0(!!sym(concept_name_alias), " (", !!sym(col), ")")) 
        } else {
            query <- query |>
            mutate(!!sym(col) := case_when(
                (is.na(!!sym(col)) | !!sym(col) == 0) ~ paste0(!!sym(mapped_source_value), " (0)"),
                TRUE ~ paste0(!!sym(concept_name_alias), " (", !!sym(col), ")")
            ))
        }
        query <- query |> select(-!!sym(concept_name_alias))
    }
    query
}


concept_id_to_concept_name2 <- function(query, concept_name, dt_col_names) {
    # map concept_id to its name
    concept_col_list <- intersect(dt_col_names, names(concept_id_source_value_map))
    
    if (length(concept_col_list) == 0) return(query)
    
    # Phase 1: Do all joins at once
    for (col in concept_col_list) {
        by <- structure("concept_id", names = col)
        concept_name_alias <- paste0(col, "_concept_name")
        
        query <- query |>
            left_join(
                concept_name |> rename(!!sym(concept_name_alias) := concept_name), 
                by = by
            )
    }
    
    # Phase 2: Do all mutations at once in a single mutate call
    mutation_list <- list()
    cols_to_drop <- c()
    
    for (col in concept_col_list) {
        mapped_source_value <- concept_id_source_value_map[[col]]
        concept_name_alias <- paste0(col, "_concept_name")
        cols_to_drop <- c(cols_to_drop, concept_name_alias)
        
        if (is.null(mapped_source_value)){
            mutation_list[[col]] <- rlang::expr(paste0(!!sym(concept_name_alias), " (", !!sym(col), ")"))
        } else {
            mutation_list[[col]] <- rlang::expr(case_when(
                (is.na(!!sym(col)) | !!sym(col) == 0) ~ paste0(!!sym(mapped_source_value), " (0)"),
                TRUE ~ paste0(!!sym(concept_name_alias), " (", !!sym(col), ")")
            ))
        }
    }
    
    query <- query |> mutate(!!!mutation_list)
    query <- query |> select(-all_of(cols_to_drop))
    
    query
}