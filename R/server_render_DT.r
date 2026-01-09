sql_dt_filter <- function(remote_tbl) {
    function(data, params) {
        # Get the actual table from the reactive without tracking dependency
        tbl <- remote_tbl|>
        mutate(shiny_row_idx = row_number())
        
        
        # 1. Column Metadata
        dt_col_names <- colnames(tbl)

        # 2. Total records in DB (fast)
        records_total <- as.numeric(tally(tbl) |> pull(1))

        # 3. Build Query
        query <- tbl

        # 4. Global Search (if any)
        global_search <- params$search$value
        if (!is.null(global_search) && global_search != "") {
            search_term <- paste0("%", global_search, "%")
            # Get only columns marked as searchable
            searchable_indices <- which(vapply(params$columns, function(x) x$searchable == "true", logical(1)))
            # Map to actual DB names (offset by 1 if column 0 is rowname placeholder)
            search_cols <- dt_col_names[searchable_indices - 1]
            search_cols <- search_cols[!is.na(search_cols)]

            query <- query |> filter(if_any(all_of(search_cols), ~ . %like% search_term))
        }

        # 5. Column-specific Search
        for (i in seq_along(params$columns)) {
            col_search <- params$columns[[i]]$search$value
            col_db_name <- params$columns[[i]]$name
            if (col_search != "" && col_db_name %in% dt_col_names) {
                query <- query |> filter(!!sym(col_db_name) %like% !!paste0("%", col_search, "%"))
            }
        }

        # 6. Filtered count
        records_filtered <- as.numeric(tally(query) |> pull(1))

        # 7. Sorting
        if (length(params$order) > 0) {
            for (i in seq_along(params$order)) {
                col_idx <- as.integer(params$order[[i]]$column) + 1
                # params$columns is 0-indexed, col 0 is often " "
                col_db_name <- params$columns[[col_idx]]$name
                direction <- params$order[[i]]$dir

                if (col_db_name %in% dt_col_names) {
                    if (direction == "asc") {
                        query <- query |> arrange(!!sym(col_db_name))
                    } else {
                        query <- query |> arrange(desc(!!sym(col_db_name)))
                    }
                }
            }
        }

        # 8. Fetch Slice (True Lazy Load)
        start <- as.integer(params$start)
        len <- as.integer(params$length)

        # Fetch data into R
        print(paste0("Fetching rows start ", start, " len ", len))
        query <- query |>
            filter(shiny_row_idx > start & shiny_row_idx <= start + len)

        for (col in setdiff(dt_col_names, names(concept_id_source_value_map))) {
            mapped_source_value <- concept_id_source_value_map[[col]]
            by <- structure("concept_id", names = col)

            query <- query |> 
            left_join(concept_name, by = by) |>
            mutate(!!sym(col) := case_when(
                !is.null(mapped_source_value) & (is.na(!!sym(col)) | !!sym(col) == 0) ~ !!sym(mapped_source_value),
                TRUE ~ paste0(!!sym(col), ": ", concept_name)
            )) |>
            select(-concept_name)
        }

        data_out <- collect(query)

        # current_page_data(data_out)

        # Add the Row Names / Index column if column 0 in UI is " "
        # This prevents "Requested unknown parameter '0'"
        row_indices <- as.character(seq(start + 1, length.out = nrow(data_out)))
        final_df <- cbind(" " = row_indices, data_out)

        # Convert to un-named matrix so JSON is [[val, val], [val, val]]
        final_matrix <- as.matrix(final_df)
        dimnames(final_matrix) <- NULL

        return(list(
            draw = as.integer(params$draw),
            recordsTotal = records_total,
            recordsFiltered = records_filtered,
            data = final_matrix,
            # DT_rows_all = seq_len(records_filtered), 
            DT_rows_current = seq(start + 1, length.out = nrow(data_out))
        ))
    }
}


render_db_DT <- function(table, additional_options = list(), ...){
    # Default options
    options <- list(
        serverSide = TRUE,
        processing = TRUE
    )
    options <- c(options, additional_options)
    

    renderDT(
        expr = {
            table |>
            head(1) |>
            collect()
        },
        server = TRUE,
        options = options,
        funcFilter = sql_dt_filter(table),
        selection = 'single',
        ...
    )
}
