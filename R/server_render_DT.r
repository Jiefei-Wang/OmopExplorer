sql_dt_filter <- function(remote_tbl, concept_name, post_process_pipe, table_name) {
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
            search_cols <- vapply(params$columns[searchable_indices], function(x) x$name, character(1))
            search_cols <- search_cols[!is.na(search_cols)&search_cols%in%dt_col_names]
            query <- sql_search(query, search_cols, global_search)
        }

        # 5. Column-specific Search
        for (i in seq_along(params$columns)) {
            col_search <- params$columns[[i]]$search$value
            col_db_name <- params$columns[[i]]$name
            if (col_search != "" && col_db_name %in% dt_col_names) {
                query <- sql_search(query, col_db_name, col_search)
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

        # map concept_id to its name
        concept_col_list <- intersect(dt_col_names, names(concept_id_source_value_map))
        for (col in concept_col_list) {
            mapped_source_value <- concept_id_source_value_map[[col]]
            by <- structure("concept_id", names = col)
            query <- query |>
                left_join(concept_name, by = by) 

            if (is.null(mapped_source_value)){
                query <- query |> 
                    mutate(!!sym(col) := paste0(!!sym(col), ": ", concept_name)) 
            } else {
                query <- query |>
                mutate(!!sym(col) := case_when(
                    (is.na(!!sym(col)) | !!sym(col) == 0) ~ !!sym(mapped_source_value),
                    TRUE ~ paste0(!!sym(col), ": ", concept_name)
                ))
            }
            query <- query |> select(-concept_name)
        }

        # additional processing and select variables
        show_columns <- omop_show_columns[[table_name]]
        query <- post_process_pipe(query)|>
        select(all_of(show_columns))

        data_out <- collect(query)

        # add meta data
        key_column <- omop_key_columns[[table_name]]
        meta_data <- c()
        for(i in seq_len(nrow(data_out))){
            row_id <- data_out[[key_column]][i]
            person_id <- data_out[['person_id']][i]
            meta <- pack_meta_info(table_name, person_id, row_id)
            meta_data <- c(meta_data,meta)
        }
        data_out <- cbind(shiny_meta = meta_data, data_out)

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


render_db_DT <- function(
    params,
    con,
    table_name, 
    filter_person_id = TRUE,
    post_process_pipe = \(x) x,
    additional_options = list(), 
    additional_callback = NULL,
    ...){
    
    # tables
    table <- con[[table_name]]
    concept_name <- con[['concept']]|>
        select(concept_id, concept_name)


    # whether the table should be filtered by person_id
    if (filter_person_id) {
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
    }

    # handle double click callback
    callback <- glue("
        table.on('dblclick.dt', 'tbody tr', function () {{
            console.log('Row double clicked');
            var row = table.row(this);
            var data = row.data();
            console.log(data[1]);
            Shiny.setInputValue('tbl_dblclick_meta', data[1], {{priority: 'event'}});
        }});
    ")

    if (!is.null(additional_callback)) {
        callback <- paste(callback, additional_callback, sep = "\n")
    }
    callback <- DT::JS(callback)


    # Default options
    options <- list(
        serverSide = TRUE,
        processing = TRUE,
        columnDefs = list(
            list(visible=FALSE, targets = "shiny_meta")
            )
    )
    options <- c(options, additional_options)
    
    show_columns <- omop_show_columns[[table_name]]
    all_columns <- c("shiny_meta", show_columns)
    dummy <- rep(list(1), length(all_columns))
    names(dummy) <- all_columns
    dummy <- as.data.frame(dummy)
    renderDT(
        expr = dummy,
        server = TRUE,
        options = options,
        funcFilter = sql_dt_filter(table, concept_name, post_process_pipe, table_name),
        selection = 'single',
        callback = callback,
        ...
    )
}
