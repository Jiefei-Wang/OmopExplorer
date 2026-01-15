render_db_DT <- function(
    params,
    con,
    table_name, 
    post_process_pipe = \(x) x,
    additional_options = list(), 
    additional_callback = NULL,
    ...){

    current_table_info <- params$table_info[[table_name]]
    
    if (is.null(current_table_info) || length(current_table_info$show_columns) == 0) {
        empty_df <- data.frame(message = sprintf("Table %s is not available or has no displayable columns.", table_name))
        return(renderDT(
            DT::datatable(empty_df, options = list(dom = "t"), rownames = FALSE)
        ))
    }

    show_columns <- current_table_info$show_columns
    search_values <- params$sidebar_search_values()
    if (table_name != "person") {
        search_values[['person_id']] <- params$target_person_id()
    }
    # search_values <- list(person_id = "<10")
    # get relevant search parameters
    params_search <- search_deparser(
        con,
        params$table_info,
        table_name,
        search_values
    )

    init_DT_cache(table_name)
    if(is_DT_render_cached(table_name, params_search)){
        flog.debug(glue::glue("Using cached DT render for table {table_name}"))
        return(get_DT_render_from_cache(table_name))
    }

    flog.debug(glue::glue("Rendering DT for table {table_name} with search params: {paste(names(params_search), params_search, sep='=', collapse=', ')}"))
    
    callback <- "
        table.on('dblclick.dt', 'tbody tr', function () {
            console.log('Row double clicked');
            var row = table.row(this);
            var data = row.data();
            console.log(data[1]);
            Shiny.setInputValue('tbl_dblclick_meta', data[1], {priority: 'event'});
        });
    "

    if (!is.null(additional_callback)) {
        callback <- paste(callback, additional_callback, sep = "\n")
    }
    callback <- DT::JS(callback)


    options <- list(
        serverSide = TRUE,
        processing = TRUE, 
        columnDefs = list(
            list(visible=FALSE, targets = "shiny_meta")
            ),
        dom = "l<'search'>rtip"
    )
    options <- c(options, additional_options)
    
    all_columns <- c("shiny_meta", show_columns)
    dummy <- rep(list(1), length(all_columns))
    names(dummy) <- all_columns
    dummy <- as.data.frame(dummy)

    dt_render <- renderDT(
        expr = dummy,
        server = TRUE,
        options = options,
        funcFilter = sql_dt_filter(
            table_info = params$table_info,
            con = con,
            table_name = table_name,
            post_process_pipe = post_process_pipe,
            params_search = params_search
        ),
        selection = 'single',
        callback = callback,
        ...
    )
    set_DT_render_cache(table_name, dt_render, params_search)
    dt_render
}




sql_dt_filter <- function(table_info, con, table_name, post_process_pipe,  params_search) {
    function(data, params_dt) {
        current_table_info <- table_info[[table_name]]
        show_columns <- current_table_info$show_columns
        tbl_all_cols <- current_table_info$columns
        key_column <- current_table_info$key_column

        params_start <- as.integer(params_dt$start)
        params_len <- as.integer(params_dt$length)

        ## TODO:support custom columns
        params_order <- list()
        for (i in seq_along(params_dt$order)) {
            col_idx <- as.integer(params_dt$order[[i]]$column) + 1
            col_db_name <- params_dt$columns[[col_idx]]$name
            ascending <- (params_dt$order[[i]]$dir == "asc")
            params_order[[col_db_name]] <- ascending
        }
        # key column is always added as the last order criterion for stable paging
        if (!is.null(key_column) && !(key_column %in% names(params_order))) {
            params_order[[key_column]] <- TRUE
        }

        cache_available <- is_DT_data_cache_available(
            table_name,
            params_search,
            params_order,
            params_start,
            params_len
        )
        # build cache if not available
        if (!cache_available) {
            flog.debug("Building new DT cache")
            cache_start_idx <- (floor(params_start / cache_row_count)) * cache_row_count
            cache_len <- max(cache_row_count, params_start + params_len - cache_start_idx)
            query <- con[[table_name]]
            concept_columns <- current_table_info$concept_columns

            query <- concept_id_to_concept_name(
                query = query,
                con = con,
                table_name = table_name,
                concept_columns = concept_columns)

            
            # For the concept id column, we need to use its mapped concept name column for diaplay
            mapped_show_columns <- show_columns
            mapped_columns_idx <- which(mapped_show_columns %in% concept_columns)
            if (length(mapped_columns_idx) > 0){
                mapped_show_columns[mapped_columns_idx] <- get_mapped_concept_id_col(mapped_show_columns[mapped_columns_idx])
            }

            result <- filter_data(
                query = query,
                show_columns = mapped_show_columns,
                params_search = params_search,
                params_order = params_order,
                row_start = cache_start_idx,
                row_length = cache_len
            ) 
            data_out <- result$data
            recordsFiltered <- result$recordsFiltered
            
            # turn the mapped concept name columns back to concept id columns
            idx <- which(colnames(data_out) %in% get_mapped_concept_id_col(concept_columns))
            if (length(idx) > 0){
                cn <- colnames(data_out)
                cn[idx] <- strip_mapped_concept_id_col(cn[idx])
                colnames(data_out) <- cn
            }

            # transform to the DT output data
            key_column <- current_table_info$key_column
            person_col <- if ("person_id" %in% colnames(data_out)) "person_id" else NULL
            meta_data <- c()
            for(i in seq_len(nrow(data_out))){
                row_id <- data_out[[key_column]][i]
                person_id <- if (!is.null(person_col)) data_out[[person_col]][i] else NA
                meta <- pack_meta_info(table_name, person_id, row_id)
                meta_data <- c(meta_data,meta)
            }
            data_out <- cbind(shiny_meta = meta_data, data_out)
            row_indices <- as.character(seq(params_start + 1, length.out = nrow(data_out)))
            final_df <- cbind(" " = row_indices, data_out)

            final_matrix <- as.matrix(final_df)
            dimnames(final_matrix) <- NULL
            
            set_cached_DT(
                table_name,
                cache_start_idx,
                params_search,
                params_order,
                recordsFiltered,
                final_matrix
            )
        }

        dataDT <- get_cached_DT(
                table_name,
                params_start,
                params_len
            )

        flog.debug(glue::glue("DT: start={params_start}, len={params_len},  filtered={dataDT$recordsFiltered}"))

        return(list(
            draw = as.integer(params_dt$draw),
            recordsTotal = dataDT$recordsFiltered,
            recordsFiltered = dataDT$recordsFiltered,
            data = dataDT$cached_data,
            DT_rows_current = seq(params_start + 1, length.out = nrow(dataDT$cached_data))
        ))
    }
}