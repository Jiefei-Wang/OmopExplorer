sql_dt_filter <- function(con, table, table_name, post_process_pipe, global_search_value) {
    # for the given table:
    # 1. build search query based on params
    # 2. construct concept name
    # 3. keep only show columns
    # 4. apply post_process_pipe
    function(data, params) {
        # Get the actual table from the reactive without tracking dependency
        tbl <- table
        
        concept_name <- con[['concept']]|>
            select(concept_id, concept_name)
        
        # 1. Column Metadata
        tbl_all_cols <- colnames(tbl)
        omop_col_names <- omop[[table_name]][['column']]
        show_cols <- omop_show_columns[[table_name]]

        # 2. Total records in DB (fast)
        records_total <- as.numeric(tally(tbl) |> pull(1))

        # 3. Build Query
        query <- tbl

        # query <- con$person
        # 4. Global Search (if any)
        # search_value <- "1961"
        search_value <- global_search_value
        if (!is.null(search_value) && search_value != "") {
            query <- sql_search(con, query, table_name, tbl_all_cols, search_value)
        }

        # 5. Column-specific Search
        all_searchs <- list()
        for (i in seq_along(params$columns)) {
            col_search <- params$columns[[i]]$search$value
            col_db_name <- params$columns[[i]]$name
            if (col_search != "" && col_db_name %in% tbl_all_cols) {
                all_searchs <- c(all_searchs, list(c(
                    col_name = col_db_name,
                    search_value = col_search
                )))
            }
        }
        if (length(all_searchs) > 0) {
            all_searchs <- do.call(rbind, all_searchs)
            
            # all_searchs <- data.frame(col_name = c("year_of_birth", "month_of_birth"), search_value = c("1961", "11"))
            query <- sql_search(con, query, table_name, all_searchs$col_name, all_searchs$search_value, relation = "AND")
        }


        # 7. Sorting
        if (length(params$order) > 0) {
            for (i in seq_along(params$order)) {
                col_idx <- as.integer(params$order[[i]]$column) + 1
                # params$columns is 0-indexed, col 0 is often " "
                col_db_name <- params$columns[[col_idx]]$name
                direction <- params$order[[i]]$dir

                if (col_db_name %in% tbl_all_cols) {
                    if (direction == "asc") {
                        query <- query |> arrange(!!sym(col_db_name))
                    } else {
                        query <- query |> arrange(desc(!!sym(col_db_name)))
                    }
                }
            }
        }
        
        # add row index for sorting and paging
        query <- query |>
            mutate(shiny_row_idx = row_number())
            
        # 8. Fetch Slice (True Lazy Load)
        start <- as.integer(params$start)
        len <- as.integer(params$length)

        # Fetch data into R
        print(paste0("Fetching rows start ", start, " len ", len))
        query <- query |>
            mutate(shiny_total_rows = n()) |>
            filter(shiny_row_idx > start & shiny_row_idx <= start + len)
        
        # 6. Filtered count
        # records_filtered <- as.numeric(tally(query) |> pull(1))

        # print("before adding concept names")
        # print(query|>show_query())
        # map concept_id to its name
        # browser()
        query <- concept_id_to_concept_name(
            query = query, 
            con = con, 
            table_name = table_name, 
            tbl_all_cols = tbl_all_cols)
        # print("after adding concept names")
        # print(query|>show_query())

        # additional processing and select variables
        show_columns <- omop_show_columns[[table_name]]
        query <- post_process_pipe(query)|>
        select(shiny_total_rows, all_of(show_columns))

        data_out <- collect(query)

        records_filtered <- if (nrow(data_out) > 0) {
            data_out$shiny_total_rows[1]
        } else {
            0
        }

        data_out$shiny_total_rows <- NULL

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

    # handle double click callback
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


    # Default options
    options <- list(
        serverSide = TRUE,
        processing = TRUE, 
        columnDefs = list(
            list(visible=FALSE, targets = "shiny_meta")
            ),
        dom = "l<'search'>rtip"
    )
    options <- c(options, additional_options)
    
    show_columns <- omop_show_columns[[table_name]]
    all_columns <- c("shiny_meta", show_columns)
    dummy <- rep(list(1), length(all_columns))
    names(dummy) <- all_columns
    dummy <- as.data.frame(dummy)

    global_search_value <- params$global_search_value()
    renderDT(
        expr = dummy,
        server = TRUE,
        filter = 'top',
        options = options,
        funcFilter = sql_dt_filter(con, table, table_name, post_process_pipe, global_search_value),
        selection = 'single',
        callback = callback,
        ...
    )
}
