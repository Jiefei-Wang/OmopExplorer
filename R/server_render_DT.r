sql_dt_filter <- function(con, table_name, post_process_pipe, table_info, show_columns, params_search) {
    function(data, params_dt) {
        tbl_all_cols <- table_info$columns

        params_start <- as.integer(params_dt$start)
        params_len <- as.integer(params_dt$length)

        ## TODO:support custom columns
        params_order <- list()
        for (i in seq_along(params_dt$order)) {
            col_idx <- as.integer(params_dt$order[[i]]$column) + 1
            col_db_name <- params_dt$columns[[col_idx]]$name
            ascending <- (params_dt$order[[i]]$dir == "asc")
            params_order[[i]] <- list(
                column = col_db_name,
                ascending = ascending
            )
        }

        cache_available <- is_DT_cache_available(
            table_name,
            params_search,
            params_order,
            params_start,
            params_len
        )
        # build cache if not available
        if (!cache_available) {
            flog.debug("Building new DT cache")
            data_cache_DT <- build_cache_DT(
                con, table_info, table_name, post_process_pipe, show_columns, params_search, params_order, params_start, params_len) 
            set_cached_DT(
                table_name,
                data_cache_DT$start,
                params_search,
                params_order,
                data_cache_DT$recordsTotal,
                data_cache_DT$recordsFiltered,
                data_cache_DT$cached_data
            )
        }

        dataDT <- get_cached_DT(
                table_name,
                params_start,
                params_len
            )

        flog.debug(glue::glue("DT: start={params_start}, len={params_len}, total={dataDT$recordsTotal}, filtered={dataDT$recordsFiltered}"))

        return(list(
            draw = as.integer(params_dt$draw),
            recordsTotal = dataDT$recordsTotal,
            recordsFiltered = dataDT$recordsFiltered,
            data = dataDT$cached_data,
            DT_rows_current = seq(params_start + 1, length.out = nrow(dataDT$cached_data))
        ))
    }
}

keep_exist_cols <- function(table_info, table_name, cols){
    if (table_name %in% names(table_info)){
        available_cols <- table_info[[table_name]]$columns
        intersect(cols, available_cols)
    } else {
        cols
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
    table_info <- params$table_info[[table_name]]
    show_columns <- keep_exist_cols(table_info, table_name, omop_show_columns[[table_name]])
    
    if (is.null(table_info) || length(show_columns) == 0) {
        empty_df <- data.frame(message = sprintf("Table %s is not available or has no displayable columns.", table_name))
        return(renderDT(
            DT::datatable(empty_df, options = list(dom = "t"), rownames = FALSE)
        ))
    }

    # get relevant search parameters
    params_search <- get_relavent_search_values(
        params$sidebar_search_values(),
        params$table_info,
        table_name
    )

    if (table_name != "person" ) {
        if (!params$target_person_id() %in% c(NA, NULL)){
            person_id_value <- params$target_person_id()
            params_search[['person_id']] <- person_id_value
        }
    }


    table_cache <- global_DT_space[[table_name]]
    if (is.null(table_cache)){
        table_cache <- list(
            params_search = list(),
            dt_render = NULL
        )
        global_DT_space[[table_name]] <- table_cache
    } else {
        # check if search params changed
        if (identical(table_cache$params_search, params_search)){
            # stop here, no need to re-render
            return(table_cache$dt_render)
        } else{
            table_cache$params_search <- params_search
        }
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
            con = con,
            table_name = table_name,
            post_process_pipe = post_process_pipe,
            table_info = table_info,
            show_columns = show_columns,
            params_search = params_search
        ),
        selection = 'single',
        callback = callback,
        ...
    )

    table_cache$dt_render <- dt_render
    global_DT_space[[table_name]] <- table_cache
    dt_render
}
