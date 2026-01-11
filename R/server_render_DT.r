resolve_show_columns <- function(table_info, post_process_pipe) {
    if (is.null(table_info)) return(character(0))
    candidate_show <- table_info$show_columns
    if (length(candidate_show) == 0) {
        candidate_show <- table_info$columns
    }

    preview_cols <- tryCatch({
        preview <- table_info$table |> head(0)
        colnames(post_process_pipe(preview))
    }, error = function(...) NULL)

    available_cols <- if (is.null(preview_cols)) table_info$columns else preview_cols
    intersect(candidate_show, available_cols)
}

sql_dt_filter <- function(con, table_name, post_process_pipe, table_info, show_columns, global_search_value, sidebar_filters) {
    function(data, params_dt) {
        if (is.null(table_info) || length(show_columns) == 0) {
            return(list(
                draw = as.integer(params_dt$draw),
                recordsTotal = 0,
                recordsFiltered = 0,
                data = matrix(nrow = 0, ncol = length(show_columns) + 2),
                DT_rows_current = integer(0)
            ))
        }

        tbl <- table_info$table
        tbl_all_cols <- table_info$columns

        records_total <- if (length(tbl_all_cols) > 0) {
            as.numeric(tally(tbl) |> pull(1))
        } else {
            0
        }

        query <- tbl |>
            mutate(shiny_row_idx = row_number())
        

        # search from the global filter
        if (!is.null(global_search_value) && global_search_value != "") {
            query <- sql_search(con, query, table_name, tbl_all_cols, global_search_value, table_info = table_info)
        }

        # for each individual column filter
        if (length(sidebar_filters) > 0) {
            query <- sql_search(
                con,
                query,
                table_name,
                names(sidebar_filters),
                unlist(sidebar_filters),
                relation = "AND",
                table_info = table_info
            )
        }

        if (length(params_dt$order) > 0) {
            sort_exprs <- list()
            
            for (i in seq_along(params_dt$order)) {
                col_idx <- as.integer(params_dt$order[[i]]$column) + 1
                col_db_name <- params_dt$columns[[col_idx]]$name
                direction <- params_dt$order[[i]]$dir

                if (col_db_name %in% tbl_all_cols) {
                    if (direction == "asc") {
                        sort_exprs[[length(sort_exprs) + 1]] <- expr(!!sym(col_db_name))
                    } else {
                        sort_exprs[[length(sort_exprs) + 1]] <- expr(desc(!!sym(col_db_name)))
                    }
                }
            }
            
            # Add shiny_row_idx as the last sort criterion
            sort_exprs[[length(sort_exprs) + 1]] <- expr(shiny_row_idx)
            
            # Apply all sort expressions in a single arrange
            if (length(sort_exprs) > 0) {
                query <- query |> arrange(!!!sort_exprs)
            }
        }
        
        query <- query |>
            mutate(shiny_row_row_number = row_number())
            
        start <- as.integer(params_dt$start)
        len <- as.integer(params_dt$length)
        print(glue::glue("DT request: start={start}, len={len}"))
        query <- query |>
            mutate(shiny_total_rows = n()) |>
            filter(shiny_row_row_number > start & shiny_row_row_number <= start + len)
        
        query <- concept_id_to_concept_name(
            query = query, 
            con = con, 
            table_name = table_name, 
            tbl_all_cols = tbl_all_cols)

        query <- post_process_pipe(query)

        query <- query |>
            select(shiny_total_rows, all_of(show_columns))

        data_out <- collect(query)

        records_filtered <- if (nrow(data_out) > 0) {
            data_out$shiny_total_rows[1]
        } else {
            0
        }

        data_out$shiny_total_rows <- NULL

        key_column <- table_info$key_column
        person_col <- if ("person_id" %in% colnames(data_out)) "person_id" else NULL
        meta_data <- c()
        for(i in seq_len(nrow(data_out))){
            row_id <- if (!is.null(key_column) && key_column %in% colnames(data_out)) data_out[[key_column]][i] else NA
            person_id <- if (!is.null(person_col)) data_out[[person_col]][i] else NA
            meta <- pack_meta_info(table_name, person_id, row_id)
            meta_data <- c(meta_data,meta)
        }
        data_out <- cbind(shiny_meta = meta_data, data_out)

        row_indices <- as.character(seq(start + 1, length.out = nrow(data_out)))
        final_df <- cbind(" " = row_indices, data_out)

        final_matrix <- as.matrix(final_df)
        dimnames(final_matrix) <- NULL
        return(list(
            draw = as.integer(params_dt$draw),
            recordsTotal = records_total,
            recordsFiltered = records_filtered,
            data = final_matrix,
            DT_rows_current = seq(start + 1, length.out = nrow(data_out))
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

    global_search_value <- get_global_sidebar_value(
        params,
        table_name
    )
    sidebar_filters <- get_sidebar_column_filters(
        params,
        table_name
    )


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

    renderDT(
        expr = dummy,
        server = TRUE,
        filter = 'top',
        options = options,
        funcFilter = sql_dt_filter(
            con = con,
            table_name = table_name,
            post_process_pipe = post_process_pipe,
            table_info = table_info,
            show_columns = show_columns,
            global_search_value = global_search_value,
            sidebar_filters = sidebar_filters
        ),
        selection = 'single',
        callback = callback,
        ...
    )
}
