register_server_sidebar <- function(input, output, session, con, params){
    observe({
        # Update date range filter based on data
        date_range <- con$person |>
            summarise(
                min_date = min(birth_datetime, na.rm = TRUE),
                max_date = max(birth_datetime, na.rm = TRUE)
            ) |>
            collect()
        
        updateDateRangeInput(
            session,
            "sidebar_date_range_filter",
            start = as.Date(date_range$min_date),
            end = as.Date(date_range$max_date),
            min = as.Date(date_range$min_date),
            max = as.Date(date_range$max_date)
        )
    })

    
    # Dynamically render column search boxes based on visible table
    output$sidebar_column_filters <- renderUI({
        table_name <- params$visible_table_name()
        
        # Get columns for the visible table
        tbl_cols <- colnames(con[[table_name]])
        
        # Create search input for each column with persisted values
        column_inputs <- lapply(tbl_cols, function(col) {
            input_id <- paste0("col_search_", col)
            
            # Get stored value for this column (if any)
            stored_value <- isolate(params$column_search_values[[col]])
            if (is.null(stored_value)) stored_value <- ""
            
            tagList(
                textInput(
                    inputId = input_id,
                    label = col,
                    value = stored_value,
                    placeholder = paste("Search", col)
                )
            )
        })
        
        tagList(
            column_inputs
        )
    })
    
    # Observe all column search inputs and store their values with debounce
    observe({
        table_name <- params$visible_table_name()
        tbl_cols <- colnames(con[[table_name]])
        
        lapply(tbl_cols, function(col) {
            input_id <- paste0("col_search_", col)
            
            # Create debounced reactive for this input
            debounced_input <- debounce(
                reactive({ input[[input_id]] }),
                millis = 2000
            )
            
            # Observe the debounced value
            observeEvent(debounced_input(), {
                params$column_search_values[[col]] <- debounced_input()
            }, ignoreInit = TRUE, ignoreNULL = FALSE)
        })
    })
}

