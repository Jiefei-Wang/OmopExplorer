sidebar_column_key <- function(column_name) {
    paste(column_name, sep = "::")
}

sidebar_column_input_id <- function(column_name) {
    paste0("sidebar_col_search_", column_name)
}

sidebar_column_clear_id <- function(column_name) {
    paste0("sidebar_clear_col_search_", column_name)
}


get_relavent_search_values <- function(sidebar_search_values, table_info, table_name){
    table_info_current <- table_info[[table_name]]
    available_cols <- "shiny_search_anything"
    if (!is.null(table_info_current)){
        available_cols <- c(available_cols, table_info_current$columns)
    }
    relavent_search_values <- list()
    for (i in available_cols){
        elt <- sidebar_search_values[[i]]
        if (!is.null(elt) && elt != ""){
            relavent_search_values[[i]] <- elt
        }
    }
    relavent_search_values
}



get_sidebar_column_filters <- function(params, table_name) {
    cols <- params$table_info[[table_name]]$columns
    filters <- list()
    for (col in cols) {
        val <- params$sidebar_search_values_internal[[col]]
        if (!is.null(val)) {
            filters[[col]] <- val
        }
    }
    filters
}

get_global_sidebar_value <- function(params, table_name) {
    stored <- params$sidebar_search_values_internal[["shiny_search_anything"]]
    if (is.null(stored)) "" else stored
}


register_server_sidebar <- function(input, output, session, con, params){
    available_tables <- names(params$table_info)
    all_available_columns <- unlist(lapply(available_tables, function(tn) {
        params$table_info[[tn]]$columns
    }))
    all_available_columns <- unique(all_available_columns)

    observeEvent(params$displayed_table_name(), {
        current_table <- params$displayed_table_name()
        # Clear the search input when switching tables
        shinyWidgets::updateSearchInput(session, "sidebar_search_anything", value = "")
        params$sidebar_search_values_internal[["shiny_search_anything"]] <- ""
    }, ignoreInit = FALSE)

    # collect search anything input
    observeEvent(input$sidebar_search_anything, {
        # global search input
        params$sidebar_search_values_internal[["shiny_search_anything"]] <- input$sidebar_search_anything
    })

    observeEvent(input$sidebar_clear_all, {
        current_table <- params$displayed_table_name()
        table_info <- params$table_info[[current_table]]

        shinyWidgets::updateSearchInput(session, "sidebar_search_anything", value = "")
        params$sidebar_search_values_internal[["shiny_search_anything"]] <- ""

        for (col in table_info$columns) {
            input_id <- sidebar_column_input_id(col)
            params$sidebar_search_values_internal[[col]] <- ""
            shinyWidgets::updateSearchInput(session, input_id, value = "")
        }
    }, ignoreInit = TRUE)

    # Dynamically render column search boxes based on visible table
    output$sidebar_column_filters <- renderUI({
        table_name <- params$displayed_table_name()
        table_info <- params$table_info[[table_name]]
        if (is.null(table_info) || length(table_info$columns) == 0) {
            return(div("No filters available for this table."))
        }

        tbl_cols <- table_info$columns

        column_inputs <- lapply(tbl_cols, function(col) {
            input_id <- sidebar_column_input_id(col)
            stored_value <- isolate(params$sidebar_search_values_internal[[col]])
            if (is.null(stored_value)) stored_value <- ""

            div(
                make_search_box(
                    inputId = input_id,
                    label = col,
                    value = stored_value,
                    placeholder = paste("Search", col)
                )
            )
        })

        tagList(column_inputs)
    })

    # Observe all column search inputs and store their values
    for (col in all_available_columns) {
        local({
            col_local <- col
            input_id <- sidebar_column_input_id(col_local)

            observeEvent(input[[input_id]], {
                # print(input_id)
                new_value <- input[[input_id]]
                old_value <- params$sidebar_search_values_internal[[col_local]]
                if (is.null(old_value)) old_value <- ""
                if (is.null(new_value)) new_value <- ""
                
                # Only update if value changed
                if (new_value != old_value) {
                    params$sidebar_search_values_internal[[col_local]] <- new_value
                }
            }, ignoreInit = TRUE, ignoreNULL = FALSE)
        })
    }
}

