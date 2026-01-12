
build_table_info <- function(con) {
    available_tables <- setdiff(names(con), "dbcon")
    table_info <- list()

    for (table_name in available_tables) {
        tbl <- con[[table_name]]
        table_columns <- tryCatch(colnames(tbl), error = function(...) character(0))
        column_types <- if (length(table_columns) > 0) sql_date_types(tbl) else list()

        desired_show_columns <- omop_show_columns[[table_name]]
        if (is.null(desired_show_columns)) {
            desired_show_columns <- table_columns
        }
        show_columns <- intersect(desired_show_columns, union(table_columns, desired_show_columns))

        concept_columns <- intersect(
            if (is.null(omop_concept_id_columns[[table_name]])) character(0) else omop_concept_id_columns[[table_name]],
            table_columns
        )

        key_column <- omop_key_columns[[table_name]]
        if (!is.null(key_column) && !(key_column %in% table_columns)) {
            stop(glue::glue("Key column '{key_column}' for table '{table_name}' not found in table columns"))
        }

        table_info[[table_name]] <- list(
            table = tbl,
            columns = table_columns,
            column_types = column_types,
            show_columns = show_columns,
            concept_columns = concept_columns,
            key_column = key_column
        )
    }

    table_info
}


browser_server <- function(input, output, session, con) {
    params <- new.env()

    params$table_info <- build_table_info(con)

    default_table <- if ("person" %in% names(params$table_info)) {
        "person"
    } else if (length(params$table_info) > 0) {
        names(params$table_info)[1]
    } else {
        "person"
    }

    params$displayed_table_name <- reactiveVal(default_table)

    # Keep the original reactiveValues for immediate updates
    params$sidebar_search_values_internal <- reactiveValues()
    # Delay updates using debounce
    params$sidebar_search_values <- debounce(
        reactive({
            reactiveValuesToList(params$sidebar_search_values_internal)
        }),
        millis = sidebar_debounce_millis
    )

    # Update displayed_table_name when tab changes
    observeEvent(input$main_tabs, {
        params$displayed_table_name(input$main_tabs)
    })

    target_person_id <- reactiveVal(NA)
    params$target_person_id <- target_person_id
    # Update UI filter when table row is clicked
    observeEvent(input$tbl_person_id, {
        print(input$tbl_person_id)
        meta_dt <- unpack_meta_info(input$tbl_person_id)
        target_person_id(toNum(meta_dt$person_id))
    })

    # update global search value when sidebar input changes
    params$global_search_value <- debounce(
        reactive({
            input$sidebar_search_anything
        }),
        millis = sidebar_debounce_millis
    )
    
    register_server_modal(input, output, session, con, params)
    register_server_sidebar(input, output, session, con, params)
    

    register_server_person_DT(input, output, session, con, params)
    register_server_visit_DT(input, output, session, con, params)
    register_server_condition_DT(input, output, session, con, params)
    register_server_procedure_DT(input, output, session, con, params)
    register_server_measurement_DT(input, output, session, con, params)
    register_server_drug_DT(input, output, session, con, params)
    register_server_note_DT(input, output, session, con, params)
    register_server_death_DT(input, output, session, con, params)
    register_server_provider_DT(input, output, session, con, params)
    register_server_care_site_DT(input, output, session, con, params)
    
}




