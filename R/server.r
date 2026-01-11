

browser_server <- function(input, output, session, con) {
    params <- list()
    params$global_search_value <- debounce(
        reactive({
            print("update")
            input$sidebar_table_filter
        }),
        millis = 2000  # 2 second delay
    )

    params$visible_table_name <- reactiveVal("person")
    
    # Store persistent column search values by column name (not table-specific)
    params$column_search_values <- reactiveValues()
    
    # Print column search values when they change
    observe({
        values_list <- reactiveValuesToList(params$column_search_values)
        print("Column search values changed:")
        print(values_list)
    })
    
    # Update visible_table_name when tab changes
    observeEvent(input$main_tabs, {
        params$visible_table_name(input$main_tabs)
        print(paste("Visible table:", input$main_tabs))
    })

    target_person_id <- reactiveVal(NA)
    params$target_person_id <- target_person_id
    # Update UI filter when table row is clicked
    observeEvent(input$tbl_person_id, {
        print(input$tbl_person_id)
        meta_dt <- unpack_meta_info(input$tbl_person_id)
        target_person_id(toNum(meta_dt$person_id))
    })
    
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




