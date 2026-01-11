
register_server_person_DT <- function(input, output, session, con, params){
    observe({
        output$person_DT <- render_db_DT(
            params = params,
            con = con,
            table_name = "person",
            filter_person_id = FALSE,
            additional_callback  = "
                var person_selectedRow = null;
                table.on('click.dt', 'tbody tr', function () {
                var row = table.row(this);

                if (person_selectedRow === this) {
                    // unselect
                    $(this).removeClass('selected');
                    person_selectedRow = null;
                    Shiny.setInputValue('tbl_person_id', '', {priority: 'event'});
                } else {
                    // select new row
                    if (person_selectedRow !== null) {
                    $(person_selectedRow).removeClass('selected');
                    }
                    $(this).addClass('selected');
                    person_selectedRow = this;

                    var data = row.data();
                    // person_id is column index 1
                    Shiny.setInputValue('tbl_person_id', data[1], {priority: 'event'});
                }
                });
            "
        )
    })
    
}




register_server_visit_DT <- function(input, output, session, con, params){
    observe({
        output$visit_occurrence_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "visit_occurrence",
            filter_person_id = TRUE)
    })
}



register_server_condition_DT <- function(input, output, session, con, params){
    observe({
        output$condition_occurrence_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "condition_occurrence",
            filter_person_id = TRUE)
    })
}



register_server_procedure_DT <- function(input, output, session, con, params){
    observe({
        output$procedure_occurrence_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "procedure_occurrence",
            filter_person_id = TRUE)
    })
}



register_server_measurement_DT <- function(input, output, session, con, params){
    observe({
        output$measurement_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "measurement",
            filter_person_id = TRUE)
    })
}



register_server_drug_DT <- function(input, output, session, con, params){
    observe({
        output$drug_exposure_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "drug_exposure",
            filter_person_id = TRUE)
    })
}



register_server_note_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        mutate(note_text_preview = substr(note_text, 1, 100)) 
    observe({
        output$note_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "note",
            filter_person_id = TRUE,
            post_process_pipe = select_pipe)
    })
}



register_server_death_DT <- function(input, output, session, con, params){
    observe({
        output$death_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "death",
            filter_person_id = TRUE)
    })
}

register_server_provider_DT <- function(input, output, session, con, params){
    observe({
        output$provider_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "provider",
            filter_person_id = FALSE)
    })
}

register_server_care_site_DT <- function(input, output, session, con, params){
    observe({
        output$care_site_DT <- 
        render_db_DT(
            params = params,
            con = con,
            table_name = "care_site",
            filter_person_id = FALSE)
    })
}

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
}

browser_server <- function(input, output, session, con) {
    params <- list()
    params$global_search_value <- debounce(
        reactive({
            print("update")
            input$sidebar_table_filter
        }),
        millis = 2000  # 1 second delay
    )

    params$visible_table_name <- reactiveVal("person")
    
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
    
    register_server_modal(input, output, session, con, params)
}




