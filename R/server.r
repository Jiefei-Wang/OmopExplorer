
make_server_person_DT <- function(input, output, session, con, params){
    select_pipi <- \(x) x|> 
        mutate(birth_date = as.Date(birth_datetime))
        
    observe({
        output$person_DT <- render_db_DT(
            params = params,
            con = con,
            table_name = "person",
            filter_person_id = FALSE,
            post_process_pipe = select_pipi,
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



make_server_condition_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        select(person_id, condition_occurrence_id, condition_concept_id, condition_start_date, condition_end_date)

    observe({
        table <- con$condition_occurrence
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$condition_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



make_server_visit_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        select(person_id, visit_occurrence_id, visit_concept_id, visit_start_date, visit_end_date)
    observe({
        table <- con$visit_occurrence
        print(params$target_person_id())
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$visit_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



make_server_procedure_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        select(person_id, procedure_occurrence_id, procedure_concept_id, procedure_date)
    observe({
        table <- con$procedure_occurrence
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$procedure_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



make_server_measurement_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        select(person_id, measurement_id, measurement_date, measurement_concept_id, value_as_number, unit_concept_id, value_as_concept_id)
    observe({
        table <- con$measurement
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$measurement_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



make_server_drug_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        select(person_id, drug_exposure_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, quantity)
    observe({
        table <- con$drug_exposure
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$drug_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



make_server_note_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        mutate(note_text_preview = substr(note_text, 1, 100)) |>
        select(person_id, note_id, note_date, note_type_concept_id, note_title, note_text_preview)
    observe({
        table <- con$note
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$note_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



make_server_death_DT <- function(input, output, session, con, params){
    select_pipe <- \(x) x |>
        select(person_id, death_date, death_type_concept_id, cause_concept_id)
    observe({
        table <- con$death
        if (!is.na(params$target_person_id())) {
            table <- table |>
                filter(person_id == params$target_person_id())
        }
        output$death_DT <- 
        render_db_DT(table,
            concept_name = con$concept_name,
            post_process_pipe = select_pipe)
    })
}



browser_server <- function(input, output, session, con) {
    params <- list()
    # Use reactiveVal to store the selected person ID
    params$start_date <- reactive({
        as.Date(input$sidebar_date_range_filter[1])
    })
    params$end_date <- reactive({
        as.Date(input$sidebar_date_range_filter[2])
    })
    
    # Debug: Print when date range changes
    observeEvent(params$start_date(), {
        print(paste("Start date:", params$start_date()))
        print(paste("End date:", params$end_date()))
    })

    
    con$concept_name <- con$concept |> 
        select(concept_id, concept_name) 

    target_person_id <- reactiveVal(NA)
    params$target_person_id <- target_person_id
    # Update UI filter when table row is clicked
    observeEvent(input$tbl_person_id, {
        target_person_id(toNum(input$tbl_person_id))
    })
    
    # Update param when UI filter changes
    observeEvent(input$sidebar_person_id_filter, {
        target_person_id(toNum(input$sidebar_person_id_filter))
    })

    make_server_person_DT(input, output, session, con, params)
    # make_server_visit_DT(input, output, session, con, params)
    # make_server_condition_DT(input, output, session, con, params)
    # make_server_procedure_DT(input, output, session, con, params)
    # make_server_measurement_DT(input, output, session, con, params)
    # make_server_drug_DT(input, output, session, con, params)
    # make_server_note_DT(input, output, session, con, params)
    # make_server_death_DT(input, output, session, con, params)
    
    person_details <- eventReactive(input$tbl_dblclick_meta, {
        req(input$tbl_dblclick_meta)
        meta_dt <- unpack_meta_info(input$tbl_dblclick_meta)
        print(meta_dt)

        # Replace with your dbplyr query:
        # person_detail_tbl() should return a tbl() or data.frame
        detail <- con[[meta_dt$table_name]] |> 
            filter(person_id == meta_dt$person_id) |>
            collect()

        detail
    })

    observeEvent(person_details(), {
        showModal(modalDialog(
            title = paste("Person", person_details()$person_id),
            DTOutput("person_detail_DT"),
            easyClose = TRUE,
            size = "l"
        ))
    })

    output$person_detail_DT <- renderDT({
        req(person_details())
        datatable(person_details(), rownames = FALSE, options = list(dom = "t"))
    })
    
}