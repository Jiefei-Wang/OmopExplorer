
make_server_person_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    reactiveVal(
        con$person |> 
        mutate(birth_date = as.Date(birth_datetime)) |>
        inner_join(concept_name, by = c("gender_concept_id" = "concept_id")) |>
        rename(gender = concept_name)|>
        inner_join(concept_name, by = c("race_concept_id" = "concept_id")) |>
        rename(race = concept_name)|>
        inner_join(concept_name, by = c("ethnicity_concept_id" = "concept_id")) |>
        rename(ethnicity = concept_name)|>
        select(person_id, birth_date, gender, race, ethnicity)
    ) -> person_table
    
    observe({
        output$person_DT <- render_db_DT(person_table(),
            callback = DT::JS("
            table.on('click.dt', 'tbody tr', function() {
                var data = table.row(this).data();
                console.log(data);
                // person_id is the second column in df
                Shiny.setInputValue('tbl_person_id', data[1], {priority: 'event'});
            });
            ")
        )
    })
    
}



make_server_condition_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    condition_table <- reactiveVal({
        dt <- con$condition_occurrence |> 
            inner_join(concept_name, by = c("condition_concept_id" = "concept_id")) |>
            rename(condition = concept_name)|>
            select(person_id, condition, condition_start_date, condition_end_date)
    })
    observe({
        table <- condition_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$condition_DT <- 
        render_db_DT(table)
    })
}



make_server_visit_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    visit_table <- reactiveVal({
        dt <- con$visit_occurrence |> 
            inner_join(concept_name, by = c("visit_concept_id" = "concept_id")) |>
            rename(visit_type = concept_name)|>
            select(person_id, visit_type, visit_start_date, visit_end_date, visit_occurrence_id)
    })
    observe({
        table <- visit_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$visit_DT <- 
        render_db_DT(table)
    })
}



make_server_procedure_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    procedure_table <- reactiveVal({
        dt <- con$procedure_occurrence |> 
            inner_join(concept_name, by = c("procedure_concept_id" = "concept_id")) |>
            rename(procedure = concept_name)|>
            select(person_id, procedure, procedure_date, procedure_occurrence_id)
    })
    observe({
        table <- procedure_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$procedure_DT <- 
        render_db_DT(table)
    })
}



make_server_measurement_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    measurement_table <- reactiveVal({
        dt <- con$measurement |> 
            inner_join(concept_name, by = c("measurement_concept_id" = "concept_id")) |>
            rename(measurement = concept_name)|>
            inner_join(concept_name, by = c("unit_concept_id" = "concept_id")) |>
            rename(unit = concept_name)|>
            select(person_id, measurement, value_as_number, unit, measurement_date, measurement_id)
    })
    observe({
        table <- measurement_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$measurement_DT <- 
        render_db_DT(table)
    })
}



make_server_drug_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    drug_table <- reactiveVal({
        dt <- con$drug_exposure |> 
            inner_join(concept_name, by = c("drug_concept_id" = "concept_id")) |>
            rename(drug = concept_name)|>
            select(person_id, drug, drug_exposure_start_date, drug_exposure_end_date, quantity, drug_exposure_id)
    })
    observe({
        table <- drug_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$drug_DT <- 
        render_db_DT(table)
    })
}



make_server_note_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    note_table <- reactiveVal({
        dt <- con$note |> 
            inner_join(concept_name, by = c("note_type_concept_id" = "concept_id")) |>
            rename(note_type = concept_name)|>
            mutate(note_text_preview = substr(note_text, 1, 100)) |>
            select(person_id, note_type, note_date, note_text_preview, note_id)
    })
    observe({
        table <- note_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$note_DT <- 
        render_db_DT(table)
    })
}



make_server_death_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    death_table <- reactiveVal({
        dt <- con$death |> 
            inner_join(concept_name, by = c("death_type_concept_id" = "concept_id")) |>
            rename(death_type = concept_name)|>
            left_join(concept_name, by = c("cause_concept_id" = "concept_id")) |>
            rename(cause = concept_name)|>
            select(person_id, death_date, death_type, cause)
    })
    observe({
        table <- death_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$death_DT <- 
        render_db_DT(table)
    })
}



browser_server <- function(input, output, session, con) {
    params <- list()
    params$selected_person_id <- reactive(input$tbl_person_id)
    make_server_person_DT(input, output, session, con, params)
    make_server_visit_DT(input, output, session, con, params)
    make_server_condition_DT(input, output, session, con, params)
    make_server_procedure_DT(input, output, session, con, params)
    make_server_measurement_DT(input, output, session, con, params)
    make_server_drug_DT(input, output, session, con, params)
    make_server_note_DT(input, output, session, con, params)
    make_server_death_DT(input, output, session, con, params)
    
    observeEvent(input$tbl_person_id, {
        print(input$tbl_person_id)
    })
}