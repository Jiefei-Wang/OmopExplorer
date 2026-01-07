
make_server_person_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    reactiveVal(
        con$person |> 
        mutate(birth_date = as.Date(birth_datetime)) |>
        left_join(concept_name, by = c("gender_concept_id" = "concept_id"), suffix = c("", "_gender")) |>
        mutate(gender = case_when(
            is.na(gender_concept_id) | gender_concept_id == 0 ~ gender_source_value,
            TRUE ~ paste0(concept_name, " (", gender_concept_id, ")")
        )) |>
        left_join(concept_name, by = c("race_concept_id" = "concept_id"), suffix = c("", "_race")) |>
        mutate(race = case_when(
            is.na(race_concept_id) | race_concept_id == 0 ~ race_source_value,
            TRUE ~ paste0(concept_name_race, " (", race_concept_id, ")")
        )) |>
        left_join(concept_name, by = c("ethnicity_concept_id" = "concept_id"), suffix = c("", "_ethnicity")) |>
        mutate(ethnicity = case_when(
            is.na(ethnicity_concept_id) | ethnicity_concept_id == 0 ~ ethnicity_source_value,
            TRUE ~ paste0(concept_name_ethnicity, " (", ethnicity_concept_id, ")")
        )) |>
        select(person_id, birth_date, gender, race, ethnicity)
    ) -> person_table
    
    observe({
        output$person_DT <- render_db_DT(person_table(),
            additional_options = list(
                order = list(list(1, 'asc'))
            ),
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
            left_join(concept_name, by = c("condition_concept_id" = "concept_id")) |>
            mutate(condition = case_when(
                is.na(condition_concept_id) | condition_concept_id == 0 ~ condition_source_value,
                TRUE ~ paste0(concept_name, " (", condition_concept_id, ")")
            )) |>
            select(person_id, condition_occurrence_id, condition, condition_start_date, condition_end_date)
    })
    observe({
        table <- condition_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$condition_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
    })
}



make_server_visit_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    visit_table <- reactiveVal({
        dt <- con$visit_occurrence |> 
            left_join(concept_name, by = c("visit_concept_id" = "concept_id")) |>
            mutate(visit_type = case_when(
                is.na(visit_concept_id) | visit_concept_id == 0 ~ visit_source_value,
                TRUE ~ paste0(concept_name, " (", visit_concept_id, ")")
            )) |>
            select(person_id, visit_occurrence_id, visit_type, visit_start_date, visit_end_date)
    })
    observe({
        table <- visit_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$visit_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
    })
}



make_server_procedure_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    procedure_table <- reactiveVal({
        dt <- con$procedure_occurrence |> 
            left_join(concept_name, by = c("procedure_concept_id" = "concept_id")) |>
            mutate(procedure = case_when(
                is.na(procedure_concept_id) | procedure_concept_id == 0 ~ procedure_source_value,
                TRUE ~ paste0(concept_name, " (", procedure_concept_id, ")")
            )) |>
            select(person_id, procedure_occurrence_id, procedure, procedure_date)
    })
    observe({
        table <- procedure_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$procedure_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
    })
}



make_server_measurement_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    measurement_table <- reactiveVal({
        dt <- con$measurement |> 
            left_join(concept_name, by = c("measurement_concept_id" = "concept_id"), suffix = c("", "_meas")) |>
            mutate(measurement = case_when(
                is.na(measurement_concept_id) | measurement_concept_id == 0 ~ measurement_source_value,
                TRUE ~ paste0(concept_name, " (", measurement_concept_id, ")")
            )) |>
            left_join(concept_name, by = c("unit_concept_id" = "concept_id"), suffix = c("", "_unit")) |>
            mutate(unit = case_when(
                is.na(unit_concept_id) | unit_concept_id == 0 ~ unit_source_value,
                TRUE ~ paste0(concept_name_unit, " (", unit_concept_id, ")")
            )) |>
            select(person_id, measurement_id, measurement, value_as_number, unit, measurement_date)
    })
    observe({
        table <- measurement_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$measurement_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
    })
}



make_server_drug_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    drug_table <- reactiveVal({
        dt <- con$drug_exposure |> 
            left_join(concept_name, by = c("drug_concept_id" = "concept_id")) |>
            mutate(drug = case_when(
                is.na(drug_concept_id) | drug_concept_id == 0 ~ drug_source_value,
                TRUE ~ paste0(concept_name, " (", drug_concept_id, ")")
            )) |>
            select(person_id, drug_exposure_id, drug, drug_exposure_start_date, drug_exposure_end_date, quantity)
    })
    observe({
        table <- drug_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$drug_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
    })
}



make_server_note_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    note_table <- reactiveVal({
        dt <- con$note |> 
            left_join(concept_name, by = c("note_type_concept_id" = "concept_id")) |>
            mutate(note_type = case_when(
                is.na(note_type_concept_id) | note_type_concept_id == 0 ~ note_title,
                TRUE ~ paste0(concept_name, " (", note_type_concept_id, ")")
            )) |>
            mutate(note_text_preview = substr(note_text, 1, 100)) |>
            select(person_id, note_id, note_type, note_date, note_text_preview)
    })
    observe({
        table <- note_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$note_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
    })
}



make_server_death_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    death_table <- reactiveVal({
        dt <- con$death |> 
            left_join(concept_name, by = c("death_type_concept_id" = "concept_id"), suffix = c("", "_type")) |>
            mutate(death_type = case_when(
                is.na(death_type_concept_id) | death_type_concept_id == 0 ~ as.character(death_type_concept_id),
                TRUE ~ paste0(concept_name, " (", death_type_concept_id, ")")
            )) |>
            left_join(concept_name, by = c("cause_concept_id" = "concept_id"), suffix = c("", "_cause")) |>
            mutate(cause = case_when(
                is.na(cause_concept_id) | cause_concept_id == 0 ~ cause_source_value,
                TRUE ~ paste0(concept_name_cause, " (", cause_concept_id, ")")
            )) |>
            select(person_id, death_date, death_type, cause)
    })
    observe({
        table <- death_table()
        if (!is.null(params$selected_person_id())) {
            table <- table |> 
            filter(person_id == params$selected_person_id())
        }
        output$death_DT <- 
        render_db_DT(table, additional_options = list(
            order = list(list(2, 'asc'))
        ))
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