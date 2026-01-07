
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




browser_server <- function(input, output, session, con) {
    params <- list()
    params$selected_person_id <- reactive(input$tbl_person_id)
    make_server_person_DT(input, output, session, con, params)
    make_server_condition_DT(input, output, session, con, params)
    
    observeEvent(input$tbl_person_id, {
        print(input$tbl_person_id)
    })
}