
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
    select_pipe <- function(x) {
        if ("note_text" %in% colnames(x)) {
            x |>
                mutate(note_text_preview = substr(note_text, 1, 100))
        } else {
            x
        }
    }
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
