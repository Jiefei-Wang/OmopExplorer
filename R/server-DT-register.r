# Add panes based on omop_panes definition
register_server_DT <- function(input, output, session, con, params){
    register_server_person_DT(input, output, session, con, params)
    register_server_note_DT(input, output, session, con, params)

    show_tables <- sapply(omop_panes, function(pane) pane$table_name)
    show_tables <- setdiff(show_tables, c("person", "note"))
    show_tables <- intersect(show_tables, names(params$table_info))
    for (table_name in show_tables){
        local({
            output_id <- paste0(table_name, "_DT")
            local_table_name <- table_name
            observe({
                req(params$displayed_table_name() == local_table_name)
                output[[output_id]] <- 
                render_db_DT(
                    params = params,
                    con = con,
                    table_name = local_table_name)
                })
        })
    }
}


# special handling for person table to support row selection
register_server_person_DT <- function(input, output, session, con, params){
    observe({
        # cannot use req() here because it will erase the selection after switching tabs
        # req(params$displayed_table_name() == "person")
        output$person_DT <- render_db_DT(
            params = params,
            con = con,
            table_name = "person",
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
            post_process_pipe = select_pipe
            )
    })
}