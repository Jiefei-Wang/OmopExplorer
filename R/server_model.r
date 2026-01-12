make_clickable_id <- function(id_name, id_value){
    table_name <- omop_key_to_table[[id_name]]
    
    htmlTemplate(
        filename = system.file("html/modal_clickable.html", package = "OmopExplorer"),
        table_name = table_name,
        id_value = id_value
    )
}

make_modal_view_item <- function(label, value){
    if (is.null(value) || length(value) == 0 || is.na(value)) {
        value <- ""
    }else if (label %in% names(omop_key_to_table)) {
        value <- make_clickable_id(label, value)
    } else {
        value <- as.character(value)
    }
    htmlTemplate(
        filename = system.file("html/modal_item.html", package = "OmopExplorer"),
        label = label,
        value = value
    )
}

make_modal_title <- function(detail_list){
    table_name <- detail_list$shiny_table_name
    person_id <- detail_list$person_id
    row_id <- detail_list[[omop_key_columns[[table_name]]]]
    if (table_name == "person"){
        glue("person: {person_id}")
    }else{
        HTML(glue("{table_name}: {row_id} for person: {make_clickable_id('person_id', person_id)}"))
    }
}



register_server_modal <- function(input, output, session, con, params){
    modal_key <- reactiveVal(NULL)

    observeEvent(input$tbl_dblclick_meta, {
        req(input$tbl_dblclick_meta)
        meta_dt <- unpack_meta_info(input$tbl_dblclick_meta)

        dt <- list(
            table_name = meta_dt$table_name,
            row_id = meta_dt$row_id
        )
        modal_key(dt)
    })

    observeEvent(input$modal_click_meta, {
        req(input$modal_click_meta)
        modal_key(input$modal_click_meta)
    })

    modal_details <- reactive({
        req(modal_key())
        meta_dt <- modal_key()
        table_info <- params$table_info[[meta_dt$table_name]]
        req(!is.null(table_info))
        id_col <- table_info$key_column

        req(!is.null(id_col))
        detail <- table_info$table |> 
            filter(!!rlang::sym(id_col) == meta_dt$row_id) |>
            concept_id_to_concept_name(
                con=con,
                table_name = meta_dt$table_name,
                tbl_all_cols = table_info$columns
            ) |>
            collect()|>
            as.list()

        detail$shiny_table_name <- meta_dt$table_name
        detail
    })

    observeEvent(modal_details(), {
        title <- make_modal_title(modal_details())
        showModal(
            modalDialog(
                title = title,
                uiOutput("item_detail_list"),
                easyClose = TRUE,
                size = "l"
        ))
    })

    output$item_detail_list <- renderUI({
        req(modal_details())
        dt <- modal_details()
        id_col <- params$table_info[[dt$shiny_table_name]]$key_column
        
        dt$person_id <- NULL
        dt[[id_col]] <- NULL
        
        div(
            lapply(names(dt), function(col_name) {
                val <- dt[[col_name]]
                make_modal_view_item(col_name, val)
            })
        )
    })
}
