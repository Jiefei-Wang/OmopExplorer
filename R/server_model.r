make_modal_view_item <- function(label, value){
    tagList(
        tags$strong(label, style = "display: block; margin-top: 10px; color: #555;"),
        tags$div(
            style = "border: 1px solid #e3e3e3; background-color: #f9f9f9; padding: 10px; border-radius: 4px; white-space: pre-wrap; overflow-wrap: break-word;",
            if (is.na(value) || as.character(value) == "") span("NA", style="color: #ccc; font-style: italic;") else as.character(value)
        )
    )
}


make_server_modal <- function(input, output, session, con, params){
    modal_details <- eventReactive(input$tbl_dblclick_meta, {
        req(input$tbl_dblclick_meta)
        meta_dt <- unpack_meta_info(input$tbl_dblclick_meta)
        print(meta_dt)

        detail <- con[[meta_dt$table_name]] |> 
            filter(!!rlang::sym(omop_key_columns[[meta_dt$table_name]]) == meta_dt$row_id) |>
            collect()
        detail
    })

    observeEvent(modal_details(), {
        showModal(modalDialog(
            title = paste("Person", modal_details()$person_id[1]),
            uiOutput("item_detail_list"),
            easyClose = TRUE,
            size = "l"
        ))
    })

    output$item_detail_list <- renderUI({
        req(modal_details())
        dt <- modal_details()
        
        div(
            lapply(names(dt), function(col_name) {
                val <- dt[[col_name]]
                make_modal_view_item(col_name, val)
            })
        )
    })
}