make_search_box <- function(inputId, label, value = "", placeholder="", clearButtonId = NULL) {
    # if (is.null(clearButtonId)) {
    #     clear_btn_id <- paste0("clear_", inputId)
    # } else {
    #     clear_btn_id <- clearButtonId
    # }
    div(
        style = "margin-bottom: 15px;",
        tags$label(label, `for` = inputId),
        div(
            class = "input-group",
            tags$input(
                type = "text",
                class = "form-control",
                id = inputId,
                value = value,
                placeholder = placeholder
            )
            # span(
            #     class = "input-group-btn",
            #     actionButton(
            #         inputId = clear_btn_id,
            #         label = icon("remove"),
            #         class = "btn-default btn-sm",
            #         style = "padding: 3px 8px;"
            #     )
            # )
        )
    )
}