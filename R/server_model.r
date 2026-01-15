make_clickable_id <- function(id_name, id_value){
    table_name <- omop_key_to_table[[id_name]]
    
    htmlTemplate(
        filename = system.file("html/modal_clickable.html", package = "OmopExplorer"),
        table_name = table_name,
        id_value = id_value
    )
}

make_modal_id_list <- function(id_name, id_values){
    structure(
        list(id_name = id_name, values = id_values),
        class = "modal_clickable_ids"
    )
}

make_clickable_id_list <- function(id_name, id_values){
    if (length(id_values) == 0) {
        return("")
    }
    tags$span(
        lapply(seq_along(id_values), function(idx) {
            tagList(
                make_clickable_id(id_name, id_values[[idx]]),
                if (idx < length(id_values)) ", " else ""
            )
        })
    )
}

make_modal_view_item <- function(label, value){
    if (is.null(value) || length(value) == 0) {
        value <- ""
    }else if (length(value) == 1 && is.na(value)) {
        value <- ""
    }else if (inherits(value, "modal_clickable_ids")) {
        value <- make_clickable_id_list(value$id_name, value$values)
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

make_visit_display_fields <- function(detail_list, params){
    id_col <- params$table_info[[detail_list$shiny_table_name]]$key_column
    fields <- c(
        "person_id",
        id_col,
        "visit_start_date",
        "visit_end_date",
        "visit_concept_id",
        "visit_type_concept_id",
        "admitted_from_concept_id",
        "discharge_to_concept_id",
        "provider_id",
        "care_site_id"
    )
    fields <- unique(fields[!is.na(fields) & nzchar(fields)])
    fields[!grepl("source_value", fields)]
}

make_grid_fields <- function(detail_list, params, fields){
    id_col <- params$table_info[[detail_list$shiny_table_name]]$key_column
    fields <- vapply(fields, function(field_name) {
        if (identical(field_name, "KEY_COLUMN")) {
            return(id_col)
        }
        field_name
    }, character(1))
    fields <- unique(fields[!is.na(fields) & nzchar(fields)])
    fields <- fields[!grepl("source_value", fields)]
    intersect(fields, names(detail_list))
}

make_grid_items <- function(detail_list, fields){
    lapply(fields, function(field_name) {
        div(
            class = "modal-grid-item",
            make_modal_view_item(field_name, detail_list[[field_name]])
        )
    })
}

make_visit_grid_items <- function(detail_list, params){
    fields <- make_visit_display_fields(detail_list, params)
    make_grid_items(detail_list, fields)
}

make_visit_related_items <- function(detail_list){
    related_names <- names(detail_list)[grepl("\\(related\\)$", names(detail_list))]
    if (length(related_names) == 0) {
        return(NULL)
    }
    tagList(
        tags$h5(style = "margin-top: 12px;", "Related records"),
        lapply(related_names, function(field_name) {
            make_modal_view_item(field_name, detail_list[[field_name]])
        })
    )
}

render_visit_modal <- function(detail_list, params){
    grid_items <- make_visit_grid_items(detail_list, params)
    related_items <- make_visit_related_items(detail_list)
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            grid_items
        ),
        related_items
    )
}

render_person_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "gender_concept_id",
        "birth_datetime",
        "birth_date",
        "race_concept_id",
        "ethnicity_concept_id",
        "location_id",
        "care_site_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_condition_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "condition_start_date",
        "condition_end_date",
        "condition_concept_id",
        "condition_type_concept_id",
        "condition_status_concept_id",
        "visit_occurrence_id",
        "provider_id",
        "care_site_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_procedure_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "procedure_date",
        "procedure_datetime",
        "procedure_concept_id",
        "procedure_type_concept_id",
        "modifier_concept_id",
        "visit_occurrence_id",
        "provider_id",
        "care_site_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_drug_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "drug_exposure_start_date",
        "drug_exposure_end_date",
        "drug_concept_id",
        "drug_type_concept_id",
        "route_concept_id",
        "dose_unit_concept_id",
        "quantity",
        "days_supply",
        "provider_id",
        "visit_occurrence_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_measurement_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "measurement_date",
        "measurement_datetime",
        "measurement_concept_id",
        "measurement_type_concept_id",
        "value_as_number",
        "unit_concept_id",
        "value_as_concept_id",
        "operator_concept_id",
        "visit_occurrence_id",
        "provider_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_observation_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "observation_date",
        "observation_datetime",
        "observation_concept_id",
        "observation_type_concept_id",
        "value_as_number",
        "unit_concept_id",
        "value_as_concept_id",
        "qualifier_concept_id",
        "visit_occurrence_id",
        "provider_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_device_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "device_exposure_start_date",
        "device_exposure_end_date",
        "device_concept_id",
        "device_type_concept_id",
        "unique_device_id",
        "visit_occurrence_id",
        "provider_id",
        "care_site_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_note_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "note_date",
        "note_class_concept_id",
        "note_type_concept_id",
        "provider_id",
        "visit_occurrence_id",
        "note_title"
    ))
    items <- make_grid_items(detail_list, fields)
    if ("note_text" %in% names(detail_list)) {
        items <- c(
            items,
            list(
                div(
                    class = "modal-grid-item",
                    style = "grid-column: 1 / -1;",
                    make_modal_view_item("note_text", detail_list[["note_text"]])
                )
            )
        )
    }
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            items
        )
    )
}

render_death_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "death_date",
        "death_type_concept_id",
        "cause_concept_id",
        "cause_source_value",
        "cause_source_concept_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_provider_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "provider_id",
        "specialty_concept_id",
        "gender_concept_id",
        "care_site_id",
        "provider_name",
        "npi"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_care_site_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "care_site_id",
        "care_site_name",
        "place_of_service_concept_id",
        "location_id"
    ))
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        )
    )
}

render_visit_detail_modal <- function(detail_list, params){
    fields <- make_grid_fields(detail_list, params, c(
        "person_id",
        "KEY_COLUMN",
        "visit_detail_start_date",
        "visit_detail_end_date",
        "visit_detail_concept_id",
        "visit_detail_type_concept_id",
        "admitted_from_concept_id",
        "discharge_to_concept_id",
        "provider_id",
        "care_site_id"
    ))
    related_items <- make_visit_related_items(detail_list)
    tagList(
        div(
            style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px 16px;",
            make_grid_items(detail_list, fields)
        ),
        related_items
    )
}

fetch_modal_detail_generic <- function(meta_dt, con, params){
    table_name <- meta_dt$table_name
    current_table_info <- params$table_info[[table_name]]
    req(!is.null(current_table_info))
    id_col <- current_table_info$key_column
    concept_columns <- current_table_info$concept_columns

    req(!is.null(id_col))
    detail <- con[[table_name]] |> 
        filter(!!rlang::sym(id_col) == meta_dt$row_id) |>
        concept_id_to_concept_name(
            con=con,
            table_name = table_name,
            concept_columns = concept_columns
        ) |>
        collect()|>
        as.list()
    detail$shiny_table_name <- table_name
    detail
}

get_visit_related_ids <- function(con, visit_id, params){
    related_tables <- c(
        "condition_occurrence",
        "procedure_occurrence",
        "drug_exposure",
        "measurement",
        "observation",
        "device_exposure",
        "note"
    )
    available_tables <- names(params$table_info)
    related_tables <- intersect(related_tables, available_tables)

    results <- list()
    for (tbl_name in related_tables) {
        table_info <- params$table_info[[tbl_name]]
        if (is.null(table_info)) {
            next
        }
        if (!("visit_occurrence_id" %in% table_info$columns)) {
            next
        }
        key_col <- table_info$key_column
        if (is.null(key_col)) {
            next
        }
        ids <- con[[tbl_name]] |>
            filter(visit_occurrence_id == visit_id) |>
            select(!!rlang::sym(key_col)) |>
            distinct() |>
            collect() |>
            dplyr::pull(1)
        if (length(ids) > 0) {
            results[[tbl_name]] <- ids
        }
    }
    results
}

fetch_modal_detail_visit <- function(meta_dt, con, params){
    detail <- fetch_modal_detail_generic(meta_dt, con, params)
    id_col <- params$table_info[[meta_dt$table_name]]$key_column
    if (!is.null(id_col) && length(detail[[id_col]]) > 0) {
        visit_id <- detail$visit_occurrence_id
        related_ids <- get_visit_related_ids(con, visit_id, params)
        if (length(related_ids) > 0) {
            for (tbl_name in names(related_ids)) {
                key_col <- params$table_info[[tbl_name]]$key_column
                label <- glue("{key_col} (related)")
                detail[[label]] <- make_modal_id_list(key_col, related_ids[[tbl_name]])
            }
        }
    }
    detail
}

fetch_modal_detail_person <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_condition <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_procedure <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_measurement <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_drug <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_note <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_death <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_provider <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

fetch_modal_detail_care_site <- function(meta_dt, con, params){
    fetch_modal_detail_generic(meta_dt, con, params)
}

dispatch_modal_detail <- function(meta_dt, con, params){
    if (meta_dt$table_name == "visit_occurrence" || meta_dt$table_name == "visit_detail") {
        return(fetch_modal_detail_visit(meta_dt, con, params))
    }
    if (meta_dt$table_name == "person") {
        return(fetch_modal_detail_person(meta_dt, con, params))
    }
    if (meta_dt$table_name == "condition_occurrence") {
        return(fetch_modal_detail_condition(meta_dt, con, params))
    }
    if (meta_dt$table_name == "procedure_occurrence") {
        return(fetch_modal_detail_procedure(meta_dt, con, params))
    }
    if (meta_dt$table_name == "measurement") {
        return(fetch_modal_detail_measurement(meta_dt, con, params))
    }
    if (meta_dt$table_name == "drug_exposure") {
        return(fetch_modal_detail_drug(meta_dt, con, params))
    }
    if (meta_dt$table_name == "note") {
        return(fetch_modal_detail_note(meta_dt, con, params))
    }
    if (meta_dt$table_name == "death") {
        return(fetch_modal_detail_death(meta_dt, con, params))
    }
    if (meta_dt$table_name == "provider") {
        return(fetch_modal_detail_provider(meta_dt, con, params))
    }
    if (meta_dt$table_name == "care_site") {
        return(fetch_modal_detail_care_site(meta_dt, con, params))
    }
    fetch_modal_detail_generic(meta_dt, con, params)
}

make_modal_history_item <- function(idx, meta_dt, is_active){
    label <- glue("{meta_dt$table_name}: {meta_dt$row_id}")
    click_meta <- jsonlite::toJSON(list(index = idx), auto_unbox = TRUE)
    remove_meta <- jsonlite::toJSON(list(index = idx), auto_unbox = TRUE)
    div(
        style = if (is_active) "display:flex; align-items:center; justify-content:space-between; padding:6px 8px; background:#eef2ff; border-radius:4px; margin-bottom:4px;" else "display:flex; align-items:center; justify-content:space-between; padding:6px 8px; margin-bottom:4px;",
        tags$a(
            href = "#",
            onclick = sprintf("Shiny.setInputValue('modal_history_click', %s, {priority: 'event'}); return false;", click_meta),
            style = "flex:1; text-decoration:none;",
            label
        ),
        tags$a(
            href = "#",
            onclick = sprintf("Shiny.setInputValue('modal_history_remove', %s, {priority: 'event'}); return false;", remove_meta),
            style = "margin-left: 6px; color: #a33; text-decoration: none;",
            "×"
        )
    )
}

format_raw_value <- function(value){
    if (inherits(value, "modal_clickable_ids")) {
        return(paste(value$values, collapse = ", "))
    }
    if (is.null(value) || length(value) == 0) {
        return("")
    }
    if (length(value) > 1) {
        return(paste(value, collapse = ", "))
    }
    if (is.na(value)) {
        return("")
    }
    as.character(value)
}



register_server_modal <- function(input, output, session, con, params){
    modal_state <- reactiveVal(list(key = NULL, add_history = TRUE))
    modal_history <- reactiveVal(list())
    modal_active_index <- reactiveVal(NA_integer_)
    modal_show_raw <- reactiveVal(FALSE)
    modal_record_cache <- new.env(parent = emptyenv())

    make_record_cache_key <- function(table_name, row_id){
        paste0(table_name, "::", row_id)
    }

    get_cached_record <- function(table_name, row_id){
        if (is.null(table_name) || is.null(row_id)) {
            return(NULL)
        }
        key <- make_record_cache_key(table_name, row_id)
        modal_record_cache[[key]]
    }

    set_cached_record <- function(table_name, row_id, record){
        if (is.null(table_name) || is.null(row_id)) {
            return(invisible(NULL))
        }
        key <- make_record_cache_key(table_name, row_id)
        modal_record_cache[[key]] <- record
    }

    fetch_modal_with_cache <- function(meta_dt){
        cached_detail <- get_cached_record(meta_dt$table_name, meta_dt$row_id)
        if (!is.null(cached_detail)) {
            return(cached_detail)
        }
        detail <- dispatch_modal_detail(meta_dt, con, params)
        set_cached_record(meta_dt$table_name, meta_dt$row_id, detail)
        detail
    }

    set_modal_key <- function(key, add_history = TRUE){
        modal_state(list(key = key, add_history = add_history))
    }

    add_modal_history <- function(key){
        history <- modal_history()
        existing_idx <- which(vapply(history, function(x) identical(x, key), logical(1)))
        if (length(existing_idx) > 0) {
            modal_active_index(existing_idx[[1]])
            return(invisible(NULL))
        }
        history <- append(history, list(key))
        modal_history(history)
        modal_active_index(length(history))
    }

    observeEvent(input$tbl_dblclick_meta, {
        req(input$tbl_dblclick_meta)
        meta_dt <- unpack_meta_info(input$tbl_dblclick_meta)

        dt <- list(
            table_name = meta_dt$table_name,
            row_id = meta_dt$row_id
        )
        set_modal_key(dt, add_history = TRUE)
    })

    observeEvent(input$modal_click_meta, {
        req(input$modal_click_meta)
        set_modal_key(input$modal_click_meta, add_history = TRUE)
    })

    observeEvent(input$modal_history_click, {
        req(input$modal_history_click)
        history <- modal_history()
        idx <- as.integer(input$modal_history_click$index)
        if (!is.na(idx) && idx >= 1 && idx <= length(history)) {
            set_modal_key(history[[idx]], add_history = FALSE)
            modal_active_index(idx)
        }
    })

    observeEvent(input$modal_history_remove, {
        req(input$modal_history_remove)
        history <- modal_history()
        idx <- as.integer(input$modal_history_remove$index)
        if (is.na(idx) || idx < 1 || idx > length(history)) {
            return(NULL)
        }
        history <- history[-idx]
        modal_history(history)
        active_idx <- modal_active_index()
        if (length(history) == 0) {
            modal_active_index(NA_integer_)
            modal_state(list(key = NULL, add_history = FALSE))
            return(NULL)
        }
        if (is.na(active_idx) || active_idx == idx) {
            new_idx <- min(idx, length(history))
            modal_active_index(new_idx)
            set_modal_key(history[[new_idx]], add_history = FALSE)
        } else if (active_idx > idx) {
            modal_active_index(active_idx - 1)
        }
    })

    observeEvent(input$modal_toggle_raw, {
        modal_show_raw(!isTRUE(modal_show_raw()))
    })

    modal_details <- reactive({
        req(modal_state()$key)
        meta_dt <- modal_state()$key
        fetch_modal_with_cache(meta_dt)
    })

    observeEvent(modal_details(), {
        if (isTRUE(modal_state()$add_history)) {
            add_modal_history(modal_state()$key)
        }
        modal_show_raw(FALSE)
        title <- make_modal_title(modal_details())
        showModal(
            modalDialog(
                title = title,
                div(
                    style = "display: flex; gap: 12px; align-items: stretch;",
                    div(
                        style = "width: 220px; border-right: 1px solid #e6e6e6; padding-right: 8px; overflow-y: auto; max-height: 70vh;",
                        uiOutput("modal_history_list")
                    ),
                    div(
                        style = "flex: 1; min-width: 0;",
                        div(
                            style = "display: flex; justify-content: flex-end; margin-bottom: 6px;",
                            uiOutput("modal_toggle_button")
                        ),
                        uiOutput("item_detail_list"),
                        uiOutput("modal_raw_block")
                    )
                ),
                easyClose = TRUE,
                size = "l"
        ))
    })

    output$modal_history_list <- renderUI({
        history <- modal_history()
        if (length(history) == 0) {
            return(tags$div(style = "color: #777;", "No history"))
        }
        active_idx <- modal_active_index()
        tagList(
            lapply(seq_along(history), function(idx) {
                make_modal_history_item(idx, history[[idx]], isTRUE(idx == active_idx))
            })
        )
    })

    output$item_detail_list <- renderUI({
        req(modal_details())
        if (isTRUE(modal_show_raw())) {
            return(NULL)
        }
        dt <- modal_details()
        if (dt$shiny_table_name == "visit_occurrence") {
            return(render_visit_modal(dt, params))
        }
        if (dt$shiny_table_name == "visit_detail") {
            return(render_visit_detail_modal(dt, params))
        }
        if (dt$shiny_table_name == "person") {
            return(render_person_modal(dt, params))
        }
        if (dt$shiny_table_name == "condition_occurrence") {
            return(render_condition_modal(dt, params))
        }
        if (dt$shiny_table_name == "procedure_occurrence") {
            return(render_procedure_modal(dt, params))
        }
        if (dt$shiny_table_name == "drug_exposure") {
            return(render_drug_modal(dt, params))
        }
        if (dt$shiny_table_name == "measurement") {
            return(render_measurement_modal(dt, params))
        }
        if (dt$shiny_table_name == "observation") {
            return(render_observation_modal(dt, params))
        }
        if (dt$shiny_table_name == "device_exposure") {
            return(render_device_modal(dt, params))
        }
        if (dt$shiny_table_name == "note") {
            return(render_note_modal(dt, params))
        }
        if (dt$shiny_table_name == "death") {
            return(render_death_modal(dt, params))
        }
        if (dt$shiny_table_name == "provider") {
            return(render_provider_modal(dt, params))
        }
        if (dt$shiny_table_name == "care_site") {
            return(render_care_site_modal(dt, params))
        }
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

    output$modal_toggle_button <- renderUI({
        if (isTRUE(modal_show_raw())) {
            return(actionButton("modal_toggle_raw", "Back to summary", class = "btn btn-sm btn-secondary"))
        }
        actionButton("modal_toggle_raw", "Raw data", class = "btn btn-sm btn-secondary")
    })

    output$modal_raw_block <- renderUI({
        req(modal_details())
        if (!isTRUE(modal_show_raw())) {
            return(NULL)
        }
        dt <- modal_details()
        div(
            style = "margin-top: 10px;",
            lapply(names(dt), function(col_name) {
                val <- format_raw_value(dt[[col_name]])
                div(
                    style = "display: flex; gap: 10px; align-items: flex-start; padding: 6px 0; border-bottom: 1px solid #efefef;",
                    tags$strong(style = "min-width: 180px;", col_name),
                    tags$span(style = "white-space: pre-wrap; overflow-wrap: break-word;", val)
                )
            })
        )
    })
}
