if (!"OmopExplorer" %in% loadedNamespaces()) {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("pkgload is required to run this test file directly.")
    }
    pkgload::load_all(path = ".", export_all = FALSE)
}
source(testthat::test_path("helper-server-setup.R"))

test_that("modal history helpers build valid tags", {
    item <- OmopExplorer:::make_modal_history_item(
        1,
        list(table_name = "person", row_id = 1),
        TRUE
    )
    expect_true(inherits(item, "shiny.tag") || inherits(item, "shiny.tag.list"))

    list_val <- OmopExplorer:::make_modal_id_list("person_id", c(1, 2))
    expect_true(inherits(list_val, "modal_clickable_ids"))
    expect_identical(list_val$id_name, "person_id")
    expect_true(length(list_val$values) == 2)

    raw_val <- OmopExplorer:::format_raw_value(list_val)
    expect_true(is.character(raw_val))
})

test_that("modal history navigation and raw toggle do not error", {
    con <- OmopExplorer::mockCon()
    params <- make_test_params(con, "person")

    shiny::testServer(function(input, output, session) {
        OmopExplorer:::register_server_modal(input, output, session, con, params)
    }, {
        person_id <- get_first_person_id(con)
        if (!is.na(person_id)) {
            meta <- OmopExplorer:::pack_meta_info("person", person_id, person_id)
            session$setInputs(tbl_dblclick_meta = meta)
            session$flushReact()
            expect_silent(output$modal_history_list)
            expect_silent(output$item_detail_list)
            expect_silent(output$modal_toggle_button)

            session$setInputs(modal_history_click = list(index = 1))
            session$flushReact()
            expect_silent(output$item_detail_list)

            session$setInputs(modal_toggle_raw = 1)
            session$flushReact()
            expect_silent(output$modal_raw_block)
            expect_silent(output$modal_toggle_button)

            session$setInputs(modal_toggle_raw = 2)
            session$flushReact()
            expect_silent(output$item_detail_list)

            session$setInputs(modal_history_remove = list(index = 1))
            session$flushReact()
            expect_silent(output$modal_history_list)
        } else {
            expect_silent(output$modal_history_list)
        }
    })
})

test_that("visit related id lookup is table-safe", {
    con <- OmopExplorer::mockCon()
    params <- make_test_params(con, "person")

    if ("visit_occurrence" %in% names(params$table_info)) {
        visit_tbl <- params$table_info$visit_occurrence$table
        visit_id_col <- params$table_info$visit_occurrence$key_column
        if (!is.null(visit_id_col) && nrow(visit_tbl |> head(1) |> collect()) > 0) {
            visit_id <- visit_tbl |>
                dplyr::select(!!rlang::sym(visit_id_col)) |>
                head(1) |>
                dplyr::collect() |>
                dplyr::pull(1)
            related <- OmopExplorer:::get_visit_related_ids(visit_id, params, visit_id_col)
            expect_true(is.list(related))
        } else {
            expect_true(TRUE)
        }
    } else {
        expect_true(TRUE)
    }
})

test_that("visit modal uses grid fields and excludes source values", {
    con <- OmopExplorer::mockCon()
    params <- make_test_params(con, "person")

    if ("visit_occurrence" %in% names(params$table_info)) {
        visit_id_col <- params$table_info$visit_occurrence$key_column
        visit_tbl <- params$table_info$visit_occurrence$table
        if (!is.null(visit_id_col) && nrow(visit_tbl |> head(1) |> collect()) > 0) {
            visit_id <- visit_tbl |>
                dplyr::select(!!rlang::sym(visit_id_col)) |>
                head(1) |>
                dplyr::collect() |>
                dplyr::pull(1)
            meta <- list(table_name = "visit_occurrence", row_id = visit_id)
            detail <- OmopExplorer:::fetch_modal_detail_visit(meta, con, params)
            fields <- OmopExplorer:::make_visit_display_fields(detail, params)
            fields <- intersect(fields, names(detail))

            expect_true(length(fields) > 0)
            expect_false(any(grepl("source_value", fields)))

            rendered <- OmopExplorer:::render_visit_modal(detail, params)
            expect_true(inherits(rendered, "shiny.tag") || inherits(rendered, "shiny.tag.list"))
        } else {
            expect_true(TRUE)
        }
    } else {
        expect_true(TRUE)
    }
})
