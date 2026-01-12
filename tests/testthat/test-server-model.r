test_that("modal helpers return HTML or tags", {
    clickable <- make_clickable_id("person_id", 1)
    expect_true(inherits(clickable, "shiny.tag") || inherits(clickable, "shiny.tag.list"))

    view_item <- make_modal_view_item("person_id", 1)
    expect_true(inherits(view_item, "shiny.tag") || inherits(view_item, "shiny.tag.list"))

    title_person <- make_modal_title(list(shiny_table_name = "person", person_id = 1))
    expect_true(is.character(title_person))

    title_visit <- make_modal_title(list(
        shiny_table_name = "visit_occurrence",
        person_id = 1,
        visit_occurrence_id = 2
    ))
    expect_true(inherits(title_visit, "html") || inherits(title_visit, "shiny.tag"))
})

test_that("modal server registers output", {
    con <- mockCon()
    params <- make_test_params(con, "person")

    shiny::testServer(function(input, output, session) {
        register_server_modal(input, output, session, con, params)
    }, {
        person_id <- get_first_person_id(con)
        if (!is.na(person_id)) {
            meta <- pack_meta_info("person", person_id, person_id)
            session$setInputs(tbl_dblclick_meta = meta)
            session$flushReact()
            expect_silent(output$item_detail_list)
        } else {
            expect_silent(output$item_detail_list)
        }
    })
})
