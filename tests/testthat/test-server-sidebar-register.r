test_that("sidebar search values update and clear", {
    con <- mockCon()
    params <- make_test_params(con, "person")
    table_cols <- params$table_info[["person"]]$columns
    target_col <- table_cols[[1]]
    input_id <- sidebar_search_input_id(target_col)

    shiny::testServer(function(input, output, session) {
        register_server_sidebar(input, output, session, con, params)
    }, {
        session$setInputs(sidebar_search_anything = "abc")
        session$flushReact()
        expect_equal(params$sidebar_search_values_internal$shiny_search_anything, "abc")

        do.call(session$setInputs, setNames(list("val"), input_id))
        session$flushReact()
        expect_equal(params$sidebar_search_values_internal[[target_col]], "val")

        session$setInputs(sidebar_clear_all = 1)
        session$flushReact()
        expect_equal(params$sidebar_search_values_internal$shiny_search_anything, "")
        expect_equal(params$sidebar_search_values_internal[[target_col]], "")

        expect_silent(output$sidebar_column_filters)
    })
})
