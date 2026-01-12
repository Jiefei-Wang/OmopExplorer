test_that("topbar renders placeholder and patient info", {
    con <- mockCon()
    params <- make_test_params(con, "person")

    shiny::testServer(function(input, output, session) {
        register_server_topbar(input, output, session, con, params)
    }, {
        session$flushReact()
        expect_silent(output$patient_topbar)

        person_id <- get_first_person_id(con)
        if (!is.na(person_id)) {
            params$target_person_id(person_id)
            session$flushReact()
            expect_silent(output$patient_topbar)
        }
    })
})
