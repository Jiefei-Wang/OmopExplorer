test_that("render_db_DT caches on unchanged search params", {
    con <- mockCon()
    params <- make_test_params(con, "person")
    clear_global_DT_cache()

    shiny::testServer(function(input, output, session) {}, {
        dt_render_1 <- render_db_DT(
            params = params,
            con = con,
            table_name = "person"
        )
        dt_render_2 <- render_db_DT(
            params = params,
            con = con,
            table_name = "person"
        )

        expect_true(is.function(dt_render_1))
        expect_identical(dt_render_1, dt_render_2)
    })
})

test_that("sql_dt_filter returns expected DT payload", {
    con <- mockCon()
    table_info <- build_table_info(con)[["person"]]
    show_columns <- keep_exist_cols(table_info, "person", omop_show_columns[["person"]])

    filter_fn <- sql_dt_filter(
        con = con,
        table_name = "person",
        post_process_pipe = \(x) x,
        table_info = table_info,
        show_columns = show_columns,
        params_search = list()
    )

    params_dt <- list(
        start = 0,
        length = 5,
        draw = 1,
        order = list(list(column = 0, dir = "asc")),
        columns = lapply(show_columns, function(col) list(name = col))
    )

    res <- filter_fn(NULL, params_dt)
    expect_true(all(c(
        "draw",
        "recordsTotal",
        "recordsFiltered",
        "data",
        "DT_rows_current"
    ) %in% names(res)))
})
