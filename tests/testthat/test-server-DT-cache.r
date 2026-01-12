test_that("DT cache set/get works", {
    clear_global_DT_cache()

    cache_matrix <- matrix(1:6, nrow = 2)
    params_search <- list(shiny_search_anything = "abc")
    params_order <- list(list(column = "person_id", ascending = TRUE))

    set_cached_DT(
        table_name = "person",
        cache_start = 0,
        params_search = params_search,
        params_order = params_order,
        records_total = 100,
        records_filtered = 2,
        cache_matrix = cache_matrix
    )

    expect_true(is_DT_cache_available(
        "person",
        params_search,
        params_order,
        params_start = 0,
        params_len = 2
    ))

    data_out <- get_cached_DT("person", params_start = 0, params_len = 2)
    expect_equal(data_out$recordsTotal, 100)
    expect_equal(data_out$recordsFiltered, 2)
    expect_equal(nrow(data_out$cached_data), 2)
})

test_that("build_cache_DT returns matrix with records", {
    con <- mockCon()
    table_info <- build_table_info(con)[["person"]]
    show_columns <- keep_exist_cols(table_info, "person", omop_show_columns[["person"]])
    person_count <- table_info$table|>summarise(n = n())|>pull(n)
    cached_size <- min(person_count, cache_row_count)
    res <- build_cache_DT(
        con = con,
        table_info = table_info,
        table_name = "person",
        post_process_pipe = \(x) x,
        show_columns = show_columns,
        params_search = list(),
        params_order = list(),
        params_start = 0,
        params_len = 10
    )

    expect_true(is.matrix(res$cached_data))
    expect_true(res$recordsTotal >= res$recordsFiltered)
    expect_equal(nrow(res$cached_data), cached_size)
})
