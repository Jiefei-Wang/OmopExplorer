test_that("filter_data matches numeric and character filters", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(
        con,
        "condition_occurrence",
        c("person_id", "condition_source_value"),
        c("1", "2724")
    )

    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(
            person_id == 1,
            stringr::str_like(tolower(condition_source_value), tolower("%2724%"))
        ) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data ignores invalid numeric search values", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "person_id", "abc")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data parses year range for date columns", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "condition_start_date", "2020~2021")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(
            condition_start_date >= "2020-01-01",
            condition_start_date <= "2021-12-31"
        ) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data parses numeric range for numeric columns", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "person_id", "12~13")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(person_id >= 12, person_id <= 13) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data parses numeric inequality operators", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params_gt <- search_deparser(con, "condition_occurrence", "person_id", ">123")
    res_gt <- filter_data(con[["condition_occurrence"]], show_columns, params_gt)
    expected_gt <- con[["condition_occurrence"]] |>
        dplyr::filter(person_id > 123) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res_gt, expected_gt)

    params_lte <- search_deparser(con, "condition_occurrence", "person_id", "<= 123")
    res_lte <- filter_data(con[["condition_occurrence"]], show_columns, params_lte)
    expected_lte <- con[["condition_occurrence"]] |>
        dplyr::filter(person_id <= 123) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res_lte, expected_lte)
})

test_that("filter_data parses comma-separated numeric values", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "person_id", "1,2,3")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(person_id %in% c(1, 2, 3)) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data mirrors concept id searches to concept name", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "condition_concept_id", "123")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(condition_concept_id == 123) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data falls back to source value for non-numeric concept search", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "condition_concept_id", "abc")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(stringr::str_like(tolower(condition_source_value), tolower("%abc%"))) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data expands global search into OR filters", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "search_anything", "2724")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(
            condition_occurrence_id == 2724 |
                person_id == 2724 |
                condition_concept_id == 2724 |
                (condition_start_date >= "2724-01-01" &
                    condition_start_date <= "2724-12-31") |
                (condition_start_datetime >= "2724-01-01 00:00:00" &
                    condition_start_datetime <= "2724-12-31 23:59:59") |
                (condition_end_date >= "2724-01-01" &
                    condition_end_date <= "2724-12-31") |
                (condition_end_datetime >= "2724-01-01 00:00:00" &
                    condition_end_datetime <= "2724-12-31 23:59:59") |
                condition_type_concept_id == 2724 |
                condition_status_concept_id == 2724 |
                stringr::str_like(tolower(stop_reason), tolower("%2724%")) |
                provider_id == 2724 |
                visit_occurrence_id == 2724 |
                visit_detail_id == 2724 |
                stringr::str_like(tolower(condition_source_value), tolower("%2724%")) |
                condition_source_concept_id == 2724 |
                stringr::str_like(tolower(condition_status_source_value), tolower("%2724%"))
        ) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})

test_that("filter_data parses partial datetime for datetime columns", {
    con <- mockCon()
    show_columns <- c(
        "person_id",
        "condition_concept_id",
        "condition_source_value",
        "condition_status_source_value",
        "stop_reason",
        "condition_start_date",
        "condition_start_datetime"
    )

    params <- search_deparser(con, "condition_occurrence", "condition_start_datetime", "2021-06")
    res <- filter_data(con[["condition_occurrence"]], show_columns, params)
    expected <- con[["condition_occurrence"]] |>
        dplyr::filter(
            condition_start_datetime >= "2021-06-01 00:00:00",
            condition_start_datetime <= "2021-06-30 23:59:59"
        ) |>
        dplyr::select(dplyr::all_of(show_columns)) |>
        dplyr::collect()

    expect_equal(res, expected)
})
