test_that("search_deparser builds numeric and character filters", {
    con <- mockCon()

    res <- search_deparser(
        con,
        "condition_occurrence",
        c("person_id", "condition_source_value"),
        c("1", "2724")
    )

    expected_result <- list(
        list(var = "person_id", operator = "=", value = 1),
        list(var = "condition_source_value", operator = "ILIKE", value = "2724")
    )
    expect_equal(res, expected_result)
})

test_that("search_deparser ignores invalid numeric search values", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "person_id", "abc")
    expected_result <- list()
    expect_equal(res, expected_result)
})

test_that("search_deparser parses year range for date columns", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "condition_start_date", "2020~2021")
    expected <- list(
        list(
            var = "condition_start_date",
            operator = "BETWEEN",
            value = c("2020-01-01", "2021-12-31")
        )
    )
    expect_equal(res, expected)
})

test_that("search_deparser parses numeric range for numeric columns", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "person_id", "12~13")
    expected <- list(
        list(
            var = "person_id",
            operator = "BETWEEN",
            value = c(12, 13)
        )
    )
    expect_equal(res, expected)
})

test_that("search_deparser parses numeric inequality operators", {
    con <- mockCon()

    res_gt <- search_deparser(con, "condition_occurrence", "person_id", ">123")
    expected_gt <- list(
        list(
            var = "person_id",
            operator = ">",
            value = 123
        )
    )
    expect_equal(res_gt, expected_gt)

    res_lte <- search_deparser(con, "condition_occurrence", "person_id", "<= 123")
    expected_lte <- list(
        list(
            var = "person_id",
            operator = "<=",
            value = 123
        )
    )
    expect_equal(res_lte, expected_lte)
})

test_that("search_deparser parses comma-separated numeric values", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "person_id", "1,2,3")
    expected_result <- list(
        list(
            relation = "OR",
            items = list(
                list(var = "person_id", operator = "=", value = 1),
                list(var = "person_id", operator = "=", value = 2),
                list(var = "person_id", operator = "=", value = 3)
            )
        )
    )
    expect_equal(res, expected_result)
})

test_that("search_deparser mirrors concept id searches to concept name", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "condition_concept_id", "123")
    expected_result <- list(
        list(var = "condition_concept_id", operator = "=", value = 123)
    )
    expect_equal(res, expected_result)
})

test_that("search_deparser falls back to source value for non-numeric concept search", {
    con <- mockCon()
    res <- search_deparser(con, "condition_occurrence", "condition_concept_id", "abc")
    expected_result <- list(
        list(var = "condition_source_value", operator = "ILIKE", value = "abc")
    )
    expect_equal(res, expected_result)
})

test_that("search_deparser expands global search into OR filters", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "search_anything", "2724")
    expected_result <- list(list(
        relation = "OR", 
        items = list(
            list(var = "condition_occurrence_id", operator = "=", value = 2724),
            list(var = "person_id", operator = "=", value = 2724),
            list(var = "condition_concept_id", operator = "=", value = 2724),
            list(
                var = "condition_start_date",
                operator = "BETWEEN",
                value = c("2724-01-01", "2724-12-31")
            ),
            list(
                var = "condition_start_datetime",
                operator = "BETWEEN",
                value = c("2724-01-01 00:00:00", "2724-12-31 23:59:59")
            ),
            list(
                var = "condition_end_date",
                operator = "BETWEEN",
                value = c("2724-01-01", "2724-12-31")
            ),
            list(
                var = "condition_end_datetime",
                operator = "BETWEEN",
                value = c("2724-01-01 00:00:00", "2724-12-31 23:59:59")
            ),
            list(var = "condition_type_concept_id", operator = "=", value = 2724),
            list(var = "condition_status_concept_id", operator = "=", value = 2724),
            list(var = "stop_reason", operator = "ILIKE", value = "2724"),
            list(var = "provider_id", operator = "=", value = 2724),
            list(var = "visit_occurrence_id", operator = "=", value = 2724),
            list(var = "visit_detail_id", operator = "=", value = 2724),
            list(var = "condition_source_value", operator = "ILIKE", value = "2724"),
            list(var = "condition_source_concept_id", operator = "=", value = 2724),
            list(var = "condition_status_source_value", operator = "ILIKE", value = "2724")
            )
    ))
    expect_equal(res, expected_result)
})

test_that("search_deparser parses partial datetime for datetime columns", {
    con <- mockCon()

    res <- search_deparser(con, "condition_occurrence", "condition_start_datetime", "2021-06")
    expected_result <- list(
        list(
            var = "condition_start_datetime",
            operator = "BETWEEN",
            value = c("2021-06-01 00:00:00", "2021-06-30 23:59:59")
        )
    )
    expect_equal(res, expected_result)
})
