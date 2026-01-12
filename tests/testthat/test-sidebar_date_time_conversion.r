# Test Date Column - Single Values (Various Formats)
test_that("date column - ISO format", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021-05-15", "Date")
    expect_equal(rlang::expr_text(res), 'date_col == "2021-05-15"')
})

test_that("date column - US format", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "05/15/2021", "Date")
    expect_equal(rlang::expr_text(res), 'date_col == "2021-05-15"')
})

test_that("date column - European format", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "15/05/2021", "Date")
    expect_equal(rlang::expr_text(res), 'date_col == "2021-05-15"')
})

# Test Date Column - Partial Dates (expand to ranges)
test_that("date column - year only expands to full year range", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-01-01" & date_col <= "2021-12-31")'
    )
})

test_that("date column - year-month expands to full month range", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021-05", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-05-01" & date_col <= "2021-05-31")'
    )
})

test_that("date column - month/year format expands to full month", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "08/2021", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-08-01" & date_col <= "2021-08-31")'
    )
})

# Test Date Column - Ranges with ~
test_that("date column - year range", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021~2022", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-01-01" & date_col <= "2022-12-31")'
    )
})

test_that("date column - full date range", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021-01-15~2021-12-31", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-01-15" & date_col <= "2021-12-31")'
    )
})

test_that("date column - mixed precision range", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021-01~2021-03-15", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-01-01" & date_col <= "2021-03-15")'
    )
})

# Test Date Column - Inequality Operators
test_that("date column - greater than", {
    res <- build_date_filter_expr(rlang::sym("date_col"), ">2021-05-15", "Date")
    expect_equal(rlang::expr_text(res), 'date_col > "2021-05-15"')
})

test_that("date column - greater than or equal", {
    res <- build_date_filter_expr(rlang::sym("date_col"), ">=2021-05-15", "Date")
    expect_equal(rlang::expr_text(res), 'date_col >= "2021-05-15"')
})

test_that("date column - less than", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "<2021-05-15", "Date")
    expect_equal(rlang::expr_text(res), 'date_col < "2021-05-15"')
})

test_that("date column - less than or equal", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "<=2021-05-15", "Date")
    expect_equal(rlang::expr_text(res), 'date_col <= "2021-05-15"')
})

test_that("date column - inequality with year", {
    res <- build_date_filter_expr(rlang::sym("date_col"), ">=2021", "Date")
    expect_equal(rlang::expr_text(res), 'date_col >= "2021-01-01"')
})

# Test DateTime Column - Single Values
test_that("datetime column - full datetime", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021-05-15 14:30:00", "POSIXct")
    expect_equal(rlang::expr_text(res), 'datetime_col == "2021-05-15 14:30:00"')
})

test_that("datetime column - date with time HH:MM", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021-05-15 14:30", "POSIXct")
    expect_equal(rlang::expr_text(res), 'datetime_col == "2021-05-15 14:30:00"')
})

test_that("datetime column - date only", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021-05-15", "POSIXct")
    expect_equal(rlang::expr_text(res), 'datetime_col == "2021-05-15 00:00:00"')
})

# Test DateTime Column - Partial Dates (expand to ranges with time)
test_that("datetime column - year only expands to full year with time", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021", "POSIXct")
    expect_equal(
        rlang::expr_text(res),
        '(datetime_col >= "2021-01-01 00:00:00" & datetime_col <= "2021-12-31 23:59:59")'
    )
})

test_that("datetime column - year-month expands to full month with time", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021-05", "POSIXct")
    expect_equal(
        rlang::expr_text(res),
        '(datetime_col >= "2021-05-01 00:00:00" & datetime_col <= "2021-05-31 23:59:59")'
    )
})

# Test DateTime Column - Ranges with ~
test_that("datetime column - year range", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021~2022", "POSIXct")
    expect_equal(
        rlang::expr_text(res),
        '(datetime_col >= "2021-01-01 00:00:00" & datetime_col <= "2022-12-31 23:59:59")'
    )
})

test_that("datetime column - full datetime range", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "2021-05-15 10:00:00~2021-05-15 18:00:00", "POSIXct")
    expect_equal(
        rlang::expr_text(res),
        '(datetime_col >= "2021-05-15 10:00:00" & datetime_col <= "2021-05-15 18:00:00")'
    )
})

# Test DateTime Column - Inequality Operators
test_that("datetime column - greater than with time", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), ">2021-05-15 14:30:00", "POSIXct")
    expect_equal(rlang::expr_text(res), 'datetime_col > "2021-05-15 14:30:00"')
})

test_that("datetime column - less than or equal with time", {
    res <- build_date_filter_expr(rlang::sym("datetime_col"), "<=2021-05-15 14:30:00", "POSIXct")
    expect_equal(rlang::expr_text(res), 'datetime_col <= "2021-05-15 14:30:00"')
})

# Test Edge Cases
test_that("date column - February leap year", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2020-02", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2020-02-01" & date_col <= "2020-02-29")'
    )
})

test_that("date column - February non-leap year", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "2021-02", "Date")
    expect_equal(
        rlang::expr_text(res),
        '(date_col >= "2021-02-01" & date_col <= "2021-02-28")'
    )
})

test_that("date column - invalid date returns NULL", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "invalid-date", "Date")
    expect_null(res)
})

test_that("date column - empty string returns NULL", {
    res <- build_date_filter_expr(rlang::sym("date_col"), "", "Date")
    expect_null(res)
})