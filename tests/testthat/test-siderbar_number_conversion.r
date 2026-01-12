# Test Number Column - Single Values
test_that("number column - integer value", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "42")
    expect_equal(rlang::expr_text(res), "num_col == 42")
})

test_that("number column - decimal value", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "3.14")
    expect_equal(rlang::expr_text(res), "num_col == 3.14")
})

test_that("number column - negative value", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "-7")
    expect_equal(rlang::expr_text(res), "num_col == -7")
})

# Test Number Column - Ranges with ~
test_that("number column - range", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "1~5")
    expect_equal(
        rlang::expr_text(res),
        "(num_col >= 1 & num_col <= 5)"
    )
})

test_that("number column - range with whitespace", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "  1 ~  5 ")
    expect_equal(
        rlang::expr_text(res),
        "(num_col >= 1 & num_col <= 5)"
    )
})

# Test Number Column - Inequality Operators
test_that("number column - greater than", {
    res <- build_num_filter_expr(rlang::sym("num_col"), ">10")
    expect_equal(rlang::expr_text(res), "num_col > 10")
})

test_that("number column - greater than or equal", {
    res <- build_num_filter_expr(rlang::sym("num_col"), ">=10")
    expect_equal(rlang::expr_text(res), "num_col >= 10")
})

test_that("number column - less than", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "<10")
    expect_equal(rlang::expr_text(res), "num_col < 10")
})

test_that("number column - less than or equal", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "<=10")
    expect_equal(rlang::expr_text(res), "num_col <= 10")
})

# Test Edge Cases
test_that("number column - invalid value returns NULL", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "invalid-number")
    expect_null(res)
})

test_that("number column - invalid range returns NULL", {
    res <- build_num_filter_expr(rlang::sym("num_col"), "1~invalid")
    expect_null(res)
})
