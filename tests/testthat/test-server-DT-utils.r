test_that("keep_exist_cols filters to available columns", {
    table_info <- list(
        person = list(columns = c("person_id", "gender_concept_id"))
    )

    res <- keep_exist_cols(table_info, "person", c("person_id", "age"))
    expect_equal(res, "person_id")
})

test_that("keep_exist_cols returns input when table missing", {
    table_info <- list()
    cols <- c("person_id", "age")
    res <- keep_exist_cols(table_info, "person", cols)
    expect_equal(res, cols)
})
