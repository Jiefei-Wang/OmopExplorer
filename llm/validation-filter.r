suppressPackageStartupMessages({
    library(dplyr)
    library(glue)
})

source("llm/utils.r")
source("llm/implementation.r")

path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"
con <- duckdbCon(path)
table_name <- "person"
tbl_all_cols <- colnames(collect(head(con[[table_name]], 0)))

query <- concept_id_to_concept_name(
    query = con$person,
    con = con,
    table_name = table_name,
    tbl_all_cols = tbl_all_cols
)

show_columns <- c(
    "person_id",
    "birth_datetime",
    "shiny_gender_concept_id",
    "shiny_race_concept_id",
    "shiny_ethnicity_concept_id"
)

params_order <- list(
    person_id = TRUE,
    year_of_birth = TRUE
)

params_search <- list(
    list(var = "year_of_birth", operator = ">=", value = 1960),
    list(var = "year_of_birth", operator = "<=", value = 1970)
)

data <- query |>
    collect()

data_baseline <- data |>
    filter(
        year_of_birth >= 1960,
        year_of_birth <= 1970
    )
data_baseline <- data_baseline |>
    arrange(person_id, year_of_birth) |>
    select(all_of(show_columns))

print("test case 1 validation:")
chunk_size <- 50
if (nrow(data_baseline) == 0) {
    print("No rows found for test case 1.")
} else {
    for (i in seq(0, nrow(data_baseline) - 1, by = chunk_size)) {
        data_chunk <- filter_data(
            query,
            show_columns,
            params_search,
            params_order,
            i,
            chunk_size
        )|>collect()
        correct_size <- min(chunk_size, nrow(data_baseline) - i)
        baseline_chunk <- data_baseline[(i + 1):(i + correct_size), ]
        if (!identical(as.data.frame(data_chunk), as.data.frame(baseline_chunk))) {
            stop(glue::glue("Mismatch found at rows {i + 1} to {i + correct_size}"))
        } else {
            print(glue::glue("Rows {i + 1} to {i + correct_size} match."))
        }
    }
}












print("test case 2 validation:")

params_search <- list(
    list(
        relation = "AND",
        items = list(
            list(var = "year_of_birth", operator = ">=", value = 1960),
            list(var = "year_of_birth", operator = "<=", value = 1970)
        )
    ),
    list(
        relation = "OR",
        items = list(
            list(var = "shiny_race_concept_id", operator = "LIKE", value = "White"),
            list(var = "shiny_race_concept_id", operator = "LIKE", value = "Asian")
        )
    )
)


# Hard-coded search logic for this validation case
data_baseline <- data |>
    filter(
        year_of_birth >= 1960,
        year_of_birth <= 1970,
        grepl("white", tolower(shiny_race_concept_id)) |
            grepl("asian", tolower(shiny_race_concept_id))
    )

data_baseline <- data_baseline |>
    arrange(person_id, year_of_birth) |>
    select(all_of(show_columns))

chunk_size <- 50
if (nrow(data_baseline) == 0) {
    print("No rows found for test case 2.")
} else {
    for (i in seq(0, nrow(data_baseline) - 1, by = chunk_size)) {
        data_chunk <- filter_data(
            query,
            show_columns,
            params_search,
            params_order,
            i,
            chunk_size
        )|>collect()
        correct_size <- min(chunk_size, nrow(data_baseline) - i)
        baseline_chunk <- data_baseline[(i + 1):(i + correct_size), ]
        if (!identical(as.data.frame(data_chunk), as.data.frame(baseline_chunk))) {
            stop(glue::glue("Mismatch found at rows {i + 1} to {i + correct_size}"))
        } else {
            print(glue::glue("Rows {i + 1} to {i + correct_size} match."))
        }
    }
}









