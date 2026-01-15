suppressPackageStartupMessages({
    library(dplyr)
})

source("llm/utils.r")
source("llm/implementation.r")
path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"
con <- duckdbCon(path)
table_name <- "measurement"

query <- con$measurement

query <- concept_id_to_concept_name(
    query = query,
    con = con,
    table_name = table_name,
    tbl_all_cols = colnames(collect(head(query, 0)))
)

show_columns <- c(
    "person_id",
    "measurement_id",
    "measurement_date",
    "shiny_measurement_concept_id",
    "value_as_number",
    "shiny_unit_concept_id",
    "shiny_value_as_concept_id"
)
params_search <- list(
    list(var = "value_as_number", operator = ">", value = 0.2),
    list(var = "shiny_measurement_concept_id", operator = "ILIKE", value = "psa")
)

params_order <- list(
    measurement_concept_id = TRUE
)

res2 <- system.time({
    for (i in 1:10) {
        filter_data(
            query,
            show_columns,
            params_search,
            params_order,
            row_start = 999,
            row_length = 9001
        )|>collect()
    }
})
print("filter2 code time:")
print(res2)
