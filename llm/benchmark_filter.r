suppressPackageStartupMessages({
    library(DBI)
    library(dplyr)
})

source("llm/utils.r")
source("llm/implementation.r")
path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"
con <- duckdbCon(path)
measurement <- con$measurement
table_name <- "measurement"
tbl_all_cols <- colnames(collect(head(measurement, 0)))
query <- concept_id_to_concept_name(
    query = con$measurement,
    con = con,
    table_name = table_name,
    tbl_all_cols = tbl_all_cols)
show_columns <- c(
    "person_id",
    "measurement_id",
    "measurement_date",
    "shiny_measurement_concept_id",
    "value_as_number",
    "shiny_unit_concept_id",
    "shiny_value_as_concept_id"
)
# [1] "person_id"              "measurement_id"         "measurement_date"
# [4] "measurement_concept_id" "value_as_number"        "unit_concept_id"
# [7] "value_as_concept_id"
params_search <- list(
    list(
        relation = "AND",
        items = list(
            list(var = "shiny_measurement_concept_id", operator = "ILIKE", value = "psa")
        )
    )
)

params_order <- list(
    measurement_id = FALSE
)

timer <- system.time({
    for(i in 1:10){
        filter_res <- filter_data(
            query,
            show_columns,
            params_search,
            params_order,
            row_start = 10,
            row_length = 20
        )|>collect()
    }
})
print("filter code time:")
print(timer)


sql <- "
SELECT
  q.person_id,
  q.measurement_id,
  q.measurement_date,
  q.measurement_concept_id AS shiny_measurement_concept_id,
  q.value_as_number,
  q.unit_concept_id AS shiny_unit_concept_id,
  q.value_as_concept_id AS shiny_value_as_concept_id
FROM (
  SELECT
    m.person_id,
    m.measurement_id,
    m.measurement_date,

    CASE
      WHEN m.measurement_concept_id IS NULL
        OR m.measurement_concept_id = 0
        OR c_meas.concept_name IS NULL
      THEN CONCAT(m.measurement_source_value, ' (',
                  CAST(COALESCE(m.measurement_concept_id, 0) AS VARCHAR), ')')
      ELSE CONCAT(c_meas.concept_name, ' (',
                  CAST(m.measurement_concept_id AS VARCHAR), ')')
    END AS measurement_concept_id,

    m.value_as_number,

    CASE
      WHEN m.unit_concept_id IS NULL
        OR m.unit_concept_id = 0
        OR c_unit.concept_name IS NULL
      THEN CONCAT(m.unit_source_value, ' (',
                  CAST(COALESCE(m.unit_concept_id, 0) AS VARCHAR), ')')
      ELSE CONCAT(c_unit.concept_name, ' (',
                  CAST(m.unit_concept_id AS VARCHAR), ')')
    END AS unit_concept_id,

    CASE
      WHEN m.value_as_concept_id IS NULL
        OR m.value_as_concept_id = 0
        OR c_val.concept_name IS NULL
      THEN CONCAT(m.value_source_value, ' (',
                  CAST(COALESCE(m.value_as_concept_id, 0) AS VARCHAR), ')')
      ELSE CONCAT(c_val.concept_name, ' (',
                  CAST(m.value_as_concept_id AS VARCHAR), ')')
    END AS value_as_concept_id

  FROM measurement m
  LEFT JOIN concept c_meas ON c_meas.concept_id = m.measurement_concept_id
  LEFT JOIN concept c_unit ON c_unit.concept_id = m.unit_concept_id
  LEFT JOIN concept c_val  ON c_val.concept_id  = m.value_as_concept_id
) AS q

WHERE
  q.measurement_concept_id ILIKE '%psa%'

ORDER BY q.measurement_id DESC
LIMIT 20 OFFSET 10;
"
sql_res <- DBI::dbGetQuery(con$dbcon, sql)


testthat::expect_identical(as.data.frame(filter_res), sql_res)
