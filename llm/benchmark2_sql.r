suppressPackageStartupMessages({
    library(DBI)
})
source("llm/utils.r")
path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"
con <- duckdbCon(path)

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

  WHERE
    m.value_as_number IS NOT NULL
    AND m.value_as_number > 0.2
) AS q

WHERE
  q.measurement_concept_id ILIKE '%psa%'

ORDER BY q.measurement_concept_id ASC
LIMIT 9001 OFFSET 999;
"

res <- system.time({
    for (i in 1:10) {
        DBI::dbGetQuery(con$dbcon, sql)
    }
})

print("sql2 code time:")
print(res)
