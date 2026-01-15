source("llm/implementation.r")

path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"
con <- duckdbCon(path)

table_name <- "person"
tbl_all_cols <- colnames(con$person |> head(0) |> collect())

query <- concept_id_to_concept_name(
    query = con$person,
    con = con,
    table_name = table_name,
    tbl_all_cols = tbl_all_cols
)

result <- query |>
    collect() |>
    as_tibble()

person_df <- con$person |>
    collect() |>
    as_tibble()

concept_df <- con$concept |>
    select(concept_id, concept_name) |>
    collect() |>
    as_tibble()

concept_map <- setNames(concept_df$concept_name, concept_df$concept_id)

baseline <- person_df |>
    mutate(
        shiny_gender_concept_id = if_else(
            is.na(gender_concept_id) | gender_concept_id == 0,
            paste0(coalesce(gender_source_value, ""), " (0)"),
            if_else(
                is.na(concept_map[as.character(gender_concept_id)]),
                NA_character_,
                paste0(concept_map[as.character(gender_concept_id)], " (", gender_concept_id, ")")
            )
        ),
        shiny_race_concept_id = if_else(
            is.na(race_concept_id) | race_concept_id == 0,
            paste0(coalesce(race_source_value, ""), " (0)"),
            if_else(
                is.na(concept_map[as.character(race_concept_id)]),
                NA_character_,
                paste0(concept_map[as.character(race_concept_id)], " (", race_concept_id, ")")
            )
        ),
        shiny_ethnicity_concept_id = if_else(
            is.na(ethnicity_concept_id) | ethnicity_concept_id == 0,
            paste0(coalesce(ethnicity_source_value, ""), " (0)"),
            if_else(
                is.na(concept_map[as.character(ethnicity_concept_id)]),
                NA_character_,
                paste0(concept_map[as.character(ethnicity_concept_id)], " (", ethnicity_concept_id, ")")
            )
        ),
        shiny_gender_source_concept_id = if_else(
            is.na(gender_source_concept_id) |
                is.na(concept_map[as.character(gender_source_concept_id)]),
            NA_character_,
            paste0(
                concept_map[as.character(gender_source_concept_id)],
                " (",
                gender_source_concept_id,
                ")"
            )
        ),
        shiny_race_source_concept_id = if_else(
            is.na(race_source_concept_id) |
                is.na(concept_map[as.character(race_source_concept_id)]),
            NA_character_,
            paste0(
                concept_map[as.character(race_source_concept_id)],
                " (",
                race_source_concept_id,
                ")"
            )
        ),
        shiny_ethnicity_source_concept_id = if_else(
            is.na(ethnicity_source_concept_id) |
                is.na(concept_map[as.character(ethnicity_source_concept_id)]),
            NA_character_,
            paste0(
                concept_map[as.character(ethnicity_source_concept_id)],
                " (",
                ethnicity_source_concept_id,
                ")"
            )
        )
    )

if (!identical(result, baseline)) {
    stop("concept_id_to_concept_name mismatch for person table.")
} else {
    print("concept_id_to_concept_name validation passed for person table.")
}
