
omop <- local({
  config_path <- system.file("config/omop.yaml", package = "OmopExplorer")
  omop_table_config <- yaml::read_yaml(config_path)
  
  result <- list()
  for (table_name in names(omop_table_config)){
    result[[table_name]] <- bind_rows(omop_table_config[[table_name]])
  }
  result
})

omop_concept_id_columns <- local({
  result <- list()
  for (table_name in names(omop)) {
    table_df <- omop[[table_name]]
    
    # Find columns ending in _concept_id
    concept_id_cols <- table_df|>
        filter(foreign_table == "concept") |>
        pull(column)
    
    result[[table_name]] <- concept_id_cols
  }
  result
})

# map concept_id columns to their source value columns
omop_concept_id_source_value_map <- local({
  result <- list()
  for (table_name in names(omop)) {
    table_df <- omop[[table_name]]
    
    # Find columns ending in _concept_id

    if (!"source_name_column" %in% colnames(table_df)) {
      next
    }
    concept_id_cols <- table_df|>
    filter(foreign_table == "concept") |>
    filter(!is.na(source_name_column)) |>
    pull(source_name_column,column)|>
    as.list()

    result[[table_name]] <- concept_id_cols
  }
  result
})

omop_key_columns <- local({
  result <- list()
  for (table_name in names(omop)) {
    table_df <- omop[[table_name]]
    
    key_col <- table_df |>
      filter(primary_key) |>
      pull(column)
    
    if (length(key_col) == 1) {
      result[[table_name]] <- key_col
    }
  }
  result
})



omop_key_to_table <- list(
    person_id = "person",
    observation_period_id = "observation_period",
    visit_occurrence_id = "visit_occurrence",
    visit_detail_id = "visit_detail",
    condition_occurrence_id = "condition_occurrence",
    drug_exposure_id = "drug_exposure",
    procedure_occurrence_id = "procedure_occurrence",
    device_exposure_id = "device_exposure",
    measurement_id = "measurement",
    observation_id = "observation",
    note_id = "note",
    note_nlp_id = "note_nlp",
    specimen_id = "specimen",
    provider_id = "provider",
    care_site_id = "care_site"
)



omop_panes <- list(
  person = list(
    display_name = "Person",
    show_columns = c("person_id", "birth_datetime", "gender_concept_id", "race_concept_id", "ethnicity_concept_id")
  ),
  visit_occurrence = list(
    display_name = "Visit",
    show_columns = c("person_id", "visit_occurrence_id", "visit_concept_id", "visit_start_date", "visit_end_date")
  ),
  condition_occurrence = list(
    display_name = "Condition",
    show_columns = c("person_id", "condition_occurrence_id", "condition_concept_id", "condition_start_date", "condition_end_date")
  ),
  procedure_occurrence = list(
    display_name = "Procedure",
    show_columns = c("person_id", "procedure_occurrence_id", "procedure_concept_id", "procedure_date")
  ),
  measurement = list(
    display_name = "Measurement",
    show_columns = c("person_id", "measurement_id", "measurement_date", "measurement_concept_id", "value_as_number", "unit_concept_id", "value_as_concept_id")
  ),
  drug_exposure = list(
    display_name = "Drug",
    show_columns = c("person_id", "drug_exposure_id", "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date", "quantity")
  ),
  note = list(
    display_name = "Note",
    show_columns = c("person_id", "note_id", "note_date", "note_type_concept_id", "note_title", "note_text_preview")
  ),
  death = list(
    display_name = "Death",
    show_columns = c("person_id", "death_date", "death_type_concept_id", "cause_concept_id")
  ),
  provider = list(
    display_name = "Provider",
    show_columns = c("provider_id", "provider_name", "specialty_concept_id")
  ),
  care_site = list(
    display_name = "Care Site",
    show_columns = c("care_site_id", "care_site_name", "place_of_service_concept_id")
  )
)

for (i in names(omop_panes)) {
  omop_panes[[i]]$table_name <- i
}


