
config_path <- system.file("config/omop.yaml", package = "OmopExplorer")
omop_table_config <- yaml::read_yaml(config_path)

omop <- list()
for (table_name in names(omop_table_config)){
    omop[[table_name]] <- bind_rows(omop_table_config[[table_name]])
}



concept_id_source_value_map <- list(
  # PERSON
  gender_concept_id = "gender_source_value",
  race_concept_id = "race_source_value",
  ethnicity_concept_id = "ethnicity_source_value",

  # OBSERVATION_PERIOD
  period_type_concept_id = NULL,

  # VISIT_OCCURRENCE
  visit_concept_id = "visit_source_value",
  visit_type_concept_id = NULL,
  admitted_from_concept_id = "admitted_from_source_value",
  discharged_to_concept_id = "discharged_to_source_value",

  # VISIT_DETAIL
  visit_detail_concept_id = "visit_detail_source_value",
  visit_detail_type_concept_id = NULL,
  # (admitted/discharged in Detail inherit names from Visit)
  admitted_from_concept_id = "admitted_from_source_value",
  discharged_to_concept_id = "discharged_to_source_value",

  # CONDITION_OCCURRENCE
  condition_concept_id = "condition_source_value",
  condition_type_concept_id = NULL,
  condition_status_concept_id = "condition_status_source_value",

  # DRUG_EXPOSURE
  drug_concept_id = "drug_source_value",
  drug_type_concept_id = NULL,
  route_concept_id = "route_source_value",

  # PROCEDURE_OCCURRENCE
  procedure_concept_id = "procedure_source_value",
  procedure_type_concept_id = NULL,
  modifier_concept_id = "modifier_source_value",

  # DEVICE_EXPOSURE
  device_concept_id = "device_source_value",
  device_type_concept_id = NULL,
  unit_concept_id = "unit_source_value",

  # MEASUREMENT
  measurement_concept_id = "measurement_source_value",
  measurement_type_concept_id = NULL,
  operator_concept_id = NULL,
  value_as_concept_id = "value_source_value",
  unit_concept_id = "unit_source_value",
  meas_event_field_concept_id = NULL,

  # OBSERVATION
  observation_concept_id = "observation_source_value",
  observation_type_concept_id = NULL,
  value_as_concept_id = "value_source_value",
  qualifier_concept_id = "qualifier_source_value",
  unit_concept_id = "unit_source_value",
  obs_event_field_concept_id = NULL,

  # DEATH
  death_type_concept_id = NULL,
  cause_concept_id = "cause_source_value",

  # NOTE
  note_type_concept_id = NULL,
  note_class_concept_id = "note_source_value",
  encoding_concept_id = NULL,
  language_concept_id = NULL,
  note_event_field_concept_id = NULL,

  # NOTE_NLP
  section_concept_id = NULL,
  note_nlp_concept_id = NULL,

  # SPECIMEN
  specimen_concept_id = "specimen_source_value",
  specimen_type_concept_id = NULL,
  unit_concept_id = "unit_source_value",
  anatomic_site_concept_id = "anatomic_site_source_value",
  disease_status_concept_id = "disease_status_source_value",

  # FACT_RELATIONSHIP
  domain_concept_id_1 = NULL,
  domain_concept_id_2 = NULL,
  relationship_concept_id = NULL,

  # LOCATION
  country_concept_id = "country_source_value",

  # CARE_SITE
  place_of_service_concept_id = "place_of_service_source_value",

  # PROVIDER
  specialty_concept_id = "specialty_source_value",
  gender_concept_id = "gender_source_value",

  # PAYER_PLAN_PERIOD
  payer_concept_id = "payer_source_value",
  plan_concept_id = "plan_source_value",
  sponsor_concept_id = "sponsor_source_value",
  stop_reason_concept_id = "stop_reason_source_value",

  # COST
  cost_type_concept_id = NULL,
  currency_concept_id = NULL,
  revenue_code_concept_id = "revenue_code_source_value",
  drg_concept_id = "drg_source_value",

  # ERAS (DRUG, DOSE, CONDITION)
  # Era tables are derived and do not have source value columns
  drug_concept_id = NULL, 
  unit_concept_id = NULL,
  condition_concept_id = NULL,

  # EPISODE
  episode_concept_id = "episode_source_value",
  episode_object_concept_id = NULL,
  episode_type_concept_id = NULL,

  # EPISODE_EVENT
  episode_event_field_concept_id = NULL,

  # METADATA
  metadata_concept_id = NULL,
  metadata_type_concept_id = NULL,
  value_as_concept_id = NULL,

  # CDM_SOURCE
  cdm_version_concept_id = NULL
)



omop_key_columns <- list(
  person = "person_id",
  observation_period = "observation_period_id",
  visit_occurrence = "visit_occurrence_id",
  visit_detail = "visit_detail_id",
  condition_occurrence = "condition_occurrence_id",
  drug_exposure = "drug_exposure_id",
  procedure_occurrence = "procedure_occurrence_id",
  device_exposure = "device_exposure_id",
  measurement = "measurement_id",
  observation = "observation_id",
  death = "person_id",
  note = "note_id",
  note_nlp = "note_nlp_id",
  specimen = "specimen_id",
  provider = "provider_id",
  care_site = "care_site_id"
)

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


omop_show_columns <- list(
  person = c("person_id", "birth_date", "gender_concept_id", "race_concept_id", "ethnicity_concept_id"),

  condition_occurrence = c(
    "person_id",
    "condition_occurrence_id",
    "condition_concept_id",
    "condition_start_date",
    "condition_end_date"
  ),
  visit_occurrence = c(
    "person_id",
    "visit_occurrence_id",
    "visit_concept_id",
    "visit_start_date",
    "visit_end_date"
  ),
  procedure_occurrence = c(
    "person_id",
    "procedure_occurrence_id",
    "procedure_concept_id",
    "procedure_date"
  ),
  measurement = c(
    "person_id",
    "measurement_id",
    "measurement_date",
    "measurement_concept_id",
    "value_as_number",
    "unit_concept_id",
    "value_as_concept_id"
  ),
  drug_exposure = c(
    "person_id",
    "drug_exposure_id",
    "drug_concept_id",
    "drug_exposure_start_date",
    "drug_exposure_end_date",
    "quantity"
  ),
  note = c(
    "person_id",
    "note_id",
    "note_date",
    "note_type_concept_id",
    "note_title",
    "note_text_preview"
  ),
  death = c(
    "person_id",
    "death_date",
    "death_type_concept_id",
    "cause_concept_id"
  )
)

