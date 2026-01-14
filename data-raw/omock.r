library(omopgenerics)
library(omock)
library(dplyr)
# Create mock CDM object
set.seed(1001)
find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  while (path != dirname(path)) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    path <- dirname(path)
  }
  getwd()
}

project_root <- find_project_root()
usethis::proj_set(project_root)
wd <- project_root
Sys.setenv(OMOP_DATA_FOLDER = file.path(project_root, "data-raw"))

datasetName <- "synpuf-1k_5.4"
cdm <- mockCdmFromDataset(datasetName = datasetName)


person_1 <- cdm$person |> anti_join(cdm$death, by = "person_id") |> head(10)
person_2 <- cdm$person |> semi_join(cdm$death, by = "person_id") |> head(11)
person <- bind_rows(person_1, person_2) |> collect()

visit_occurrence <- cdm$visit_occurrence |>
  semi_join(person, by = "person_id") |>
  group_by(person_id) |>
  arrange(stats::runif(dplyr::n())) |>
  slice_head(n = 2) |>
  ungroup() |>
  collect()

person_ids <- person$person_id
visit_ids <- visit_occurrence$visit_occurrence_id

filter_person <- function(tbl) {
  if (!"person_id" %in% names(tbl)) {
    return(tbl)
  }
  tbl |> filter(person_id %in% person_ids)
}

limit_by_visit <- function(tbl) {
  if (!"visit_occurrence_id" %in% names(tbl)) {
    return(tbl)
  }
  tbl |>
    filter(visit_occurrence_id %in% visit_ids) |>
    group_by(visit_occurrence_id) |>
    arrange(stats::runif(dplyr::n())) |>
    slice_head(n = 2) |>
    ungroup()
}

tables <- c(
  "care_site",
  "cdm_source",
  "concept",
  "concept_ancestor",
  "concept_class",
  "concept_relationship",
  "concept_synonym",
  "condition_era",
  "condition_occurrence",
  "death",
  "device_exposure",
  "domain",
  "drug_era",
  "drug_exposure",
  "drug_strength",
  "location",
  "measurement",
  "observation",
  "observation_period",
  "payer_plan_period",
  "person",
  "procedure_occurrence",
  "provider",
  "relationship",
  "visit_occurrence",
  "vocabulary"
)


mock_cdm <- list()
for (tbl_name in tables) {
  tbl <- cdm[[tbl_name]]

  if (tbl_name == "person") {
    tbl <- person
  } else if (tbl_name == "visit_occurrence") {
    tbl <- visit_occurrence
  } else {
    tbl <- tbl |> filter_person() |> limit_by_visit() |> collect()
  }

  mock_cdm[[tbl_name]] <- tbl
}

vocab_tables <- c(
  "concept",
  "concept_relationship",
  "concept_ancestor",
  "concept_synonym",
  "concept_class",
  "domain",
  "vocabulary",
  "relationship",
  "drug_strength"
)

concept_source_tables <- setdiff(names(mock_cdm), vocab_tables)
concept_ids <- unique(unlist(lapply(concept_source_tables, function(tbl_name) {
  tbl <- mock_cdm[[tbl_name]]
  cols <- grep("_concept_id$", names(tbl), value = TRUE)
  if (length(cols) == 0) {
    return(integer())
  }
  vals <- unlist(tbl[cols], use.names = FALSE)
  vals <- vals[!is.na(vals) & vals != 0]
  as.integer(vals)
})))

if ("concept" %in% names(mock_cdm)) {
  mock_cdm$concept <- mock_cdm$concept |>
    filter(concept_id %in% concept_ids)
}

if ("concept_relationship" %in% names(mock_cdm)) {
  mock_cdm$concept_relationship <- mock_cdm$concept_relationship |>
    filter(concept_id_1 %in% concept_ids, concept_id_2 %in% concept_ids)
}

if ("concept_ancestor" %in% names(mock_cdm)) {
  mock_cdm$concept_ancestor <- mock_cdm$concept_ancestor |>
    filter(ancestor_concept_id %in% concept_ids, descendant_concept_id %in% concept_ids)
}

if ("concept_synonym" %in% names(mock_cdm)) {
  mock_cdm$concept_synonym <- mock_cdm$concept_synonym |>
    filter(concept_id %in% concept_ids)
}

if ("drug_strength" %in% names(mock_cdm)) {
  mock_cdm$drug_strength <- mock_cdm$drug_strength |>
    filter(drug_concept_id %in% concept_ids | ingredient_concept_id %in% concept_ids)
}

if ("concept_class" %in% names(mock_cdm) && "concept" %in% names(mock_cdm)) {
  used_concept_class <- unique(mock_cdm$concept$concept_class_id)
  mock_cdm$concept_class <- mock_cdm$concept_class |>
    filter(concept_class_id %in% used_concept_class)
}

if ("domain" %in% names(mock_cdm) && "concept" %in% names(mock_cdm)) {
  used_domain <- unique(mock_cdm$concept$domain_id)
  mock_cdm$domain <- mock_cdm$domain |>
    filter(domain_id %in% used_domain)
}

if ("vocabulary" %in% names(mock_cdm) && "concept" %in% names(mock_cdm)) {
  used_vocab <- unique(mock_cdm$concept$vocabulary_id)
  mock_cdm$vocabulary <- mock_cdm$vocabulary |>
    filter(vocabulary_id %in% used_vocab)
}

if ("relationship" %in% names(mock_cdm) && "concept_relationship" %in% names(mock_cdm)) {
  used_rel <- unique(mock_cdm$concept_relationship$relationship_id)
  mock_cdm$relationship <- mock_cdm$relationship |>
    filter(relationship_id %in% used_rel)
}

if ("care_site" %in% names(mock_cdm)) {
  used_care_site <- unique(c(person$care_site_id, visit_occurrence$care_site_id))
  used_care_site <- used_care_site[!is.na(used_care_site)]
  mock_cdm$care_site <- mock_cdm$care_site |>
    filter(care_site_id %in% used_care_site)
}

if ("provider" %in% names(mock_cdm)) {
  used_provider <- unique(c(person$provider_id, visit_occurrence$provider_id))
  used_provider <- used_provider[!is.na(used_provider)]
  mock_cdm$provider <- mock_cdm$provider |>
    filter(provider_id %in% used_provider)
}

if ("location" %in% names(mock_cdm)) {
  used_location <- person$location_id
  if ("care_site" %in% names(mock_cdm)) {
    used_location <- c(used_location, mock_cdm$care_site$location_id)
  }
  used_location <- unique(used_location)
  used_location <- used_location[!is.na(used_location)]
  mock_cdm$location <- mock_cdm$location |>
    filter(location_id %in% used_location)
}


usethis::use_data(mock_cdm, overwrite = TRUE, internal = TRUE)

# check the file size
paste0(file.info("R/sysdata.rda")$size/1024/1024, " MB")
