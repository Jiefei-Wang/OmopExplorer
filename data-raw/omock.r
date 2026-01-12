library(omopgenerics)
library(omock)
library(dplyr)

# Create mock CDM object
datasetName <- "GiBleed"
cdm <- mockCdmFromDataset(datasetName = datasetName)

# Collect all tables into a list of data.frames
mock_cdm <- lapply(names(cdm), function(tbl) {
  cdm[[tbl]] |> collect() |> as.data.frame()
})
names(mock_cdm) <- names(cdm)

patient_tables <- c(
  "person", "observation_period", "visit_occurrence", "visit_detail",
  "condition_occurrence", "drug_exposure", "procedure_occurrence",
  "device_exposure", "measurement", "observation", "death",
  "specimen", "payer_plan_period", "cost", "drug_era",
  "dose_era", "condition_era", "note", "note_nlp",
  "episode", "episode_event", "cohort"
)

keep_tables <- intersect(names(mock_cdm), c(patient_tables, "concept", "concept_ancestor"))
mock_cdm <- mock_cdm[keep_tables]

person_sample <- integer(0)
if ("person" %in% names(mock_cdm)) {
  person_ids_all <- unique(mock_cdm$person$person_id)
  person_n <- min(1000, length(person_ids_all))
  set.seed(1)
  person_sample <- sample(person_ids_all, size = person_n)
}

if (length(person_sample) > 0) {
  for (tbl in intersect(patient_tables, names(mock_cdm))) {
    df <- mock_cdm[[tbl]]
    if ("person_id" %in% names(df)) {
      mock_cdm[[tbl]] <- dplyr::filter(df, person_id %in% person_sample)
    }
  }
}

used_concepts <- c()
for (tbl in intersect(patient_tables, names(mock_cdm))) {
  df <- mock_cdm[[tbl]]
  concept_cols <- grep("_concept_id$", names(df), value = TRUE)
  if (length(concept_cols) > 0) {
    used_concepts <- c(used_concepts, unlist(df[concept_cols], use.names = FALSE))
  }
}
used_concepts <- unique(used_concepts)
used_concepts <- used_concepts[!is.na(used_concepts) & used_concepts != 0]

all_concepts <- used_concepts
if ("concept_ancestor" %in% names(mock_cdm) && length(used_concepts) > 0) {
  ca <- mock_cdm$concept_ancestor
  ancestor_ids <- unique(ca$ancestor_concept_id[ca$descendant_concept_id %in% used_concepts])
  all_concepts <- unique(c(all_concepts, ancestor_ids))
  mock_cdm$concept_ancestor <- dplyr::filter(
    ca,
    descendant_concept_id %in% all_concepts,
    ancestor_concept_id %in% all_concepts
  )
} else if ("concept_ancestor" %in% names(mock_cdm)) {
  mock_cdm$concept_ancestor <- mock_cdm$concept_ancestor[0, ]
}

if ("concept" %in% names(mock_cdm)) {
  mock_cdm$concept <- dplyr::filter(mock_cdm$concept, concept_id %in% all_concepts)
}

usethis::use_data(mock_cdm, overwrite = TRUE, internal = TRUE)
