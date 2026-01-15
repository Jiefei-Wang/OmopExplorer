suppressPackageStartupMessages({
    library(DBI)
    library(duckdb)
    library(dplyr)
    library(dbplyr)
    library(tibble)
    library(yaml)
})

omop_tables <- c(
    "person", "measurement", "concept"
)

duckdbCon <- function(dbdir, read_only = TRUE, ...) {
    dbcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only, ...)
    available_tables <- intersect(omop_tables, DBI::dbListTables(dbcon))
    con <- list()
    for (i in available_tables) {
        con[[i]] <- dplyr::tbl(dbcon, i)
    }
    con$dbcon <- dbcon
    con
}

sql_date_types <- function(tbl) {
    lapply(as.data.frame(head(tbl, 1)), function(x) class(x)[1])
}

load_omop_config <- function(config_path = NULL) {
    if (is.null(config_path)) {
        candidate <- file.path(getwd(), "inst", "config", "omop.yaml")
        if (!file.exists(candidate)) {
            candidate <- file.path(getwd(), "config", "omop.yaml")
        }
        config_path <- candidate
    }
    if (!file.exists(config_path)) {
        stop("omop.yaml not found. Provide config_path.")
    }
    yaml::read_yaml(config_path)
}

omop_from_config <- function(omop_table_config) {
    result <- list()
    for (table_name in names(omop_table_config)) {
        result[[table_name]] <- dplyr::bind_rows(omop_table_config[[table_name]])
    }
    result
}

build_omop_concept_id_columns <- function(omop) {
    result <- list()
    for (table_name in names(omop)) {
        table_df <- omop[[table_name]]
        concept_id_cols <- table_df |>
            dplyr::filter(foreign_table == "concept") |>
            dplyr::pull(column)
        result[[table_name]] <- concept_id_cols
    }
    result
}

build_omop_concept_id_source_value_map <- function(omop) {
    result <- list()
    for (table_name in names(omop)) {
        table_df <- omop[[table_name]]
        if (!"source_name_column" %in% colnames(table_df)) {
            next
        }
        concept_id_cols <- table_df |>
            dplyr::filter(foreign_table == "concept") |>
            dplyr::filter(!is.na(source_name_column)) |>
            dplyr::pull(source_name_column, column) |>
            as.list()
        result[[table_name]] <- concept_id_cols
    }
    result
}

init_omop_mappings <- function(config_path = NULL) {
    omop_config <- load_omop_config(config_path)
    omop <- omop_from_config(omop_config)
    list(
        omop = omop,
        omop_concept_id_columns = build_omop_concept_id_columns(omop),
        omop_concept_id_source_value_map = build_omop_concept_id_source_value_map(omop)
    )
}

if (!exists("omop_concept_id_columns", inherits = FALSE) ||
    !exists("omop_concept_id_source_value_map", inherits = FALSE)) {
    mappings <- init_omop_mappings()
    omop_concept_id_columns <- mappings$omop_concept_id_columns
    omop_concept_id_source_value_map <- mappings$omop_concept_id_source_value_map
}

# a helper function to get column types
get_column_type <- function(query){
    tbl <- query |> head(0) |> collect()
    col_types <- sapply(tbl, class)
    col_types <- lapply(col_types, function(ct) ct[1])
    col_types
}
