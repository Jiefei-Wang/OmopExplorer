pack_meta_info <- function(table_name, person_id, row_id) {
    paste0(table_name, "|", person_id, "|", row_id)
}

unpack_meta_info <- function(meta_string) {
    parts <- strsplit(meta_string, "|", fixed = TRUE)[[1]]
    list(
        table_name = parts[1],
        person_id = as.numeric(parts[2]),
        row_id = as.numeric(parts[3])
    )
}
