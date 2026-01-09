
sql_date_types <- function(tbl) {
    lapply(as.data.frame(head(tbl, 1)), typeof)
}


sql_search <- function(tbl, cols, term){
    col_types <- sql_date_types(tbl)
    search_term <- paste0("%", term, "%")
    term_num <- toNum(term)
    like_exprs <- lapply(cols, function(col) {
        sym_col <- rlang::sym(col)
        ctype <- col_types[[col]]
        if (!is.null(ctype) && ctype %in% c("character", "logical")) {
            rlang::expr(!!sym_col %like% !!search_term)
        } else if (!is.null(ctype) && ctype %in% c("double", "integer", "numeric") && !is.na(term_num)) {
            rlang::expr(!!sym_col == !!term_num)
        } else {
            rlang::expr(as.character(!!sym_col) %like% !!search_term)
        }
    })

    combined_like <- Reduce(\(x, y) rlang::expr((!!x) | (!!y)), like_exprs)
    tbl |> filter(!!combined_like)
}


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