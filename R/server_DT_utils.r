
keep_exist_cols <- function(table_info, table_name, cols){
    if (table_name %in% names(table_info)){
        available_cols <- table_info[[table_name]]$columns
        intersect(cols, available_cols)
    } else {
        cols
    }
}