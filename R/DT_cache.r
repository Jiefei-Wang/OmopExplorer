global_DT_space <- new.env()

cache_row_count <- 1000

is_DT_cache_available <- function(table_name, params_search, order_params, params_start, params_len){
    table_cache <- global_DT_space[[table_name]]
    if (is.null(table_cache)){
        return(FALSE)
    }
    dt_render_cache <- table_cache$dt_render_cache
    if (is.null(dt_render_cache)) {
        return(FALSE)
    }

    cache_start <- dt_render_cache$start
    cache_end <- cache_start + cache_row_count
    if (params_start >= cache_start &&
        (params_start + params_len) <= cache_end) {
            combined_condition <- list(params_search, order_params)
            if (identical(combined_condition, dt_render_cache$combined_condition)) {
                return(TRUE)
            }
        }
    return(FALSE)
}

#' @return list with elements: recordsTotal, recordsFiltered, cached_data
get_cached_DT <- function(table_name, params_start, params_len){
    table_cache <- global_DT_space[[table_name]]
    dt_render_cache <- table_cache$dt_render_cache
    cache_matrix <- dt_render_cache$cache_matrix
    cache_start <- params_start - dt_render_cache$start + 1
    cache_end <- min(params_start - dt_render_cache$start + params_len, nrow(cache_matrix))

    cached_data <- cache_matrix[
        cache_start:cache_end, , drop = FALSE]
    return(list(
        recordsTotal = dt_render_cache$recordsTotal,
        recordsFiltered = dt_render_cache$recordsFiltered,
        cached_data = cached_data
    ))
}

set_cached_DT <- function(table_name, cache_start, params_search, params_order, records_total, records_filtered, cache_matrix){
    table_cache <- global_DT_space[[table_name]]
    if (is.null(table_cache)){
        table_cache <- list()
    }
    dt_render_cache <- list(
        start = cache_start,
        combined_condition = list(params_search, params_order),
        recordsTotal = records_total,
        recordsFiltered = records_filtered,
        cache_matrix = cache_matrix
    )
    table_cache$dt_render_cache <- dt_render_cache
    global_DT_space[[table_name]] <- table_cache
}


build_cache_DT <- function(con, table_info, table_name, post_process_pipe, show_columns, params_search, params_order, params_start, params_len) {
    tbl <- table_info$table
    tbl_all_cols <- table_info$columns
    key_column <- table_info$key_column

    records_total <- if (length(tbl_all_cols) > 0) {
        as.numeric(tally(tbl) |> pull(1))
    } else {
        0
    }

    query <- tbl

    # search from the global filter
    search_anything <- params_search[["sidebar_search_anything"]]
    if (!is.null(search_anything) && search_anything != "") {
        query <- sql_search(con, query, table_name, tbl_all_cols, search_anything, table_info = table_info)
    }

    # for each individual column filter
    sidebar_filters <- params_search[names(params_search) != "sidebar_search_anything"]
    if (length(sidebar_filters) > 0) {
        query <- sql_search(
            con,
            query,
            table_name,
            names(sidebar_filters),
            unlist(sidebar_filters),
            relation = "AND",
            table_info = table_info
        )
    }

    if (length(params_order) > 0) {
        sort_exprs <- list()
        for (i in seq_along(params_order)) {
            col_db_name <- params_order[[i]]$column
            ascending <- params_order[[i]]$ascending
            if (ascending) {
                sort_exprs[[length(sort_exprs) + 1]] <- expr(!!sym(col_db_name))
            } else {
                sort_exprs[[length(sort_exprs) + 1]] <- expr(desc(!!sym(col_db_name)))
            }
        }
        
        # Add key column as the last sort criterion
        sort_exprs[[length(sort_exprs) + 1]] <- expr(!!sym(key_column))
        
        # Apply all sort expressions in a single arrange
        if (length(sort_exprs) > 0) {
            query <- query |> arrange(!!!sort_exprs)
        }
    }
    
    query <- query |>
        mutate(shiny_row_number = row_number())
        

    # round down the cache start to nearest cache_row_count
    start <- (floor(params_start / cache_row_count)) * cache_row_count

    print(glue::glue("DT request: start={start + 1}, len={cache_row_count}"))

    query <- query |>
        mutate(shiny_total_rows = n()) |>
        filter(shiny_row_number > start & shiny_row_number <= start + cache_row_count)
    
    query <- concept_id_to_concept_name(
        query = query, 
        con = con, 
        table_name = table_name, 
        tbl_all_cols = tbl_all_cols)

    query <- post_process_pipe(query)

    query <- query |>
        select(shiny_total_rows, all_of(show_columns))

    data_out <- collect(query)

    records_filtered <- if (nrow(data_out) > 0) {
        data_out$shiny_total_rows[1]
    } else {
        0
    }
    data_out$shiny_total_rows <- NULL


    key_column <- table_info$key_column
    person_col <- if ("person_id" %in% colnames(data_out)) "person_id" else NULL
    meta_data <- c()
    for(i in seq_len(nrow(data_out))){
        row_id <- data_out[[key_column]][i]
        person_id <- if (!is.null(person_col)) data_out[[person_col]][i] else NA
        meta <- pack_meta_info(table_name, person_id, row_id)
        meta_data <- c(meta_data,meta)
    }
    data_out <- cbind(shiny_meta = meta_data, data_out)
    row_indices <- as.character(seq(start + 1, length.out = nrow(data_out)))
    final_df <- cbind(" " = row_indices, data_out)

    final_matrix <- as.matrix(final_df)
    dimnames(final_matrix) <- NULL

    list(
        start = start,
        recordsTotal = records_total,
        recordsFiltered = records_filtered,
        cached_data = final_matrix
    )
}
