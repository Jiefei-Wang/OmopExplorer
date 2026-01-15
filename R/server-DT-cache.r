global_DT_space <- new.env()
cache_row_count <- 1000

#' Clear the global DT cache
#'
#' @return No return value, called for side effects.
#' @keywords internal
clear_global_DT_cache <- function(){
    rm(list = ls(envir = global_DT_space), envir = global_DT_space)
}


init_DT_cache <- function(table_name){
    table_cache <- global_DT_space[[table_name]]
    if (is.null(table_cache)){
        table_cache <- list(
            dt_render = NULL,
            params = list()
        )
        global_DT_space[[table_name]] <- table_cache
    }
}

is_DT_render_cached <- function(table_name, params){
    table_cache <- global_DT_space[[table_name]]
    if (is.null(table_cache)){
        return(FALSE)
    }
    dt_render_cache <- table_cache$dt_render_cache
    if (is.null(dt_render_cache)) {
        return(FALSE)
    }
    cached_params <- dt_render_cache$params
    if (!identical(cached_params, params)) {
        return(FALSE)
    }
    return(TRUE)
}

get_DT_render_from_cache <- function(table_name){
    table_cache <- global_DT_space[[table_name]]
    dt_render <- table_cache$dt_render
    return(dt_render)
}

set_DT_render_cache <- function(table_name, dt_render, params){
    table_cache <- global_DT_space[[table_name]]
    table_cache$dt_render <- dt_render
    table_cache$params <- params
    global_DT_space[[table_name]] <- table_cache
}


#' Check whether cached DT data is available
#'
#' @param table_name Character scalar. Table name.
#' @param params_search Named list of search parameters.
#' @param params_order List of order specs with `column` and `ascending`.
#' @param params_start Integer scalar. Zero-based start index.
#' @param params_len Integer scalar. Requested page length.
#' @return Logical scalar indicating whether the cache can be used.
#' @keywords internal
is_DT_data_cache_available <- function(table_name, params_search, params_order, params_start, params_len){
    table_cache <- global_DT_space[[table_name]]
    data_cache <- table_cache[['data_cache']]
    if (is.null(data_cache)){
        return(FALSE)
    }

    cache_start <- data_cache$start
    cache_end <- cache_start + cache_row_count
    if (params_start >= cache_start &&
        (params_start + params_len) <= cache_end) {
            combined_condition <- list(params_search, params_order)
            if (identical(combined_condition, data_cache$combined_condition)) {
                return(TRUE)
            }
        }
    return(FALSE)
}

#' Get cached DT data
#'
#' Return cached DT data for a table and paging range.
#'
#' @param table_name Character scalar. Table name.
#' @param params_start Integer scalar. Zero-based start index.
#' @param params_len Integer scalar. Requested page length.
#' @return List with elements: `recordsTotal`, `recordsFiltered`, `cached_data`.
#' @keywords internal
get_cached_DT <- function(table_name, params_start, params_len){
    table_cache <- global_DT_space[[table_name]]
    data_cache <- table_cache$data_cache
    cache_matrix <- data_cache$cache_matrix
    cache_start <- params_start - data_cache$start + 1
    cache_end <- min(params_start - data_cache$start + params_len, nrow(cache_matrix))
    if (cache_end != 0){
        cached_data <- cache_matrix[
            cache_start:cache_end, , drop = FALSE]
    } else {
        cached_data <- cache_matrix[c(), , drop=FALSE]
    }
    return(list(
        recordsTotal = data_cache$recordsFiltered,
        recordsFiltered = data_cache$recordsFiltered,
        cached_data = cached_data
    ))
}

#' Set cached DT data
#'
#' @param table_name Character scalar. Table name.
#' @param cache_start Integer scalar. Cache start row.
#' @param params_search Named list of search parameters.
#' @param params_order List of order specs with `column` and `ascending`.
#' @param records_filtered Integer scalar. Filtered record count.
#' @param cache_matrix Matrix of cached DT rows.
#' @return No return value, called for side effects.
#' @keywords internal
set_cached_DT <- function(table_name, cache_start, params_search, params_order, records_filtered, cache_matrix){
    table_cache <- global_DT_space[[table_name]]
    if (is.null(table_cache)){
        table_cache <- list()
    }
    data_cache <- list(
        start = cache_start,
        combined_condition = list(params_search, params_order),
        recordsFiltered = records_filtered,
        cache_matrix = cache_matrix
    )
    table_cache[['data_cache']] <- data_cache
    global_DT_space[[table_name]] <- table_cache
}
