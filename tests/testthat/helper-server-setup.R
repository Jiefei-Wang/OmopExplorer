make_test_params <- function(con, displayed_table = "person") {
    params <- new.env()
    params$table_info <- build_table_info(con)
    params$displayed_table_name <- shiny::reactiveVal(displayed_table)
    params$sidebar_search_values_internal <- shiny::reactiveValues()
    params$sidebar_search_values <- function() {
        shiny::isolate(shiny::reactiveValuesToList(params$sidebar_search_values_internal))
    }
    params$target_person_id <- shiny::reactiveVal(NA)
    params
}

get_first_person_id <- function(con) {
    if (is.null(con$person)) return(NA_real_)
    data <- con$person |>
        dplyr::select(person_id) |>
        head(1) |>
        dplyr::collect()
    if (nrow(data) == 0) return(NA_real_)
    data$person_id[[1]]
}
