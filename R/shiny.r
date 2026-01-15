#' Run the OMOP Explorer Shiny app
#'
#' @param con List. Database connection created by `duckdbCon()` or `mockCon()`.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#' @examples
#' con <- mockCon()
#' \dontrun{
#' runExplorer(con)
#' }
#' DBI::dbDisconnect(con$dbcon, shutdown = TRUE)
#' @export
runExplorer <- function(con, ...) {
    clear_global_DT_cache()
    server = function(input, output, session) {
        browser_server(input, output, session, con)
    }
    ui = browser_ui()

    app <- shinyApp(ui, server)
    runApp(app, ...)
}
