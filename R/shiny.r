runExplorer <- function(con, ...) {
    clear_global_DT_cache()
    server = function(input, output, session) {
        browser_server(input, output, session, con)
    }

    app <- shinyApp(browser_ui, server)
    runApp(app, ...)
}