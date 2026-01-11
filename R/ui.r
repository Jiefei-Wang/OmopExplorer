sidebar_ui <- sidebar(
    title = "Filters",
    id = "sidebar",
    collapsed = TRUE,
    # Global search filter
    textInput(inputId = "sidebar_table_filter", label = "Search Anything", value = ""),
    hr(),
    # Dynamic column search boxes will be rendered here
    uiOutput("sidebar_column_filters")
)

# Dynamically create nav_panel UI components
create_nav_panels <- function() {
  lapply(omop_panes, function(pane) {
    dt_output_id <- paste0(pane$table_name, "_DT")
    nav_panel(
      pane$display_name,
      value = pane$table_name,
      DTOutput(dt_output_id)
    )
  })
}

browser_ui <- page_sidebar(
  title = "OMOP Explorer",
  sidebar = sidebar_ui,
  do.call(navset_tab, c(
    list(id = "main_tabs"),
    create_nav_panels()
  ))
)