sidebar_ui <- sidebar(
    title = "Filters",
    id = "sidebar",
    collapsed = TRUE,
    # Global search filter with clear buttons
    actionButton(
    inputId = "sidebar_clear_all",
    label = "Clear All",
    class = "btn-link btn-sm",
    style = "margin-top: 10px;"
    ),
    div(
      class = "search-anything-row",
      make_search_box(
        inputId = "sidebar_search_anything",
        label = "Search Anything",
        value = "",
        placeholder = "Search all columns"
      )
    ),
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
  get_app_styles(),
  do.call(navset_tab, c(
    list(id = "main_tabs"),
    create_nav_panels()
  ))
)
