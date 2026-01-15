sidebar_ui <- sidebar(
    id = "sidebar",
    collapsed = TRUE,
    # Header with title and clear all button in one row
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
      tags$h5("Filters", style = "margin: 0;"),
      actionButton(
        inputId = "sidebar_clear_all",
        label = "Clear All",
        class = "btn-link btn-sm",
        style = "padding: 0;"
      )
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
    hr(style = "margin-top: 10px; margin-bottom: 10px;"),
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
  })|>
  unname()
}

browser_ui <- function(){
    page_sidebar(
        title = div(
            style = "display: flex; justify-content: space-between; align-items: center; min-height: 60px;",
            div(
            style = "font-size: 1.25rem; font-weight: 600;",
            "OMOP Explorer"
            ),
            div(
            style = "flex: 1; margin-left: 20px; text-align: right;",
            uiOutput("patient_topbar")
            )
        ),
        sidebar = sidebar_ui,
        get_app_styles(),
        do.call(navset_tab, c(
            list(id = "main_tabs"),
            create_nav_panels()
        ))
    )
}
