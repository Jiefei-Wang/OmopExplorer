person_list_ui <- nav_panel(
    "Person List",
    DTOutput("person_DT")
)

condition_list_ui <- nav_panel(
    "Condition List",
    DTOutput("condition_DT")
)

browser_ui <- page_sidebar(
  title = "Penguins dashboard",
  sidebar = "test",
  navset_tab(
    person_list_ui,
    condition_list_ui
  )
)