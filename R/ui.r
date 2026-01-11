person_list_ui <- nav_panel(
    "Person",
    DTOutput("person_DT")
)

visit_list_ui <- nav_panel(
    "Visit",
    DTOutput("visit_DT")
)

condition_list_ui <- nav_panel(
    "Condition",
    DTOutput("condition_DT")
)

procedure_list_ui <- nav_panel(
    "Procedure",
    DTOutput("procedure_DT")
)

measurement_list_ui <- nav_panel(
    "Measurement",
    DTOutput("measurement_DT")
)

drug_list_ui <- nav_panel(
    "Drug",
    DTOutput("drug_DT")
)

note_list_ui <- nav_panel(
    "Note",
    DTOutput("note_DT")
)

death_list_ui <- nav_panel(
    "Death",
    DTOutput("death_DT")
)

provider_list_ui <- nav_panel(
    "Provider",
    DTOutput("provider_DT")
)

care_site_list_ui <- nav_panel(
    "Care Site",
    DTOutput("care_site_DT")
)

sidebar_ui <- sidebar(
    title = "Filters",
    id = "sidebar",
    collapsed = TRUE,
    # person id filter
    textInput(inputId = "sidebar_table_filter", label = "Search Anything", value = "")

)





browser_ui <- page_sidebar(
  title = "OMOP Explorer",
  sidebar = sidebar_ui,
  navset_tab(
    person_list_ui,
    visit_list_ui,
    condition_list_ui,
    procedure_list_ui,
    measurement_list_ui,
    drug_list_ui,
    note_list_ui,
    death_list_ui,
    provider_list_ui,
    care_site_list_ui
  )
)