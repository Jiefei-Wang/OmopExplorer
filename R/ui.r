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

sidebar_ui <- sidebar(
    title = "Filters",
    id = "sidebar",
    collapsed = TRUE,
    # person id filter
    numericInput("sidebar_person_id_filter", "Person ID", value = ""),
    # date range filter
    dateRangeInput(
        "sidebar_date_range_filter", "Date Range",
        start = "0000-01-01",
        end = "9999-12-31"
    )

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
    death_list_ui
  )
)