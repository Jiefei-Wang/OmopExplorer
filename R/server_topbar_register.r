
register_server_topbar <- function(input, output, session, con, params) {
    
    output$patient_topbar <- renderUI({
        person_id <- params$target_person_id()
        
        # Always render container to prevent resizing
        if (is.na(person_id)) {
            return(div(style = "height: 60px;"))
        }
        
        # Get person data
        person_tbl <- params$table_info[["person"]]$table
        person_data <- person_tbl %>%
            filter(person_id == !!person_id) %>%
            collect()
        
        if (nrow(person_data) == 0) {
            return(NULL)
        }
        
        # Calculate age from birth year
        current_year <- as.integer(format(Sys.Date(), "%Y"))
        age <- current_year - person_data$year_of_birth[1]
        
        # Get gender
        gender <- if ("gender_concept_id" %in% colnames(person_data)) {
            gender_concept_id <- person_data$gender_concept_id[1]
            concept_tbl <- params$table_info[["concept"]]$table
            gender_name <- concept_tbl %>%
                filter(concept_id == !!gender_concept_id) %>%
                pull(concept_name) %>%
                head(1)
            if (length(gender_name) > 0) gender_name else "Unknown"
        } else {
            "Unknown"
        }
        
        # Get race
        race <- if ("race_concept_id" %in% colnames(person_data)) {
            race_concept_id <- person_data$race_concept_id[1]
            concept_tbl <- params$table_info[["concept"]]$table
            race_name <- concept_tbl %>%
                filter(concept_id == !!race_concept_id) %>%
                pull(concept_name) %>%
                head(1)
            if (length(race_name) > 0) race_name else "Unknown"
        } else {
            "Unknown"
        }
        
        # Check if deceased
        death_tbl <- params$table_info[["death"]]$table
        is_deceased <- death_tbl %>%
            filter(person_id == !!person_id) %>%
            summarise(n = n()) %>%
            pull(n) > 0
        status <- if (is_deceased) "Deceased" else "Live"
        
        # Get last visit date and calculate days ago
        visit_tbl <- params$table_info[["visit_occurrence"]]$table
        last_visit_data <- visit_tbl %>%
            filter(person_id == !!person_id) %>%
            arrange(desc(visit_start_date)) %>%
            select(visit_start_date) %>%
            head(1) %>%
            collect()
        
        last_visit_text <- if (nrow(last_visit_data) > 0) {
            visit_date <- last_visit_data$visit_start_date[1]
            days_ago <- as.integer(difftime(Sys.Date(), as.Date(visit_date), units = "days"))
            formatted_date <- format(as.Date(visit_date), "%b %d, %Y")
            sprintf("%s (%d days ago)", formatted_date, days_ago)
        } else {
            "No visits"
        }
        
        # Count statistics from different tables
        visit_count <- visit_tbl %>%
            filter(person_id == !!person_id) %>%
            summarise(n = n()) %>%
            pull(n)
        
        condition_count <- params$table_info[["condition_occurrence"]]$table %>%
            filter(person_id == !!person_id) %>%
            summarise(n = n_distinct(condition_concept_id)) %>%
            pull(n)
        
        procedure_count <- params$table_info[["procedure_occurrence"]]$table %>%
            filter(person_id == !!person_id) %>%
            summarise(n = n()) %>%
            pull(n)
        
        drug_count <- params$table_info[["drug_exposure"]]$table %>%
            filter(person_id == !!person_id) %>%
            summarise(n = n_distinct(drug_concept_id)) %>%
            pull(n)
        
        measurement_count <- params$table_info[["measurement"]]$table %>%
            filter(person_id == !!person_id) %>%
            summarise(n = n()) %>%
            pull(n)
        
        # Get care period (first and last visit)
        care_period_text <- if (nrow(last_visit_data) > 0) {
            period_data <- visit_tbl %>%
                filter(person_id == !!person_id) %>%
                summarise(
                    first_visit = min(visit_start_date, na.rm = TRUE),
                    last_visit = max(visit_start_date, na.rm = TRUE)
                ) %>%
                collect()
            
            first_year <- format(as.Date(period_data$first_visit[1]), "%Y")
            last_year <- format(as.Date(period_data$last_visit[1]), "%Y")
            years_diff <- as.numeric(last_year) - as.numeric(first_year)
            years_text <- if (years_diff > 0) sprintf(" (%.1f years)", years_diff) else ""
            sprintf("%s-%s%s", first_year, last_year, years_text)
        } else {
            "N/A"
        }
        
        # Create two-row layout
        div(
            style = "background-color: #f8f9fa; padding: 8px 12px; border: 1px solid #dee2e6; border-radius: 4px; font-family: monospace; font-size: 12px; height: 60px; display: inline-block; text-align: right;",
            div(
                style = "margin-bottom: 3px; white-space: nowrap;",
                sprintf("Person ID: %s  |  Age: %d  |  Gender: %s  |  Race: %s  |  Status: %s  |  Last Visit: %s",
                        person_id, age, gender, race, status, last_visit_text)
            ),
            div(
                style = "white-space: nowrap;",
                sprintf("Total Visits: %d  |  Conditions: %d  |  Procedures: %d  |  Drugs: %d  |  Measurements: %d  |  Care Period: %s",
                        visit_count, condition_count, procedure_count, drug_count, measurement_count, care_period_text)
            )
        )
    })
}