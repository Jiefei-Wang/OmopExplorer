sql_dt_filter <- function(remote_tbl) {
    function(data, params) {
        # Get the actual table from the reactive without tracking dependency
        tbl <- isolate(remote_tbl())
        
        # 1. Column Metadata
        dt_col_names <- colnames(tbl)

        # 2. Total records in DB (fast)
        records_total <- as.numeric(tally(tbl) |> pull(1))

        # 3. Build Query
        query <- tbl

        # 4. Global Search (if any)
        global_search <- params$search$value
        if (!is.null(global_search) && global_search != "") {
            search_term <- paste0("%", global_search, "%")
            # Get only columns marked as searchable
            searchable_indices <- which(vapply(params$columns, function(x) x$searchable == "true", logical(1)))
            # Map to actual DB names (offset by 1 if column 0 is rowname placeholder)
            search_cols <- dt_col_names[searchable_indices - 1]
            search_cols <- search_cols[!is.na(search_cols)]

            query <- query |> filter(if_any(all_of(search_cols), ~ . %like% search_term))
        }

        # 5. Column-specific Search
        for (i in seq_along(params$columns)) {
            col_search <- params$columns[[i]]$search$value
            col_db_name <- params$columns[[i]]$name
            if (col_search != "" && col_db_name %in% dt_col_names) {
                query <- query |> filter(!!sym(col_db_name) %like% !!paste0("%", col_search, "%"))
            }
        }

        # 6. Filtered count
        records_filtered <- as.numeric(tally(query) |> pull(1))

        # 7. Sorting
        if (length(params$order) > 0) {
            for (i in seq_along(params$order)) {
                col_idx <- as.integer(params$order[[i]]$column) + 1
                # params$columns is 0-indexed, col 0 is often " "
                col_db_name <- params$columns[[col_idx]]$name
                direction <- params$order[[i]]$dir

                if (col_db_name %in% dt_col_names) {
                    if (direction == "asc") {
                        query <- query |> arrange(!!sym(col_db_name))
                    } else {
                        query <- query |> arrange(desc(!!sym(col_db_name)))
                    }
                }
            }
        }

        # 8. Fetch Slice (True Lazy Load)
        start <- as.integer(params$start)
        len <- as.integer(params$length)

        # Fetch data into R
        data_out <- query |>
            filter(row_number() > start & row_number() <= start + len) |>
            collect()

        # current_page_data(data_out)

        # Add the Row Names / Index column if column 0 in UI is " "
        # This prevents "Requested unknown parameter '0'"
        row_indices <- as.character(seq(start + 1, length.out = nrow(data_out)))
        final_df <- cbind(" " = row_indices, data_out)

        # Convert to un-named matrix so JSON is [[val, val], [val, val]]
        final_matrix <- as.matrix(final_df)
        dimnames(final_matrix) <- NULL

        return(list(
            draw = as.integer(params$draw),
            recordsTotal = records_total,
            recordsFiltered = records_filtered,
            data = final_matrix,
            DT_rows_all = seq_len(records_filtered), 
            DT_rows_current = seq(start + 1, length.out = nrow(data_out))
        ))
    }
}


render_db_DT <- function(table, ...){
    # Default options
    options <- list(
        serverSide = TRUE,
        processing = TRUE
    )
    
    # Get additional arguments
    args <- list(...)
    
    # Merge options if provided
    if ("options" %in% names(args)) {
        options <- c(options, args$options)
        args$options <- NULL
    }
    
    # Build the renderDT call
    dt_args <- c(
        list(
            expr = quote({
                table() |>
                    filter(FALSE) |>
                    collect()
            }),
            server = TRUE,
            options = options,
            funcFilter = sql_dt_filter(table),
            selection = 'single'
        ),
        args  # Pass remaining arguments
    )
    do.call(renderDT, dt_args)
}

make_server_person_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    reactiveVal(
        con$person |> 
        mutate(birth_date = as.Date(birth_datetime)) |>
        inner_join(concept_name, by = c("gender_concept_id" = "concept_id")) |>
        rename(gender = concept_name)|>
        inner_join(concept_name, by = c("race_concept_id" = "concept_id")) |>
        rename(race = concept_name)|>
        inner_join(concept_name, by = c("ethnicity_concept_id" = "concept_id")) |>
        rename(ethnicity = concept_name)|>
        select(person_id, birth_date, gender, race, ethnicity)
    ) -> person_table
    
    output$person_DT <- render_db_DT(
        person_table,
        callback = DT::JS("
        table.on('click.dt', 'tbody tr', function() {
            var data = table.row(this).data();
            console.log(data);
            // person_id is the second column in df
            Shiny.setInputValue('tbl_person_id', data[1], {priority: 'event'});
        });
        ")
        )
}

make_server_condition_DT <- function(input, output, session, con, params){
    concept_name <- con$concept |> select(concept_id, concept_name)
    condition_table <- reactive({
        dt <- con$condition_occurrence |> 
            inner_join(concept_name, by = c("condition_concept_id" = "concept_id")) |>
            rename(condition = concept_name)|>
            select(person_id, condition, condition_start_date, condition_end_date)

        if (!is.null(input$tbl_person_id)) {
            dt <- dt |> 
                filter(person_id == input$tbl_person_id)
        }
        dt
    })

    output$condition_DT <- render_db_DT(condition_table)
}







browser_server <- function(input, output, session, con) {
    params <- list()
    params$selected_person_id <- reactiveVal(NULL)
    make_server_person_DT(input, output, session, con, params)
    make_server_condition_DT(input, output, session, con, params)
    
    observeEvent(input$tbl_person_id, {
        print(input$tbl_person_id)
    })
}