

path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"

con <- duckdbCon(path)

# method 1
system.time({
    for(i in 1:10){
        start <- round(runif(1, 1, 100000))
        con$measurement|>
        mutate(shiny_row_idx = row_number()) |>
        filter(shiny_row_idx > start & shiny_row_idx <= start + 1000)|>
        collect() ->dt
        columns <- colnames(con$measurement)
        mapped_cols <- intersect(columns, names(omop_concept_id_source_value_map))

        for (col in mapped_cols){
            concept_ids <- dt[[col]]
            con$concept|>
            filter(concept_id %in%concept_ids)|>
            collect()
        }
    }
})
#    user  system elapsed 
#   16.77    0.80    8.79

# method 2
system.time({
    for(i in 1:10){
        start <- round(runif(1, 1, 100000))
        con$measurement|>
        mutate(shiny_row_idx = row_number()) |>
        filter(shiny_row_idx > start & shiny_row_idx <= start + 1000)|>
        concept_id_to_concept_name(concept_name, dt_col_names)|> # this id to name map cost time
        collect()
    }
})
#    user  system elapsed 
#   13.52    0.38    8.03