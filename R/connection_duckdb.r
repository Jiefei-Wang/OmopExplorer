duckdbCon <- function(path) {
  dbcon <- dbConnect(duckdb(), dbdir = path)
  
  available_tables <- intersect(omop_tables, dbListTables(dbcon))
  
  con <- list()
  for (i in available_tables) {
    con[[i]] <- tbl(dbcon, i)
  }
  con
}