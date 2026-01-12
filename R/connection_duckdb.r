duckdbCon <- function(path) {
  dbcon <- dbConnect(duckdb(), dbdir = path, read_only = TRUE)
  
  available_tables <- intersect(omop_tables, dbListTables(dbcon))
  
  con <- list()
  for (i in available_tables) {
    con[[i]] <- tbl(dbcon, i)
  }
  con$dbcon <- dbcon
  con
}