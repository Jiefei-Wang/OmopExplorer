duckdbCon <- function(dbdir, read_only = TRUE, ...) {
  dbcon <- dbConnect(duckdb(), dbdir = dbdir, read_only = read_only, ...)
  
  available_tables <- intersect(omop_tables, dbListTables(dbcon))
  
  con <- list()
  for (i in available_tables) {
    con[[i]] <- tbl(dbcon, i)
  }
  con$dbcon <- dbcon
  con
}