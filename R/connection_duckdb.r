#' Create a DuckDB-backed OMOP connection
#'
#' Build a list of `tbl` objects for available OMOP tables along with the
#' underlying DuckDB connection.
#'
#' @param dbdir Character scalar. Path to the DuckDB database directory.
#' @param read_only Logical scalar. Whether to open the database read-only.
#' @param ... Additional arguments passed to `DBI::dbConnect()`.
#' @return A list with one entry per available OMOP table plus a `dbcon`
#'   element containing the DuckDB connection.
#' @examples
#' db_path <- tempfile(fileext = ".duckdb")
#' con <- duckdbCon(db_path)
#' DBI::dbDisconnect(con$dbcon, shutdown = TRUE)
#' @export
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
