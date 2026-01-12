#' Create a mock OMOP DuckDB connection
#'
#' Build a temporary DuckDB database from the packaged mock CDM data.
#'
#' @return A list with one entry per available OMOP table plus a `dbcon`
#'   element containing the DuckDB connection.
#' @examples
#' con <- mockCon()
#' DBI::dbDisconnect(con$dbcon, shutdown = TRUE)
#' @export
mockCon <- function() {
  db_path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
  }, add = TRUE)

  for (table_name in names(mock_cdm)) {
    table_data <- as.data.frame(mock_cdm[[table_name]])
    class(table_data) <- "data.frame"

    if (nrow(table_data) == 0) {
      next
    }

    DBI::dbWriteTable(con, table_name, table_data, overwrite = TRUE)
  }

  duckdbCon(db_path)
}
