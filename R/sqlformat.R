#' Format an SQL Query using sqlformat
#'
#' Uses the python package [andialbrecht/sqlparse](https://github.com/andialbrecht/sqlparse)
#' to format an SQL query. See `sqlformat -h` for more information about formatting options.
#'
#' @param sql SQL Query for formatting
#' @param header Header added to formatted query as a comment
#' @param md Add markdown backticks?
#' @param sqlformat_options Options passed to `sqlformat`, see `sqlformat -h` for more information.
#' @export
sqlformat <- function(
  sql,
  header = NULL,
  md = TRUE,
  sqlformat_options = '-k upper -a'
) {
  sqlformat_installed <- FALSE
  try(sqlformat_installed <- length(system('sqlformat -h', intern = TRUE)))
  if (!sqlformat_installed) {
    warning("Please install sqlformat via\nhttps://github.com/andialbrecht/sqlparse", call. = FALSE)
  }

  out <- c()
  if (md) out <- paste0(out, "\n```sql\n")
  if (!is.null(header)) out <- paste0(out, "/*", header, "*/\n")
  if (sqlformat_installed) {
    sql <- system(paste0("echo '", sql, "' | sqlformat ", sqlformat_options, " -"), intern = TRUE)
  }
  out <- paste0(out, paste(sql, collapse = '\n'))
  if (md) out <- paste0(out, "\n```\n")
  out
}

#' @describeIn sqlformat Wrapper around [sqlformat] to simplify SQL statements.
#' @export
sqlsimplify <- function(sql, ...) {
  sql <- sqlformat(sql, md = FALSE, ...)
  sql <- stringr::str_split(sql, '\n')[[1]]
  sql <- stringr::str_trim(sql, 'both')
  paste(sql, collapse = ' ')
}