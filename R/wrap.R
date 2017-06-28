wrap <- function(.prefix, ..., .suffix = .prefix) paste0(.prefix, ..., .suffix)

#' @export
parens <- function(...) wrap('(', commas(...), .suffix = ')')

#' @export
quotes_ <- function(...) wrap('"', ...)

#' @export
backtick <- function(...) wrap('`', ...)

#' @export
quotes <- function(...) {
  items <- list(...)
  if (length(items) == 1 && !inherits(items[[1]], 'formula') && length(items[[1]]) > 1) {
    items <- as.list(items[[1]])
  }
  unlist(lapply(items, .quotes))
}

.quotes <- function(x, force = FALSE) {
  if (force) return(quotes_(paste(x)))

  sql_type <- sql_typeof(x)

  if (is.null(x))
    "NULL"
  else if (sql_type != 'formula' && any(is.na(x))) {
    if (sql_type == 'number') {
      return(ifelse(is.na(x), 'NULL', paste(x)))
    } else {
      y <- rep('""', length(x))
      y[which(!is.na(x))] <- .quotes(x[which(!is.na(x))])
      return(y)
    }
  } else {
    switch(sql_type,
           'formula' = Reduce(paste, deparse(x[[2]], width.cutoff = 500)),
           'factor' = quotes_(paste(x)),
           'Date'   = quotes_(strftime(x, '%F')),
           'POSIXt' = quotes_(strftime(x, '%F %T')),
           'number' = paste(x),
           quotes_(x)
    )
  }
}


escape_col <- function(..., .ignore_dot = TRUE) {
  sapply(c(...), function(x) {
    if (!.ignore_dot && grepl('\\.', x)) backtick(x)
    else if (toupper(x) %in% KEYWORDS) backtick(x)
    else if (is.numeric(x)) backtick(x)
    else x
  }, USE.NAMES = FALSE)
}

#' Returns output type of a vector or object relative to SQL
#'
#' Returns best-case (or best-guess) option for the SQL type of the input.
#' Mostly used to determine quoting rules.
sql_typeof <- function(x) {
  if (inherits(x, 'character')) y <- type.convert(x, as.is = TRUE)
  else y <- x
  is_number <- class(y) == "integer" | class(y) == "numeric"

  if (is.factor(x)) "factor"
  else if (inherits(x, 'formula')) 'formula'
  else if (inherits(x, "Date")) "Date"
  else if (inherits(x, "POSIXt")) "POSIXt"
  else if (is_number) 'number'
  else "string"
}