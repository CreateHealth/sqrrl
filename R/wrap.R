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
  if (length(items) == 1 && length(items[[1]]) > 1) items <- as.list(items[[1]])
  unlist(lapply(items, .quotes))
}

.quotes <- function(x, force = FALSE) {
  if (force)
    return(quotes_(x))
  if (inherits(x, 'character')) y <- type.convert(x, as.is = TRUE)
  else y <- x
  is_number <- class(y) == "integer" | class(y) == "numeric"
  if (is.null(x))
    "NULL"
  else if (any(is.na(x))) {
    if (is_number) ifelse(is.na(x), 'NULL', paste(y))
    else ifelse(is.na(x), '""', quotes_(x))
  } else if (is.factor(x))
    quotes_(paste(x))
  else if (inherits(x, 'Date'))
    quotes_(strftime(x, '%F'))
  else if (inherits(x, 'POSIXt'))
    quotes_(strftime(x, '%F %T'))
  else if (is_number)
    paste(x)
  else
    quotes_(x)
}


escape_col <- function(..., .ignore_dot = FALSE) {
  sapply(c(...), function(x) {
    if (!.ignore_dot && grepl('\\.', x)) backtick(x)
    else if (toupper(x) %in% KEYWORDS) backtick(x)
    else if (is.numeric(x)) backtick(x)
    else x
  }, USE.NAMES = FALSE)
}