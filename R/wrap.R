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
  unlist(lapply(items, function(x) {
    is_number <- !any(is.na(suppressWarnings(as.numeric(x))))
    if (is.factor(x)) quotes_(paste(x))
    else if (inherits(x, 'Date')) quotes_(strftime(x, '%F'))
    else if (inherits(x, 'POSIXt')) quotes_(strftime(x, '%F %T'))
    else if (is_number) paste(x)
    else quotes_(x)
  }))
}


escape_col <- function(...) {
  sapply(c(...), function(x) {
    if (grepl('\\.', x)) backtick(x)
    else x
  }, USE.NAMES = FALSE)
}