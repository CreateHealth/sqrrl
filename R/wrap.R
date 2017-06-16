
#' @export
parens <- function(...) paste0('(', commas(...), ')')

#' @export
quotes_ <- function(...) paste0('"', ..., '"')

#' @export
quotes <- function(...) {
  items <- list(...)
  unlist(lapply(items, function(x) {
    is_number <- !any(is.na(suppressWarnings(as.numeric(x))))
    if (is.factor(x)) quotes_(paste(x))
    else if (is_number) paste(x)
    else quotes_(x)
  }))
}

escape <- function(...) {
  gsub()
}