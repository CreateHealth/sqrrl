
#' @export
`%+%` <- function(a, b) paste(a, b)

#' @export
`%.%` <- function(a, b) paste0(a, b)

#' @export
commas <- function(...) paste(c(...), collapse = ', ')

