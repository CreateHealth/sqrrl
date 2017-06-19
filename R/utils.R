prefer_names <- function(x) {
  y <- unname(x)
  if (!is.null(names(x))) {
    y[names(x) != ''] <- names(x[names(x) != ''])
  }
  y
}

vec2df <- function(x) {
  x.names <- names(x)
  x <- setNames(as.character(x), x.names)
  data.frame(lapply(x, type.convert, as.is = TRUE), stringsAsFactors = FALSE)
}