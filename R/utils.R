prefer_names <- function(x) {
  y <- unname(x)
  if (!is.null(names(x))) {
    y[names(x) != ''] <- names(x[names(x) != ''])
  }
  y
}

vec2df <- function(x) {
  x <- as.character(x)
  data.frame(lapply(x, type.convert, as.is = TRUE), stringsAsFactors = FALSE)
}