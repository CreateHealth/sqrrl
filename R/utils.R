prefer_names <- function(x) {
  y <- unname(x)
  if (!is.null(names(x))) {
    y[names(x) != ''] <- names(x[names(x) != ''])
  }
  y
}