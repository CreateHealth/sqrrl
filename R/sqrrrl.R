#' @export
SELECT <- function(...) {
  cols <- c(...)
  if (!length(cols)) return("SELECT *")
  if (is.null(names(cols))) {
    paste('SELECT', commas(cols))
  } else {
    cols <- sapply(seq_along(cols), function(x) {
      if (names(cols)[x] == '') cols[x]
      else paste(cols[x], 'as', names(cols)[x])
    })
    paste("SELECT", commas(cols))
  }
}

#' @export
FROM <- function(...) {
  tables <- c(...)
  tables <- paste(tables, names(tables), sep = ' ', collapse = ', ')
  paste("FROM", tables)
}

#' @export
commas <- function(...) paste(c(...), collapse = ', ')

#' @export
parens <- function(...) paste0('(', commas(...), ')')

#' @export
quotes_ <- function(...) paste0('"', ..., '"')

#' @export
quotes <- function(...) {
  items <- list(...)
  unlist(sapply(items, function(x) {
    is_number <- !any(is.na(suppressWarnings(as.numeric(x))))
    if (is_number) paste(x)
    else quotes_(x)
  }))
}

#' @export
AND   <- function(...) paste(c(...), collapse = ' AND ')

#' @export
OR    <- function(...) paste(c(...), collapse = ' OR ')

.delim  <- function(..., op) paste(names(c(...)), quotes(...), sep = op)

#' @export
eq     <- function(...) .delim(..., op = '=')

#' @export
neq    <- function(...) .delim(..., op = '!=')

#' @export
lt     <- function(...) .delim(..., op = '<')

#' @export
leq    <- function(...) .delim(..., op = '<=')

#' @export
gt     <- function(...) .delim(..., op = '>')

#' @export
geq    <- function(...) .delim(..., op = '>=')

#' @export
WHERE  <- function(..., cond=TRUE) ifelse(cond, paste('WHERE', AND(...)), '')

#' @export
in_    <- function(var, vals) paste(var, parens(vals), sep = ' IN ')

#' @export
`%IN%` <- function(var, vals) in_(var, vals)

#' @export
like_  <- function(var, val) paste0(var, ' LIKE("', val, '")')

#' @export
`%LIKE%` <- function(var, val) ifelse(length(val) > 1, like(var, val), like_(var, val))

#' @export
like   <- function(var, ...) parens(OR(sapply(c(...), like_, var = var)))

#' @export
`%+%` <- function(a, b) paste(a, b)