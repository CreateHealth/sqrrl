.delim  <- function(..., op) .delim_(names(c(...)), c(...), op)
.delim_ <- function(x, y, op) paste(x, quotes(y), sep = op)

#' @export
eq     <- function(...) .delim(..., op = '=')
eq_    <- function(x, y) .delim_(x, y, '=')

#' @export
neq    <- function(...) .delim(..., op = '!=')
neq_ <- function(x, y) .delim_(x, y, '!=')

#' @export
lt <- function(...) .delim(..., op = '<')
lt_ <- function(x, y) .delim_(x, y, op = '<')

#' @export
leq <- function(...) .delim(..., op = '<=')
leq_ <- function(x, y) .delim_(x, y, op = '<=')

#' @export
gt <- function(...) .delim(..., op = '>')
gt_ <- function(x, y) .delim_(x, y, op = '>')

#' @export
geq <- function(...) .delim(..., op = '>=')
geq_ <- function(x, y) .delim_(x, y, op = '>=')

#' @export
in_ <- function(var, vals) paste(var, ifelse(length(vals) > 1, parens(quotes(vals)), parens(vals)), sep = ' IN ')

#' @export
`%IN%` <- function(var, vals) in_(var, vals)

#' @export
like_  <- function(var, val) paste0(var, ' LIKE("', val, '")')

#' @export
`%LIKE%` <- function(var, val) ifelse(length(val) > 1, like(var, val), like_(var, val))

#' @export
like   <- function(var, ...) parens(OR(sapply(c(...), like_, var = var)))

#' @export
BETWEEN <- function(var, val1, val2) paste(var, 'BETWEEN', AND(quotes(val1, val2)))