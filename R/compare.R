#' Comparison Operators
#'
#' Generate SQL snippets comparing variables and values. `sqrrrl` provides two
#' types of each function. Functions without an underscore, like `eq`, take an
#' unlimited number of arguments and value pairs, where each argument is assumed
#' to be single valued and the argument name is used as the column name.
#' Functions with an underscore, like `eq_`, take a vector of columns and a
#' vector of values that are zipped together. Infix operators are also provided
#' for `LIKE` and `IN`.
#'
#' @name comparison
NULL


.delim  <- function(..., op) .delim_(names(c(...)), c(...), op)
.delim_ <- function(var, val, op) paste(escape_col(var), quotes(val), sep = op)

#' @rdname comparison
#' @examples
#' eq(a = 1, b = 'value')      # "a=1" "b=\"value\""
#' @export
eq     <- function(...) .delim(..., op = '=')

#' @rdname comparison
#' @examples
#' eq_(letters[1:3], 1:3)      # "a=1" "b=2" "c=3"
#'
#' @export
eq_    <- function(var, val) .delim_(var, val, '=')

#' @rdname comparison
#' @examples
#' neq(a = 1, b = 'value')     # "a!=1" "b!=\"value\""
#' @export
neq    <- function(...) .delim(..., op = '!=')

#' @rdname comparison
#' @examples
#' neq_(letters[1:3], 1:3)     # "a!=1" "b!=2" "c!=3"
#'
#' @export
neq_ <- function(var, val) .delim_(var, val, '!=')

#' @rdname comparison
#' @examples
#' lt(a = 1, b = '2017-01-01') # "a<1" "b<\"2017-01-01\""
#' @export
lt <- function(...) .delim(..., op = '<')

#' @rdname comparison
#' @examples
#' lt_(letters[1:3], 10:12)    # "a<10" "b<11" "c<12"
#'
#' @export
lt_ <- function(var, val) .delim_(var, val, op = '<')

#' @rdname comparison
#' @examples
#' leq(a = 1, b = '2017-01-01')# "a<=1" "b<=\"2017-01-01\""
#' @export
leq <- function(...) .delim(..., op = '<=')

#' @rdname comparison
#' @examples
#' leq_(letters[1:3], 10:12)   # "a<=10" "b<=11" "c<=12"
#'
#' @export
leq_ <- function(var, val) .delim_(var, val, op = '<=')

#' @rdname comparison
#' @examples
#' gt(a = 1, b = '2017-01-01') # "a>1" "b>\"2017-01-01\""
#' @export
gt <- function(...) .delim(..., op = '>')

#' @rdname comparison
#' @examples
#' gt_(letters[1:3], 10:12)    # "a>10" "b>11" "c>12"
#'
#' @export
gt_ <- function(var, val) .delim_(var, val, op = '>')

#' @rdname comparison
#' @examples
#' geq(a = 1, b = '2017-01-01')# "a>=1" "b>=\"2017-01-01\""
#' @export
geq <- function(...) .delim(..., op = '>=')

#' @rdname comparison
#' @examples
#' geq_(letters[1:3], 10:12)   # "a>=10" "b>=11" "c>=12"
#'
#' @export
geq_ <- function(var, val) .delim_(var, val, op = '>=')

#' @rdname comparison
#' @examples
#' in_('a', c(5, 7, 8))        # "a IN (5, 7, 8)"
#' @export
in_ <- function(var, vals) {
  var <- escape_col(var)
  paste(var, ifelse(length(vals) > 1, parens(quotes(vals)), parens(vals)), sep = ' IN ')
}

#' @rdname comparison
#' @examples
#' 'a' %IN% c(5, 7, 8)         # "a IN (5, 7, 8)"
#'
#' @export
`%IN%` <- function(var, vals) in_(var, vals)

#' @rdname comparison
#' @examples
#' like_('a', 'Prefix%')       # "a LIKE(\"Prefix%\")"
#' @export
like_  <- function(var, val) paste0(escape_col(var), ' LIKE("', val, '")')

#' @rdname comparison
#' @examples
#' 'a' %LIKE% 'Prefix%'        # "a LIKE(\"Prefix%\")"
#' @export
`%LIKE%` <- function(var, val) ifelse(length(val) > 1, like(var, val), like_(var, val))

#' @rdname comparison
#' @examples
#' like('a', 'Prefix%', '%Suffix')
#' # "(a LIKE(\"Prefix%\") OR a LIKE(\"%Suffix\"))"
#'
#' @export
like   <- function(var, ...) parens(OR(sapply(c(...), like_, var = escape_col(var))))

#' @rdname comparison
#' @examples
#' BETWEEN('a', 0, 10)         # "a BETWEEN 0 AND 10"
#' BETWEEN('timestamp', '2016-01-01', '2017-01-01')
#' # "`timestamp` BETWEEN \"2016-01-01\" AND \"2017-01-01\""
#' @export
BETWEEN <- function(var, val1, val2) paste(escape_col(var), 'BETWEEN', AND(quotes(val1, val2)))