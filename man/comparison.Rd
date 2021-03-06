% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{comparison}
\alias{comparison}
\alias{eq}
\alias{eq_}
\alias{neq}
\alias{neq_}
\alias{lt}
\alias{lt_}
\alias{leq}
\alias{leq_}
\alias{gt}
\alias{gt_}
\alias{geq}
\alias{geq_}
\alias{in_}
\alias{\%IN\%}
\alias{like_}
\alias{\%LIKE\%}
\alias{like}
\alias{BETWEEN}
\title{Comparison Operators}
\usage{
eq(...)

eq_(var, val)

neq(...)

neq_(var, val)

lt(...)

lt_(var, val)

leq(...)

leq_(var, val)

gt(...)

gt_(var, val)

geq(...)

geq_(var, val)

in_(var, vals)

var \%IN\% vals

like_(var, val)

var \%LIKE\% val

like(var, ...)

BETWEEN(var, val1, val2)
}
\description{
Generate SQL snippets comparing variables and values. \code{sqrrl} provides two
types of each function. Functions without an underscore, like \code{eq}, take an
unlimited number of arguments and value pairs, where each argument is assumed
to be single valued and the argument name is used as the column name.
Functions with an underscore, like \code{eq_}, take a vector of columns and a
vector of values that are zipped together. Infix operators are also provided
for \code{LIKE} and \code{IN}.
}
\examples{
eq(a = 1, b = 'value')      # "a=1" "b=\\"value\\""
eq_(letters[1:3], 1:3)      # "a=1" "b=2" "c=3"

neq(a = 1, b = 'value')     # "a!=1" "b!=\\"value\\""
neq_(letters[1:3], 1:3)     # "a!=1" "b!=2" "c!=3"

lt(a = 1, b = '2017-01-01') # "a<1" "b<\\"2017-01-01\\""
lt_(letters[1:3], 10:12)    # "a<10" "b<11" "c<12"

leq(a = 1, b = '2017-01-01')# "a<=1" "b<=\\"2017-01-01\\""
leq_(letters[1:3], 10:12)   # "a<=10" "b<=11" "c<=12"

gt(a = 1, b = '2017-01-01') # "a>1" "b>\\"2017-01-01\\""
gt_(letters[1:3], 10:12)    # "a>10" "b>11" "c>12"

geq(a = 1, b = '2017-01-01')# "a>=1" "b>=\\"2017-01-01\\""
geq_(letters[1:3], 10:12)   # "a>=10" "b>=11" "c>=12"

in_('a', c(5, 7, 8))        # "a IN (5, 7, 8)"
'a' \%IN\% c(5, 7, 8)         # "a IN (5, 7, 8)"

like_('a', 'Prefix\%')       # "a LIKE(\\"Prefix\%\\")"
'a' \%LIKE\% 'Prefix\%'        # "a LIKE(\\"Prefix\%\\")"
like('a', 'Prefix\%', '\%Suffix')
# "(a LIKE(\\"Prefix\%\\") OR a LIKE(\\"\%Suffix\\"))"

BETWEEN('a', 0, 10)         # "a BETWEEN 0 AND 10"
BETWEEN('timestamp', '2016-01-01', '2017-01-01')
# "`timestamp` BETWEEN \\"2016-01-01\\" AND \\"2017-01-01\\""
}
