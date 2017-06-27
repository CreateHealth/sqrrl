
🐿 sqrrl
-------

`sqrrl` is a small collection of utility functions that help build text-based SQL queries in an R-style native-feeling and functional manner.

Unlike other packages that build SQL queries using an object-oriented style, `sqrrl` provides small functions that produce SQL snippets and can be linked together to compose SQL queries. The result is that the code to produce the SQL statement reads much like the SQL statement iteself. On the other hand, `sqrrl` doesn't know anything about your database and can't help you out with completions, etc.

Installation
------------

``` r
# install.packages('devtools')
devtools::install_github('gadenbuie/sqrrl')
library(sqrrl)
```

Package documentation and overview can be found at <http://gadenbuie.github.io/sqrrl/>.
