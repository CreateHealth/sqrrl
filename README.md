🐿 sqrrrl
--------

`sqrrrl` is a small collection of utility functions that help build
text-based SQL queries in an R-style native-feeling and functional
manner.

Unlike other packages that build SQL queries using an object-oriented
style, `sqrrrl` provides small functions that produce SQL snippets and
can be linked together to compose SQL queries. The result is that the
code to produce the SQL statement reads much like the SQL statement
iteself. On the other hand, `sqrrrl` doesn't know anything about your
database and can't help you out with completions, etc.

Quick Overview
--------------

    > library(sqrrrl)
    > 
    > # ---- SELECT ----
    > SELECT()

    [1] "SELECT *"

    > SELECT('col1', 'col2', 'col3')

    [1] "SELECT col1, col2, col3"

    > SELECT(newcol = 'col1', avg_col1 = 'mean(col1)')

    [1] "SELECT col1 as newcol, mean(col1) as avg_col1"

    > # ---- FROM ----
    > FROM('table1')

    [1] "FROM table1 "

    > FROM('table1', 'table2')

    [1] "FROM table1 , table2 "

    > FROM(t1 = 'table1', t2 = 'table2', 'table3')

    [1] "FROM table1 t1, table2 t2, table3 "

    > # ---- WHERE ----
    > WHERE('col1 IS NOT NULL')

    [1] "WHERE col1 IS NOT NULL"

    > WHERE(cond = TRUE, 'col1 = 2', 'col2 >= 10')

    [1] "WHERE col1 = 2 AND col2 >= 10"

    > WHERE(cond = FALSE, 'col1 = 2', 'col2 >= 10')

    [1] ""

    > # ---- IN ----
    > 'id' %IN% paste(SELECT('id'), FROM('other_table'))

    [1] "id IN (SELECT id FROM other_table )"

    > 'id' %IN% 1:4

    [1] "id IN (1, 2, 3, 4)"

    > # ---- LIKE ----
    > 'text_col' %LIKE% 'Prefix%'

    [1] "text_col LIKE(\"Prefix%\")"

    > 'text_col' %LIKE% c('Prefix1%', 'Prefix2%')

    [1] "(text_col LIKE(\"Prefix1%\") OR text_col LIKE(\"Prefix2%\"))"

    > # ---- Comparison Operators ----
    > eq(id = 4)

    [1] "id=4"

    > neq(id = 4)

    [1] "id!=4"

    > lt(id = 4)

    [1] "id<4"

    > leq(id = 4)

    [1] "id<=4"

    > gt(id = 4)

    [1] "id>4"

    > geq(id = 4)

    [1] "id>=4"

    > # ---- Contatenation Operators ----
    > AND(eq(id = 3, class = 'text_value'), geq(date = '2017-06-14'))

    [1] "id=3 AND class=\"text_value\" AND date>=\"2017-06-14\""

    > OR(eq(id = 9, id = 12), leq(id = 5))

    [1] "id=9 OR id=12 OR id<=5"

    > # Concatenate snippets with the %+% infix operator
    > SELECT() %+% FROM('table') %+% WHERE(eq(id = 10))

    [1] "SELECT * FROM table  WHERE id=10"

    > # ---- Complete Example ----
    > (example_query <- 
    +   SELECT('mpg', weight = 'wt', cylinders = 'cyl') %+%
    +   FROM('mtcars') %+%
    +   WHERE(
    +     lt(gear = 4),
    +     geq(mpg = 15),
    +     'cyl' %IN% c(4,6)
    +   ))

    [1] "SELECT mpg, wt as weight, cyl as cylinders FROM mtcars  WHERE gear<4 AND mpg>=15 AND cyl IN (4, 6)"

Formatting SQL Queries
----------------------

`sqrrrl` also provides a simple wrapper for
<https://github.com/andialbrecht/sqlparse>, a Python package for
formatting SQL queries. `sqlparse` can be installed via
`pip install --upgrade sqlparse`, thereafter making available the system
command `sqlformat`.

`sqrrrl::sqlformat()` pretty-prints SQL queries, such as the one above.

    > example <- sqlformat(example_query, header = 'A Beautifully Formatted Example Query')
    > cat(example)

     /* A Beautifully Formatted Example Query */
    SELECT mpg,
           wt AS weight,
           cyl AS cylinders
      FROM mtcars
     WHERE gear<4
       AND mpg>=15
       AND cyl IN (4, 6)
