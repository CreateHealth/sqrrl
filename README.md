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

Installation
------------

    # install.packages('devtools')
    devtools::install_github('gadenbuie/sqrrrl')

Quick Overview
--------------

    library(sqrrrl)

    # ---- SELECT ----
    SELECT()

    ## [1] "SELECT *"

    SELECT('col1', 'col2', 'col3')

    ## [1] "SELECT col1, col2, col3"

    SELECT(newcol = 'col1', avg_col1 = 'mean(col1)')

    ## [1] "SELECT col1 as newcol, mean(col1) as avg_col1"

    SELECT(letters[1:3], 't2' = letters[4:6])

    ## [1] "SELECT a, b, c, t2.d, t2.e, t2.f"

    SELECT(a = 'apple', b = 'banana', c = 'cherry')

    ## [1] "SELECT apple as a, banana as b, cherry as c"

    SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry')

    ## [1] "SELECT t1.apple as a, t1.banana as b, cherry as c"

    SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry', 't2' = c(d = 'dragon_fruit'))

    ## [1] "SELECT t1.apple as a, t1.banana as b, cherry as c, t2.dragon_fruit as d"

    # ---- FROM ----
    FROM('table1')

    ## [1] "FROM table1 "

    FROM('table1', 'table2')

    ## [1] "FROM table1 , table2 "

    FROM(t1 = 'table1', t2 = 'table2', 'table3')

    ## [1] "FROM table1 t1, table2 t2, table3 "

    # ---- WHERE ----
    WHERE('col1 IS NOT NULL')

    ## [1] "WHERE col1 IS NOT NULL"

    WHERE(cond = TRUE, 'col1 = 2', 'col2 >= 10')

    ## [1] "WHERE col1 = 2 AND col2 >= 10"

    WHERE(cond = FALSE, 'col1 = 2', 'col2 >= 10')

    ## [1] ""

    # ---- IN ----
    'id' %IN% 1:4

    ## [1] "id IN (1, 2, 3, 4)"

    'id' %IN% letters[1:3]

    ## [1] "id IN (\"a\", \"b\", \"c\")"

    # Note: if left-hand-side length == 1, then LHS is unquoted (for subqueries)
    'id' %IN% paste(SELECT('id'), FROM('other_table'))

    ## [1] "id IN (SELECT id FROM other_table )"

    'in' %IN% quotes(letters[1])

    ## [1] "in IN (\"a\")"

    # ---- LIKE ----
    'text_col' %LIKE% 'Prefix%'

    ## [1] "text_col LIKE(\"Prefix%\")"

    'text_col' %LIKE% c('Prefix1%', 'Prefix2%')

    ## [1] "(text_col LIKE(\"Prefix1%\") OR text_col LIKE(\"Prefix2%\"))"

    # ---- Comparison Operators ----
    eq(id = 4)

    ## [1] "id=4"

    neq(id = 4)

    ## [1] "id!=4"

    lt(id = 4)

    ## [1] "id<4"

    leq(id = 4)

    ## [1] "id<=4"

    gt(id = 4)

    ## [1] "id>4"

    geq(id = 4)

    ## [1] "id>=4"

    # ---- Contatenation Operators ----
    AND(eq(id = 3, class = 'text_value'), geq(date = '2017-06-14'))

    ## [1] "id=\"3\" AND class=\"text_value\" AND date>=\"2017-06-14\""

    OR(eq(id = 9, id = 12), leq(id = 5))

    ## [1] "id=9 OR id=12 OR id<=5"

    # Concatenate snippets with the %+% infix operator
    SELECT() %+% FROM('table') %+% WHERE(eq(id = 10))

    ## [1] "SELECT * FROM table  WHERE id=10"

    # ---- Complete Example ----
    (example_query <- 
      SELECT('mpg', weight = 'wt', cylinders = 'cyl') %+%
      FROM('mtcars') %+%
      WHERE(
        lt(gear = 4),
        geq(mpg = 15),
        'cyl' %IN% c(4,6)
      ))

    ## [1] "SELECT mpg, wt as weight, cyl as cylinders FROM mtcars  WHERE gear<4 AND mpg>=15 AND cyl IN (4, 6)"

Formatting SQL Queries
----------------------

`sqrrrl` also provides a simple wrapper for
<https://github.com/andialbrecht/sqlparse>, a Python package for
formatting SQL queries. `sqlparse` can be installed via
`pip install --upgrade sqlparse`, thereafter making available the system
command `sqlformat`.

`sqrrrl::sqlformat()` pretty-prints SQL queries, such as the one above.

    example <- sqlformat(example_query, header = 'A Beautifully Formatted Example Query')
    cat(example)

     /* A Beautifully Formatted Example Query */
    SELECT mpg,
           wt AS weight,
           cyl AS cylinders
      FROM mtcars
     WHERE gear<4
       AND mpg>=15
       AND cyl IN (4, 6)

More Operators and Examples
---------------------------

    # ---- Joins ----
    # Using one ID
    JOIN('left_tbl', 'right_tbl', 'id')

    ## [1] " JOIN right_tbl USING (id)"

    LEFT_JOIN('l', c('r' = 'right_tbl'), 'id')

    ## [1] "LEFT JOIN right_tbl r USING (id)"

    RIGHT_JOIN('l', c('r' = 'right_tbl'), 'id', prefer_using = FALSE)

    ## [1] "RIGHT JOIN right_tbl r ON l.id=r.id"

    # Join on multiple columns, with different names on left and right
    JOIN(type = 'natural right', 'left_tbl', 'right_tbl', c('left.col1' = 'right.col1', 'id2'))

    ## [1] "NATURAL RIGHT JOIN right_tbl ON (left_tbl.`left.col1`=right_tbl.`right.col1` AND left_tbl.id2=right_tbl.id2)"

    # Join multiple tables on same column
    INNER_JOIN('left_tbl', c('right_1', 'right_2'), 'id_col')

    ## [1] "INNER JOIN (right_1, right_2) USING (id_col)"

    # Join multiple tables on different columns
    OUTER_JOIN('l', c(r1 = 'right_1', r2 = 'right_2'), list('col1', 'col2'))

    ## [1] "OUTER JOIN (right_1 r1, right_2 r2) ON (l.col1=r1.col1 AND l.col2=r2.col2)"

    # Join multiple tables on different coluns with different column names
    JOIN('l', c(r1 = 'right_1', r2 = 'right_2'), list(c(right_1_id = 'id', c(right_2_id = 'id'))))

    ## [1] " JOIN (right_1 r1, right_2 r2) ON (l.right_1_id=r1.id AND l.right_2_id=r1.id AND l.right_1_id=r2.id AND l.right_2_id=r2.id)"

    # ---- INSERT INTO ... VALUES ----
    iris_example <- iris[c(1, 51, 101), ]
    # Insert all rows & columns from a data.frame
    INSERT_INTO_VALUES('iris', iris_example)

    ## [1] "INSERT INTO iris (`Sepal.Length`, `Sepal.Width`, `Petal.Length`, `Petal.Width`, Species) VALUES (5.1, 3.5, 1.4, 0.2, \"setosa\"), (7, 3.2, 4.7, 1.4, \"versicolor\"), (6.3, 3.3, 6, 2.5, \"virginica\")"

    # Insert select columns from a data.frame
    INSERT_INTO_VALUES('iris', iris_example, c('Petal.Length', 'Petal.Width', 'Species'))

    ## [1] "INSERT INTO iris (`Petal.Length`, `Petal.Width`, Species) VALUES (1.4, 0.2, \"setosa\"), (4.7, 1.4, \"versicolor\"), (6, 2.5, \"virginica\")"

    # Insert named vector
    INSERT_INTO_VALUES('iris', c('Petal.Length' = 1.9, 'Petal.Width' = 0.2, 'Species' = 'setosa'))

    ## [1] "INSERT INTO iris (`Petal.Length`, `Petal.Width`, Species) VALUES (1.9, 0.2, \"setosa\")"

    # Insert subset of named vector
    INSERT_INTO_VALUES('iris', c('Petal.Length' = 1.9, 'Petal.Width' = 0.2, 'Species' = 'setosa'),
                       cols = c('Petal.Width', 'Species'))

    ## [1] "INSERT INTO iris (`Petal.Width`, Species) VALUES (0.2, \"setosa\")"

    # Insert just vector of mixed type without column names
    INSERT_INTO_VALUES('iris', c(6.5, 3.2, 5.1, 2, 'virginica'))

    ## [1] "INSERT INTO iris  VALUES (6.5, 3.2, 5.1, 2, \"virginica\")"
