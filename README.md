README
================
Garrick Aden-Buie
2017-06-20

-   [🐿 sqrrrl](#sqrrrl)
-   [Installation](#installation)
-   [Quick Overview](#quick-overview)
    -   [SELECT](#select)
    -   [FROM](#from)
    -   [WHERE](#where)
    -   [IN](#in)
    -   [LIKE](#like)
    -   [Comparison Operators](#comparison-operators)
    -   [Concatenation Operators](#concatenation-operators)
    -   [Complete Example](#complete-example)
-   [Formatting SQL Queries](#formatting-sql-queries)
-   [More Operators and Examples](#more-operators-and-examples)
    -   [Joins](#joins)
    -   [INSERT INTO ... VALUES](#insert-into-...-values)

🐿 sqrrrl
--------

`sqrrrl` is a small collection of utility functions that help build text-based SQL queries in an R-style native-feeling and functional manner.

Unlike other packages that build SQL queries using an object-oriented style, `sqrrrl` provides small functions that produce SQL snippets and can be linked together to compose SQL queries. The result is that the code to produce the SQL statement reads much like the SQL statement iteself. On the other hand, `sqrrrl` doesn't know anything about your database and can't help you out with completions, etc.

Installation
------------

``` r
# install.packages('devtools')
devtools::install_github('gadenbuie/sqrrrl')
library(sqrrrl)
```

Quick Overview
--------------

### SELECT

``` r
> SELECT()
```

    SELECT *

``` r
> SELECT('col1', 'col2', 'col3')
```

    SELECT col1, col2, col3

``` r
> SELECT(newcol = 'col1', avg_col1 = 'mean(col1)')
```

    SELECT col1 AS newcol, mean(col1) AS avg_col1

``` r
> SELECT(letters[1:3], 't2' = letters[4:6])
```

    SELECT a, b, c, t2.d, t2.e, t2.f

``` r
> SELECT(a = 'apple', b = 'banana', c = 'cherry')
```

    SELECT apple AS a, banana AS b, cherry AS c

``` r
> SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry')
```

    SELECT t1.apple AS a, t1.banana AS b, cherry AS c

``` r
> SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry', 't2' = c(d = 'dragon_fruit'))
```

    SELECT t1.apple AS a, t1.banana AS b, cherry AS c, t2.dragon_fruit AS d

### FROM

``` r
> FROM('table1')
```

    FROM table1

``` r
> FROM('table1', 'table2')
```

    FROM table1 , table2

``` r
> FROM(t1 = 'table1', t2 = 'table2', 'table3')
```

    FROM table1 t1, table2 t2, table3

### WHERE

``` r
> WHERE('col1 IS NOT NULL')
```

    WHERE col1 IS NOT NULL

``` r
> WHERE(cond = TRUE, 'col1 = 2', 'col2 >= 10')
```

    WHERE col1 = 2 AND col2 >= 10

``` r
> WHERE(cond = FALSE, 'col1 = 2', 'col2 >= 10')
```

    ""

### IN

``` r
> 'id' %IN% 1:4
```

    id IN (1, 2, 3, 4)

``` r
> 'id' %IN% letters[1:3]
```

    id IN ("a", "b", "c")

``` r
> # Note: if left-hand-side length == 1, then LHS is unquoted (for subqueries)
> 'id' %IN% paste(SELECT('id'), FROM('other_table'))
```

    id IN (SELECT id FROM other_table )

``` r
> 'in' %IN% quotes(letters[1])
```

    IN IN ("a")

### LIKE

``` r
> 'text_col' %LIKE% 'Prefix%'
```

    text_col LIKE("Prefix%")

``` r
> 'text_col' %LIKE% c('Prefix 1%', 'Prefix 2%')
```

    (text_col LIKE("Prefix 1%") OR text_col LIKE("Prefix 2%"))

### Comparison Operators

``` r
> eq(id = 4)
```

    id=4

``` r
> neq(id = 4)
```

    id!=4

``` r
> lt(id = 4)
```

    id<4

``` r
> leq(id = 4)
```

    id<=4

``` r
> gt(id = 4)
```

    id>4

``` r
> geq(id = 4)
```

    id>=4

### Concatenation Operators

``` r
> AND(eq(id = 3, class = 'text_value'), geq(date = '2017-06-14'))
```

    id="3" AND CLASS="text_value" AND date>="2017-06-14"

``` r
> OR(eq(id = 9, id = 12), leq(id = 5))
```

    id=9 OR id=12 OR id<=5

``` r
> # Concatenate snippets with the %+% infix operator
> SELECT() %+% FROM('table') %+% WHERE(eq(id = 10))
```

    SELECT * FROM TABLE  WHERE id=10

### Complete Example

``` r
> (example_query <- 
+   SELECT('mpg', weight = 'wt', cylinders = 'cyl') %+%
+   FROM('mtcars') %+%
+   WHERE(
+     lt(gear = 4),
+     geq(mpg = 15),
+     'cyl' %IN% c(4,6)
+   ))
```

    SELECT mpg, wt AS weight, cyl AS cylinders FROM mtcars  WHERE gear<4 AND mpg>=15 AND cyl IN (4, 6)

Formatting SQL Queries
----------------------

`sqrrrl` also provides a simple wrapper for <https://github.com/andialbrecht/sqlparse>, a Python package for formatting SQL queries. `sqlparse` can be installed via `pip install --upgrade sqlparse`, thereafter making available the system command `sqlformat`.

`sqrrrl::sqlformat()` pretty-prints SQL queries, such as the one above.

``` r
> example <- sqlformat(example_query, header = 'A Beautifully Formatted Example Query')
> cat(example)
```

``` sql
 /* A Beautifully Formatted Example Query */
SELECT mpg,
       wt AS weight,
       cyl AS cylinders
  FROM mtcars
 WHERE gear<4
   AND mpg>=15
   AND cyl IN (4, 6)
```

More Operators and Examples
---------------------------

### Joins

``` r
> # Using one ID
> JOIN(left_ref = 'left_tbl', right_tbls = 'right_tbl', on = 'id')
```

     JOIN right_tbl USING (id)

``` r
> LEFT_JOIN('l', c('r' = 'right_tbl'), 'id')
```

    LEFT JOIN right_tbl r USING (id)

``` r
> RIGHT_JOIN('l', c('r' = 'right_tbl'), 'id', prefer_using = FALSE)
```

    RIGHT JOIN right_tbl r ON l.id=r.id

``` r
> # Join on multiple columns, with different names on left and right
> JOIN(type = 'natural right', 'left_tbl', 'right_tbl', c('left.col1' = 'right.col1', 'id2'))
```

    NATURAL RIGHT JOIN right_tbl ON (left_tbl.`left.col1`=right_tbl.`right.col1` AND left_tbl.id2=right_tbl.id2)

``` r
> # Join multiple tables on same column
> INNER_JOIN('left_tbl', c('right_1', 'right_2'), 'id_col')
```

    INNER JOIN (right_1, right_2) USING (id_col)

``` r
> # Join multiple tables on different columns
> OUTER_JOIN('l', c(r1 = 'right_1', r2 = 'right_2'), list('col1', 'col2'))
```

    OUTER JOIN (right_1 r1, right_2 r2) ON (l.col1=r1.col1 AND l.col2=r2.col2)

``` r
> # Join multiple tables on different coluns with different column names
> JOIN('l', c(r1 = 'right_1', r2 = 'right_2'), list(c(right_1_id = 'id', c(right_2_id = 'id'))))
```

     JOIN (right_1 r1, right_2 r2) ON (l.right_1_id=r1.id AND l.right_2_id=r1.id AND l.right_1_id=r2.id AND l.right_2_id=r2.id)

### INSERT INTO ... VALUES

``` r
> iris_example <- iris[c(1, 51, 101), ]
> # Insert all rows & columns from a data.frame
> INSERT_INTO_VALUES('iris', iris_example)
```

    INSERT INTO iris (`Sepal.Length`, `Sepal.Width`, `Petal.Length`, `Petal.Width`, Species) VALUES (5.1, 3.5, 1.4, 0.2, "setosa"), (7, 3.2, 4.7, 1.4, "versicolor"), (6.3, 3.3, 6, 2.5, "virginica")

``` r
> # Insert select columns from a data.frame
> INSERT_INTO_VALUES('iris', iris_example, c('Petal.Length', 'Petal.Width', 'Species'))
```

    INSERT INTO iris (`Petal.Length`, `Petal.Width`, Species) VALUES (1.4, 0.2, "setosa"), (4.7, 1.4, "versicolor"), (6, 2.5, "virginica")

``` r
> # Insert named vector
> INSERT_INTO_VALUES('iris', c('Petal.Length' = 1.9, 'Petal.Width' = 0.2, 'Species' = 'setosa'))
```

    INSERT INTO iris (`Petal.Length`, `Petal.Width`, Species) VALUES (1.9, 0.2, "setosa")

``` r
> # Insert subset of named vector
> INSERT_INTO_VALUES('iris', c('Petal.Length' = 1.9, 'Petal.Width' = 0.2, 'Species' = 'setosa'),
+                    cols = c('Petal.Width', 'Species'))
```

    INSERT INTO iris (`Petal.Width`, Species) VALUES (0.2, "setosa")

``` r
> # Insert just vector of mixed type without column names
> INSERT_INTO_VALUES('iris', c(6.5, 3.2, 5.1, 2, 'virginica'))
```

    INSERT INTO iris  VALUES (6.5, 3.2, 5.1, 2, "virginica")
