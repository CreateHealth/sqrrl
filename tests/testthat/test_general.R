context("General Operators")

test_that("SELECT", {
  expect_equal(
    c(SELECT(), SELECT(NULL), SELECT_(), SELECT_(NULL)),
    rep("SELECT *", 4)
  )

  expect_equal(SELECT(NULL, 'a'), "SELECT a")
  expect_equal(SELECT_DISTINCT('a'), "SELECT DISTINCT a")
  expect_equal(SELECT(NULL, a = 'apple', NULL, 'b'), "SELECT apple as a, b")
  expect_equal(SELECT(letters[1:3]), "SELECT a, b, c")
  expect_equal(SELECT('a.b'), "SELECT a.b")

  expect_equal(
    SELECT('t1' = letters[1:3], 't2' = letters[4:6]),
    "SELECT t1.a, t1.b, t1.c, t2.d, t2.e, t2.f"
  )

  expect_equal(
    SELECT(letters[1:3], 't2' = letters[4:6]),
    "SELECT a, b, c, t2.d, t2.e, t2.f"
  )

  expect_equal(
    SELECT(a = 'apple', b = 'banana', c = 'cherry'),
    "SELECT apple as a, banana as b, cherry as c"
  )

  expect_equal(
    SELECT('t1' = c(a = 'apple', b = 'banana')),
    'SELECT t1.apple as a, t1.banana as b'
  )

  expect_equal(
    SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry'),
    "SELECT t1.apple as a, t1.banana as b, cherry as c"
  )

  expect_equal(
    SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry', 't2' = c(d = 'dragon_fruit')),
    "SELECT t1.apple as a, t1.banana as b, cherry as c, t2.dragon_fruit as d"
  )

  expect_equal(
    SELECT(z = c('a', f = 'b', 'c'), i = 'd', g = c(h = 'e'), 'j'),
    "SELECT z.a, z.b as f, z.c, d as i, g.e as h, j"
  )
})

test_that("FROM", {
  expect_equal(FROM('table'), "FROM table ")
  expect_equal(FROM(t = 'table'), "FROM table t")
  expect_equal(FROM(t1 = 'table', t2 = 'table2'), "FROM table t1, table2 t2")
  expect_equal(FROM('table', t2 = 'table2'), "FROM table , table2 t2")
})

test_that("AND, OR, LIMIT", {
  expect_equal(AND('cond1'), 'cond1')
  expect_equal(AND('cond1', 'cond2'), 'cond1 AND cond2')
  expect_equal(AND(c('cond1', 'cond2')), 'cond1 AND cond2')

  expect_equal(OR('cond1'), 'cond1')
  expect_equal(OR('cond1', 'cond2'), 'cond1 OR cond2')
  expect_equal(OR(c('cond1', 'cond2')), 'cond1 OR cond2')

  expect_equal(LIMIT(10), "LIMIT 10")
  expect_equal(LIMIT(0), NULL)
  expect_equal(LIMIT(), "LIMIT 1")
  expect_equal(LIMIT(10.5), "LIMIT 10")
})

test_that("GROUP BY", {
  expect_equal(GROUP_BY('col1'),
               "GROUP BY col1")
  expect_equal(GROUP_BY('col1', 'col2', 'col3'),
               "GROUP BY col1, col2, col3")
  expect_equal(GROUP_BY('col1', ASC('col2'), DESC('col3')),
               "GROUP BY col1, col2 ASC, col3 DESC")
})

test_that("JOIN", {
  expect_equal(
    LEFT_JOIN('left_tbl', 'right_tbl', 'id'),
    "LEFT JOIN right_tbl USING (id)")

  expect_error(
    JOIN('left', right_tbls = c('r1', 'r2'), on = list('a', 'b', 'c'))
  )

  expect_equal(
    RIGHT_JOIN('left_tbl', 'right_tbl', 'id', prefer_using = FALSE),
    "RIGHT JOIN right_tbl ON left_tbl.id=right_tbl.id")

  expect_equal(
    INNER_JOIN('left_tbl', 'right_tbl', c('leftID' = 'rightID')),
    "INNER JOIN right_tbl ON left_tbl.leftID=right_tbl.rightID")

  expect_equal(
    OUTER_JOIN('left_tbl', 'right_tbl', c('lid1' = 'rid1', 'id2')),
    "OUTER JOIN right_tbl ON (left_tbl.lid1=right_tbl.rid1 AND left_tbl.id2=right_tbl.id2)")

  expect_equal(
    LEFT_JOIN('lt', c('rt' = 'right_tbl'), 'id'),
    "LEFT JOIN right_tbl rt USING (id)")

  expect_equal(
    LEFT_JOIN('lt', 'right_tbl', 'id'),
    "LEFT JOIN right_tbl USING (id)")

  expect_equal(
    LEFT_JOIN('lt', 'right_tbl', c('id1', 'id2')),
    "LEFT JOIN right_tbl USING (id1, id2)")

  expect_equal(
    LEFT_JOIN('lt', c('right_tbl1', 'right_tbl2'), list('id1', 'id2')),
    "LEFT JOIN (right_tbl1, right_tbl2) ON (lt.id1=right_tbl1.id1 AND lt.id2=right_tbl2.id2)")

  expect_equal(
    LEFT_JOIN('lt', c('rt1' = 'right_tbl1', 'rt2' = 'right_tbl2'), list('id1', 'id2')),
    "LEFT JOIN (right_tbl1 rt1, right_tbl2 rt2) ON (lt.id1=rt1.id1 AND lt.id2=rt2.id2)")

  expect_equal(
    LEFT_JOIN('lt', c('right_tbl1', 'right_tbl2'), list('id')),
    "LEFT JOIN (right_tbl1, right_tbl2) USING (id)")

  expect_equal(
    LEFT_JOIN('lt', c('right_tbl1', 'right_tbl2'), list(c('id1', 'id2'))),
    "LEFT JOIN (right_tbl1, right_tbl2) USING (id1, id2)")

  expect_equal(
    LEFT_JOIN('lt', c('rt1' = 'right_tbl1', 'rt2' = 'right_tbl2'), c('id1', 'id2')),
    "LEFT JOIN (right_tbl1 rt1, right_tbl2 rt2) USING (id1, id2)")

  expect_equal(
    LEFT_JOIN('lt', c('rt1' = 'right_tbl1', 'rt2' = 'right_tbl2'), c('col.1', 'col.2')),
    "LEFT JOIN (right_tbl1 rt1, right_tbl2 rt2) USING (`col.1`, `col.2`)")
})

test_that("INSERT_INTO_VALUES", {
  set.seed(42)
  testdf <- data.frame(
    int = 1:3,
    val = round(rnorm(3), 4),
    str = letters[1:3],
    fct = factor(letters[1:3], levels = letters),
    stringsAsFactors = FALSE
  )

  expect_equal(
    INSERT_INTO_VALUES('table', testdf),
    "INSERT INTO table (`int`, val, str, fct) VALUES (1, 1.371, \"a\", \"a\"), (2, -0.5647, \"b\", \"b\"), (3, 0.3631, \"c\", \"c\")")

  expect_equal(
    INSERT_INTO_VALUES('table', testdf, c('int', 'str')),
    "INSERT INTO table (`int`, str) VALUES (1, \"a\"), (2, \"b\"), (3, \"c\")"
  )

  expect_equal(
    INSERT_INTO_VALUES('table', testdf[1, ], c('val', 'fct')),
    "INSERT INTO table (val, fct) VALUES (1.371, \"a\")"
  )

  expect_equal(
    INSERT_INTO_VALUES('table', c(1:3)),
    "INSERT INTO table  VALUES (1, 2, 3)"
  )

  expect_equal(
    INSERT_INTO_VALUES('table', c('a' = 1, 2, 3), c('a', 'b', 'c')),
    "INSERT INTO table (a, b, c) VALUES (1, 2, 3)"
  )

  colnames(testdf)[1] <- 'an.int'
  expect_equal(
    INSERT_INTO_VALUES('table', testdf, c('an.int', 'str')),
    "INSERT INTO table (`an.int`, str) VALUES (1, \"a\"), (2, \"b\"), (3, \"c\")"
  )
  expect_equal(
    INSERT_INTO_VALUES('iris', c('Petal.Length' = 1.9, 'Petal.Width' = 0.2, 'Species' = 'setosa')),
    "INSERT INTO iris (`Petal.Length`, `Petal.Width`, Species) VALUES (1.9, 0.2, \"setosa\")"
  )

  test_with_missing <- testdf
  test_with_missing[1, 'val'] <- NA
  test_with_missing[2, 'str'] <- ''
  expect_equal(
    INSERT_INTO_VALUES('missing', test_with_missing, c('val', 'str')),
    "INSERT INTO missing (val, str) VALUES (NULL, \"a\"), (-0.5647, \"\"), (0.3631, \"c\")"
  )
})

test_that("UPDATE", {
  expect_equal(
    UPDATE('iris', c(some_column = 1, some_other_col = "high"), eq(another_col = 2), geq(a_third_col = 10)),
    "UPDATE iris  SET some_column=1, some_other_col=\"high\" WHERE another_col=2 AND a_third_col>=10")
  expect_equal(
    UPDATE('t1', c(col1 = 'a')),
    "UPDATE t1  SET col1=\"a\"")
  expect_equal(
    UPDATE('t1', c(col1 = 'a', col2 = 42), 'id' %IN% 1:5),
    "UPDATE t1  SET col1=\"a\", col2=42 WHERE id IN (1, 2, 3, 4, 5)")
  # expect_equal(
  #   UPDATE('t', c(id = 'id + 1'), .order = DESC('id')),
  #   "UPDATE t  SET id=id + 1 ORDER BY id DESC")
  # expect_equal(
  #   UPDATE(c('items', 'month'), c('items.price' = 'month.price'), eq(items.id='month.id')),
  #   "UPDATE items , month  SET items.price=month.price WHERE items.id=month.id"
  # )
})
