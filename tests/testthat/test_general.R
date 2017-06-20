context("General Operators")

test_that("SELECT", {
  expect_equal(
    c(SELECT(), SELECT(NULL), SELECT_(), SELECT_(NULL)),
    rep("SELECT *", 4)
  )

  expect_equal(
    SELECT(NULL, 'a'),
    "SELECT a"
  )

  expect_equal(
    SELECT(NULL, a = 'apple', NULL, 'b'),
    "SELECT apple as a, b"
  )

  expect_equal(
    SELECT(letters[1:3]),
    "SELECT a, b, c"
  )

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

test_that("GROUP BY", {
  expect_equal(GROUP_BY('col1'),
               "GROUP BY col1")
  expect_equal(GROUP_BY('col1', 'col2', 'col3'),
               "GROUP BY col1, col2, col3")
})

test_that("JOIN", {
  expect_equal(
    LEFT_JOIN('left_tbl', 'right_tbl', 'id'),
    "LEFT JOIN right_tbl USING (id)")

  expect_equal(
    LEFT_JOIN('left_tbl', 'right_tbl', 'id', prefer_using = FALSE),
    "LEFT JOIN right_tbl ON left_tbl.id=right_tbl.id")

  expect_equal(
    LEFT_JOIN('left_tbl', 'right_tbl', c('leftID' = 'rightID')),
    "LEFT JOIN right_tbl ON left_tbl.leftID=right_tbl.rightID")

  expect_equal(
    LEFT_JOIN('left_tbl', 'right_tbl', c('lid1' = 'rid1', 'id2')),
    "LEFT JOIN right_tbl ON (left_tbl.lid1=right_tbl.rid1 AND left_tbl.id2=right_tbl.id2)")

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
    fct = factor(letters[1:3], levels = letters)
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
})
