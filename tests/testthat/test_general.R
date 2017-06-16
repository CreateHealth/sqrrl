context("General Operators")

test_that("GROUP BY", {
  expect_equal(GROUP_BY('col1'),
               "GROUP BY col1")
  expect_equal(GROUP_BY('col1', 'col2', 'col3'),
               "GROUP BY col1, col2, col3")
})

test_that("JOIN", {
  expect_equal(
    JOIN('left', 'left_tbl', 'right_tbl', 'id'),
    "LEFT JOIN right_tbl USING (id)")

  expect_equal(
    JOIN('left', 'left_tbl', 'right_tbl', 'id', prefer_using = FALSE),
    "LEFT JOIN right_tbl ON left_tbl.id=right_tbl.id")

  expect_equal(
    JOIN('left', 'left_tbl', 'right_tbl', c('leftID' = 'rightID')),
    "LEFT JOIN right_tbl ON left_tbl.leftID=right_tbl.rightID")

  expect_equal(
    JOIN('left', 'left_tbl', 'right_tbl', c('lid1' = 'rid1', 'id2')),
    "LEFT JOIN right_tbl ON (left_tbl.lid1=right_tbl.rid1 AND left_tbl.id2=right_tbl.id2)")

  expect_equal(
    JOIN('left', 'lt', c('rt' = 'right_tbl'), 'id'),
    "LEFT JOIN right_tbl rt USING (id)")

  expect_equal(
    JOIN('left', 'lt', 'right_tbl', 'id'),
    "LEFT JOIN right_tbl USING (id)")

  expect_equal(
    JOIN('left', 'lt', 'right_tbl', c('id1', 'id2')),
    "LEFT JOIN right_tbl USING (id1, id2)")

  expect_equal(
    JOIN('left', 'lt', c('right_tbl1', 'right_tbl2'), list('id1', 'id2')),
    "LEFT JOIN (right_tbl1, right_tbl2) ON (lt.id1=right_tbl1.id1 AND lt.id2=right_tbl2.id2)")

  expect_equal(
    JOIN('left', 'lt', c('rt1' = 'right_tbl1', 'rt2' = 'right_tbl2'), list('id1', 'id2')),
    "LEFT JOIN (right_tbl1 rt1, right_tbl2 rt2) ON (lt.id1=rt1.id1 AND lt.id2=rt2.id2)")

  expect_equal(
    JOIN('left', 'lt', c('right_tbl1', 'right_tbl2'), list('id')),
    "LEFT JOIN (right_tbl1, right_tbl2) USING (id)")

  expect_equal(
    JOIN('left', 'lt', c('right_tbl1', 'right_tbl2'), list(c('id1', 'id2'))),
    "LEFT JOIN (right_tbl1, right_tbl2) USING (id1, id2)")

  expect_equal(
    JOIN('left', 'lt', c('rt1' = 'right_tbl1', 'rt2' = 'right_tbl2'), c('id1', 'id2')),
    "LEFT JOIN (right_tbl1 rt1, right_tbl2 rt2) USING (id1, id2)")
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
    "INSERT INTO table (int, val, str, fct) VALUES (1, 1.371, \"a\", \"a\"), (2, -0.5647, \"b\", \"b\"), (3, 0.3631, \"c\", \"c\")")

  expect_equal(
    INSERT_INTO_VALUES('table', testdf, c('int', 'str')),
    "INSERT INTO table (int, str) VALUES (1, \"a\"), (2, \"b\"), (3, \"c\")"
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
})
