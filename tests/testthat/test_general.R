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
