context("UPDATE Functions")

test_that("UPDATE", {
  expect_equal(
    UPDATE('iris', some_column = 1, some_other_col = "high", .where = c(eq(another_col = 2), geq(a_third_col = 10))),
    "UPDATE iris  SET some_column=1, some_other_col=\"high\" WHERE another_col=2 AND a_third_col>=10")
  expect_equal(
    UPDATE('t1', col1 = 'a'),
    "UPDATE t1  SET col1=\"a\"")
  expect_equal(
    UPDATE('t1', col1 = ~col2),
    "UPDATE t1  SET col1=col2"
  )
  expect_equal(
    UPDATE('t1', col1 = 'a', col2 = 42, .where = 'id' %IN% 1:5),
    "UPDATE t1  SET col1=\"a\", col2=42 WHERE id IN (1, 2, 3, 4, 5)")
  expect_equal(
    UPDATE('t', id = ~id + 1) %+% ORDER_BY(DESC('id')),
    "UPDATE t  SET id=id + 1 ORDER BY id DESC")
  expect_equal(
    # UPDATE(c('items', 'month'), c('items.price' = 'month.price'), eq(items.id='month.id')),
    UPDATE(c('items', 'month'), items.price = ~month.price, .where = eq(items.id = ~month.id)),
    "UPDATE items , month  SET items.price=month.price WHERE items.id=month.id"
  )
})

test_that("SET and SET_", {
  expeq_ignore_warn <- function(obj, exp) expect_equal(suppressWarnings(obj), exp)

  expect_equal(SET(apple = "banana"), "SET apple=\"banana\"")
  expect_equal(SET(apple = ~banana), "SET apple=banana")
  expect_equal(SET(fruit = "banana", cost=~price*0.75, count=12),
               "SET fruit=\"banana\", cost=price * 0.75, `count`=12")
  # Args need to be named
  expect_error(SET('mango'))
  # Args need to be single valued
  expect_warning(SET(apple = c('banana', 'mango')), regexp = 'single-valued')
  expeq_ignore_warn(SET(apple = c('banana', 'mango')), "SET apple=\"banana\"")
  # Args with lists get unlisted
  expect_warning(SET(fruit = list('apple', 'banana')), regex = 'contained a list')
  expeq_ignore_warn(SET(fruit = list('apple', 'banana')), "SET fruit=\"apple\"")
  # Args with lists and entries length > 1 get first of list and first of entry
  expect_warning(SET(fruit = list(c('apple', 'banana'))))
  expeq_ignore_warn(SET(fruit = list(c('apple', 'banana'))), "SET fruit=\"apple\"")
  # Duplicate names are not allowed
  expect_error(SET(fruit = 'apple', fruit = 'banana'))
})