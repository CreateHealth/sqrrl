context("Wrapping Functions")

test_that("sql_typeof gives correct values", {
  int    <- 1498572803L
  int_s  <- "1498572803"
  num    <- 14985.72803
  num_s  <- "14985.72803"
  str    <- "test"
  factor <- factor('g', levels = letters)
  date   <- Sys.Date()
  posixlt <- Sys.time()
  posixct <- as.POSIXct(posixlt)


  expect_sql_type <- function(x, type) expect_equal(sql_typeof(x), type)

  expect_sql_type(int, 'number')
  expect_sql_type(int_s, 'number')
  expect_sql_type(num, 'number')
  expect_sql_type(num_s, 'number')
  expect_sql_type(str, 'string')
  expect_sql_type(factor, 'factor')
  expect_sql_type(date, 'Date')
  expect_sql_type(posixlt, 'POSIXt')
  expect_sql_type(posixct, 'POSIXt')
})

