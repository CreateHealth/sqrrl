context("Comparison Operators")

test_that("%IN% handles strings and numbers", {
  expect_equal('id' %IN% 1:4, "id IN (1, 2, 3, 4)")
  expect_equal('id' %IN% letters[1:2], 'id IN ("a", "b")')
  expect_equal('id' %IN% 'subquery', 'id IN (subquery)')
  expect_equal(1 %IN% 1:3, "`1` IN (1, 2, 3)")
})

test_that("between gives correct output", {
  expect_equal(BETWEEN('id', 1, 4),
               "id BETWEEN 1 AND 4")
  expect_equal(BETWEEN('time', '2017-01-01', '2017-02-01'),
               '`time` BETWEEN "2017-01-01" AND "2017-02-01"')
  expect_equal(BETWEEN('time', as.Date('2017-01-01'), as.Date('2017-02-01')),
               '`time` BETWEEN "2017-01-01" AND "2017-02-01"')

  start <- strptime("2017-01-01 11:00", "%F %H:%M")
  end   <- strptime("2017-01-01 13:00", "%F %H:%M")
  expect_equal(BETWEEN('time', start, end),
               '`time` BETWEEN "2017-01-01 11:00:00" AND "2017-01-01 13:00:00"')
})