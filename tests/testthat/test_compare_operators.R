context("Comparison Operators")

test_that("%IN% handles strings and numbers", {
  expect_equal('id' %IN% 1:4, "id IN (1, 2, 3, 4)")
  expect_equal('id' %IN% letters[1:2], 'id IN ("a", "b")')
  expect_equal('id' %IN% 'subquery', 'id IN (subquery)')
})

test_that("between gives correct output", {
  expect_equal(BETWEEN('id', 1, 4),
               "id BETWEEN 1 AND 4")
  expect_equal(BETWEEN('time', '2017-01-01', '2017-02-01'),
               'time BETWEEN "2017-01-01" AND "2017-02-01"')
})