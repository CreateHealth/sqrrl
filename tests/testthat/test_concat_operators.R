context("Concatenation Operators")

test_that("%IN% handles strings and numbers", {
  expect_equal('id' %IN% 1:4, "id IN (1, 2, 3, 4)")
  expect_equal('id' %IN% letters[1:2], 'id IN ("a", "b")')
  expect_equal('id' %IN% 'subquery', 'id IN (subquery)')
})