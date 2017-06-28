context("INSERT INTO")

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