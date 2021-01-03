test_that("the function returns an output of appropriate length", {
  expect_equal(length(interval_score(true_values = 1:10, upper = 3:12,
                                     lower = 0:9, interval_range = 90)),
               10)
})


test_that("the function throws an error, if argument lengths don't match", {
  expect_error(interval_score(true_values = 1:10, upper = 3:13,
                              lower = 0:9, interval_range = 90))
})

test_that("the function throws an error, if an argument is missing", {
  expect_error(interval_score(upper = 3:12,
                              lower = 0:9, interval_range = 90))
})


