# common error handling --------------------------------------------------------
test_that("function throws an error if data is missing", {
  expect_error(eval_forecasts(data = NULL))
})



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
  binary_example <- data.table::setDT(scoringutils2::binary_example_data)
  eval <- eval_forecasts(binary_example,
                         summarise_by = c("model", "value_desc"),
                         quantiles = c(0.5), sd = TRUE,
                         verbose = FALSE)

  # eval2 <- scoringutils::eval_forecasts(binary_example,
  #                                       summarise_by = c("model", "value_desc"),
  #                                       quantiles = c(0.5), sd = TRUE,
  #                                       verbose = FALSE)
  #
  # all(eval == eval2)

  expect_equal(nrow(eval) > 1,
               TRUE)
})


# test quantile case -----------------------------------------------------------
test_that("function produces output for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils2::quantile_example_data)
  eval <- eval_forecasts(quantile_example,
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  # eval2 <- scoringutils::eval_forecasts(quantile_example,
  #                        summarise_by = c("model"),
  #                        quantiles = c(0.5), sd = TRUE)
  #
  # eval2 <- eval2[, .SD, .SDcols = names(eval2)[names(eval2) %in% names(eval)]]
  # all(eval == eval2)

  expect_equal(nrow(eval) > 1,
               TRUE)
})

test_that("calculation of aem is correct for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils2::quantile_example_data)
  eval <- eval_forecasts(quantile_example,
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  ae <- quantile_example[quantile == 0.5, ae := abs(true_value - prediction)
                         ][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
                           by = "model"
                           ]$mean

  expect_equal(sort(eval$aem), sort(ae))
})


# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  example <- data.table::setDT(scoringutils2::continuous_example_data)
  eval <- eval_forecasts(example,
                         summarised = TRUE,
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  # eval2 <- scoringutils::eval_forecasts(example,
  #                                       summarised = TRUE,
  #                        summarise_by = c("model"),
  #                        quantiles = c(0.5), sd = TRUE)
  #
  # setcolorder(eval2, colnames(eval))
  # eval <- eval[order(model)]
  # eval2 <- eval2[order(model)]
  # all(eval == eval2, na.rm = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})


