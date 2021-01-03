eval_forecasts_sample <- function(data,
                                  by,
                                  summarise_by,
                                  metrics,
                                  quantiles,
                                  sd,
                                  prediction_type,
                                  pit_plots,
                                  pit_arguments,
                                  summarised,
                                  verbose) {

  # calculate scores -----------------------------------------------------------
  # sharpness
  if ("sharpness" %in% metrics) {
    data[, sharpness := scoringutils::sharpness(t(prediction)), by = c(by)]
  }
  # bias
  if ("bias" %in% metrics) {
    data[, bias := scoringutils::bias(unique(true_value),
                                      t(prediction)), by = c(by)]
  }
  # DSS
  if ("dss" %in% metrics) {
    data[, dss := scoringutils::dss(unique(true_value),
                                    t(prediction)), by = c(by)]
  }
  # CRPS
  if ("crps" %in% metrics) {
    data[, crps := scoringutils::crps(unique(true_value),
                                      t(prediction)), by = c(by)]
  }
  # Log Score
  if ("log_score" %in% metrics) {
    # only compute if prediction type is continuous
    if (prediction_type == "continuous") {
      data[, log_score := scoringutils::logs(unique(true_value),
                                             t(prediction)), by = c(by)]
    }
  }
  # coverage
  if ("coverage" %in% metrics) {
  }


  # Compute PIT if specified ---------------------------------------------------
  if (any(grepl("pit", metrics)) || pit_plots) {
    # split data into chunks as determined by summarise_by, since we need to
    # get one PIT per element of summarise_by
    split_data <- split(data, by = summarise_by)

    # calculate pit for every element of the split data.frame
    pits <- lapply(split_data,
                   FUN = pit_df, plot = pit_plots)

    # extract data frames with added p-values. Bind data together again
    data_with_pit_values <- extract_from_list(pits, "data")
    data <- rbindlist(data_with_pit_values)

    if (pit_plots) {
      # extract pit histograms if plots are desired
      pit_histograms <- extract_from_list(pits, "hist_PIT")

      # add another histogram for the entire data set
      pit_histograms[["overall_pit"]] <- pit_df(data)$hist_PIT
    }
  }

  res <- data

  # make scores unique to avoid redundancy.
  res <- res[, lapply(.SD, unique),
             .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score|pit",
             by = c(by)]

  # summarise output if desired ------------------------------------------------
  if (summarised) {
    # add quantiles
    if (!is.null(quantiles)) {
      quantile_vars <- c("crps", "dss", "log_score", "pit_p_val", "bias", "sharpness")
      res <- add_quantiles(res, quantile_vars, quantiles, by = c(summarise_by))
    }

    if (sd) {
      # add standard deviations
      sd_vars <- c("crps", "dss", "log_score", "bias", "sharpness")
      res <- add_sd(res, sd_vars, by = c(summarise_by))
    }

    # take mean
    res <- res[, lapply(.SD, mean, na.rm = TRUE),
               .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score",
               by = summarise_by]
  }


  # if pit_plots is TRUE, add the plots as an output ---------------------------
  if (pit_plots) {
    res <- list(scores = res,
                pit_plots = pit_histograms)
  }

  return(res)
}
