eval_forecasts_binary <- function(data,
                                  by,
                                  summarise_by,
                                  metrics,
                                  quantiles,
                                  sd,
                                  summarised,
                                  verbose){

  res <- data[, "brier_score" := scoringutils::brier_score(true_value, prediction),
              by = by]

  if (summarised) {
    # add quantiles
    if (!is.null(quantiles)) {
      res <- add_quantiles(res, "brier_score", quantiles, by = summarise_by)
    }

    # add standard deviation
    if (sd) {
      res[, "brier_score_sd" := sd(brier_score, na.rm = TRUE), by = c(summarise_by)]
    }

    # summarise by taking the mean over all relevant columns
    res <- data[, lapply(.SD, mean, na.rm = TRUE),
                .SDcols = colnames(res) %like% "brier",
                by = summarise_by]

  }
}


