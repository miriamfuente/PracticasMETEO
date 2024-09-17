#########################
## auxiliary functions ##
#########################
computeTrend = function(ts) {  # compute trends
  df = data.frame(x = 1:length(ts), y = ts)
  if (sum(is.na(ts)) < round(0.75*length(ts))) {  # ask for a minimum of 75% of non-missing data to compute the trend
    return(lm(y ~ x, df)$coefficients[2])
  } else {
    return(NA)
  }
}
computeSigTrend = function(ts) {  # compute trends
  df = data.frame(x = 1:length(ts), y = ts)
  if (sum(is.na(ts)) < round(0.75*length(ts))) {  # ask for a minimum of 75% of non-missing data to compute the trend
    return(summary(lm(y ~ x, df))$coefficients[,4] [2])
  } else {
    return(NA)
  }
}
