#' Newey-West One Sample t-stat
#'
#' This function computes the Newey-West One Sample t-stat for a single series.
#' The optimal lag is computed by the function lag.cal.
#'
#' @param dat A series of data
#' @param na.rm Logical. If TRUE, missing values are removed.
#' @return The Newey-West t-stat

#' @export
Newey.west.one.sample.t <- function(dat, na.rm = FALSE){
  if(na.rm == TRUE){
    dat <- na.omit(dat)
  }
  if(length(dat) < 2){
    stop("The length of the data must be greater than 1.")
  }
  time_series_vector <- dat %>% as.vector()
  market_timing_ave_t_regression <- lm(time_series_vector~1)

  t <- lmtest::coeftest(market_timing_ave_t_regression, vcov. = sandwich::NeweyWest(market_timing_ave_t_regression,
                                                                  lag = lag.cal(length(dat|> na.omit())),
                                                                  prewhite = FALSE))[1,3]
  return(t)
}
