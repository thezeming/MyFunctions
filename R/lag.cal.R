#' Computing Optimal Lags for Newey-West tests
#'
#' This function computes the optimal lags for Newey-West tests.
#'
#' @param x The number of periods in the time series.
#' @return The optimal lags for Newey-West tests.

#' @export
lag.cal <- function(x){
  floor(4*(x/100)^(2/9))+1
}
