#' Return calculation
#'
#' This function computes the returns from a timestamped price series.
#' This function matches p_1 and p_2 by taking advantage of merge.xts, thus missing timestamps are easily dealt with.
#'
#' @param timestamped_DATA An xts object that is a price series of which the index has class xts, Date, or yearmon.
#'
#' @param DATA_freq A character specifying the underlying frequency of data.
#' For example, to comput half-hour returns from minutely stamped data, this value would be 'xts_minutely'.
#' possible values include 'xts_minutely', 'Date_daily', 'yearmon_monthly'.
#'
#' @param x A positive integer indicating how long the interval, over which the return is computed, is.
#' For example, to comput half-hour returns from minutely stamped data, this value would be 30.
#'
#' @param PriceColumeName The colunmn name of the price series.
#' If none, use integer 1.
#'
#' @param style A character indicating whether to compute simple or log returns.
#' Possible values are 'simple' and 'log'.
#'
#' @return A xts series of returns.

#' @export
x.intervals.return.cal <- function(timestamped_DATA,
                                   DATA_freq = 'xts_minutely',
                                     x,
                                     PriceColumeName,
                                   style = 'simple'){
  if(DATA_freq == 'xts_minutely'){
    DATA_t0 <- xts::xts(timestamped_DATA, order.by = index(timestamped_DATA) +60*x)
  }else if(DATA_freq == 'Date_daily'){
    DATA_t0 <- xts::xts(timestamped_DATA, order.by = index(timestamped_DATA) +1*x)
  }else if(DATA_freq == 'yearmon_monthly'){
    DATA_t0 <- xts::xts(timestamped_DATA, order.by = index(timestamped_DATA) +1/12*x)
  }else{
    stop('Argument \'DATA_freq\' has to be \'xts_minutely\', \'Date_daily\', or \'yearmon_monthly\'.')
  }

  DATA_t1 <- timestamped_DATA

  DATA1 <- xts::merge.xts(DATA_t0[,PriceColumeName],
                     DATA_t1[,PriceColumeName])
  colnames(DATA1) <- c('t0', 't1')

  if (style == 'simple'){
    returns <- DATA1$t1 / DATA1$t0 - 1
  }else if(style == 'log'){
    returns <- log(DATA1$t1 / DATA1$t0)
  }else{
    stop('Argument \'style\' has to be either \'simple\' or \'log\'.')
  }


  return(returns)
}
