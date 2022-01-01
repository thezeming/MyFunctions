#' Summary statistics for a single series
#'
#' This function computes the summary statistics for a single series.
#'
#' @param single_series A series of data.
#'
#' @param scaler An annualisation scaler applied to the stats computed from the raw data, if Scaled is TRUE.
#' The default value is 252.
#'
#' @param rounding_decimal A positive integer indicating how many decimals to keep, if Rounding is TRUE.
#'
#' @param Scaled A Boolean value specifying whether to scale the stats computed from the raw data.
#'
#' @param SharpeR A Boolean value specifying whether to include Sharpe ratio.
#'
#' @param NWTstat A Boolean value specifying whether to apply Newey-West correction.
#'
#' @param Rounding A Boolean value specifying whether to round the results.
#'
#' @param Staring A Boolean value specifying whether to give stars to MEAN that indicate statistical significance.
#'
#' @return A dataframe that contains summary statistics.

#' @export
single.series.summary.stat <- function(single_series,
                                       scaler = 252,
                                       rounding_decimal = 2,
                                       Scaled = TRUE,
                                       SharpeR = TRUE,
                                       NWTstat = TRUE,
                                       Rounding = TRUE,
                                       Staring = TRUE){
  single_series <- as.vector(single_series)
  No_Obs <- length(single_series)
  No_NAs <- sum(is.na(single_series))
  single_series_na_removed <- single_series[!is.na(single_series)]
  if(length(single_series_na_removed) == 0){
    MEAN <- NA
    t <- NA
    SD <- NA
    Sharpe <- NA
    Skewness <- NA
    Kurtosis <- NA
    MAX <- NA
    MIN <- NA
    MEDIAN <- NA
  }else{
    MEAN <- mean(single_series_na_removed)
    t <- Newey.west.one.sample.t(single_series_na_removed)
    SD <- sd(single_series_na_removed)
    Sharpe <- MEAN/SD
    Skewness <- EnvStats::skewness(as.numeric(single_series_na_removed))
    Kurtosis <- EnvStats::kurtosis(as.numeric(single_series_na_removed),
                         excess = FALSE)
    MAX <- max(single_series_na_removed)
    MIN <- min(single_series_na_removed)
    MEDIAN <- median(single_series_na_removed)
  }



  if(Scaled){
    MEAN <- MEAN * scaler
    SD <- SD * sqrt(scaler)
    Sharpe <- MEAN/SD
    # Strategic Asset Allocation and the Role of Alternative Investments, EFM, Appendix B.
    Skewness <- Skewness / sqrt(scaler)
    Kurtosis <- (Kurtosis / scaler) + ((ncol(combn(scaler,2)) * ncol(combn(4,2))) / scaler^2)

    MAX <- MAX * scaler
    MIN <- MIN * scaler
    MEDIAN <- MEDIAN * scaler
  }

  result <- as.data.frame(rbind(No_Obs,
                                No_NAs,
                                MEAN,
                                MEDIAN,
                                MIN,
                                MAX,
                                SD,
                                Skewness,
                                Kurtosis))
  if(SharpeR){
    result['Sharpe', ] <- Sharpe
  }
  if(NWTstat){
    result['NWTstatOfMean', ] <- t
  }

  if(length(single_series_na_removed) == 0 | !Rounding){
    return(result)
  }else{
    result['MEAN', ] <- format(round(as.numeric(result['MEAN', ]),
                                     rounding_decimal),
                               nsmall = rounding_decimal)
    result['MAX', ] <- format(round(as.numeric(result['MAX', ]),
                                    rounding_decimal),
                              nsmall = rounding_decimal)
    result['MIN', ] <- format(round(as.numeric(result['MIN', ]),
                                    rounding_decimal),
                              nsmall = rounding_decimal)
    result['MEDIAN', ] <- format(round(as.numeric(result['MEDIAN', ]),
                                       rounding_decimal),
                                 nsmall = rounding_decimal)
    result['SD', ] <- format(round(as.numeric(result['SD', ]),
                                   rounding_decimal),
                             nsmall = rounding_decimal)
    result['Skewness', ] <- format(round(as.numeric(result['Skewness', ]),
                                         rounding_decimal),
                                   nsmall = rounding_decimal)
    result['Kurtosis', ] <- format(round(as.numeric(result['Kurtosis', ]),
                                         rounding_decimal),
                                   nsmall = rounding_decimal)
    if(SharpeR){
      result['Sharpe', ] <- format(round(as.numeric(result['Sharpe', ]),
                                         rounding_decimal),
                                   nsmall = rounding_decimal)
    }
    if(NWTstat){
      result['NWTstatOfMean', ] <- c(paste('(',
                                           format(round(as.numeric(result['NWTstatOfMean', ]),
                                                        2),
                                                  nsmall = 2),
                                           ')',
                                           sep = ''))
    }
    if(Staring){
      if(t > -1.645 & t < 1.645){
      }else if(t > -1.96 & t < 1.96){
        result['MEAN', ] <- paste0(result['MEAN', ],'*')
      }else if(t > -2.576 & t < 2.576){
        result['MEAN', ] <- paste0(result['MEAN', ],'**')
      }else{
        result['MEAN', ] <- paste0(result['MEAN', ],'***')
      }
    }

    return(result)
  }

}
