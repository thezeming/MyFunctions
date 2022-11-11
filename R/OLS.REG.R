#' Simple OLS Regressions
#'
#' This function runs simple OLS regressions and return the results
#'
#' @param multicolumn_data The data that goes into the regression, with the first column being the dependent variable.
#'
#' @param annualisation_factor The multiplier used to annualise the data.
#' Effectively this value is multiplied with the alpha from the regression using original data.
#' The default value is 252.
#'
#' @param rounding_decimal An positive integer indicating how many decimal to keep in the reported results.
#'
#' @param scalar A scalar applied to the betas. The default value is 1.
#'
#' @param star_paren A Boolean value, indicating whether to give stars to the coefficients.
#' Note, if TRUE, this changes the results from numeric to character.
#'
#' @param NeweyWest A Boolean value, indicating whether Newey-West t values are used. Lags are computed using lag.cal.R. The default is FALSE.
#'
#' @return A dataframe that reports the regression results.
#'
#'
#' @export
OLS.REG <- function(multicolumn_data,
                    annualisation_factor = 252,
                    rounding_decimal = 2,
                    scalar =1,
                    star_paren = TRUE,
                    NeweyWest = FALSE){
  if(nrow(na.omit(multicolumn_data)) < (multicolumn_data %>% ncol + 1)){
    stop('Not enough non na observations.')
  }
  DATA <- as.data.frame(multicolumn_data)
  IndVarNames <- colnames(DATA)[-1]
  number_of_columns <- ncol(DATA)
  regression <- lm(DATA)
  sum_reg <- summary(regression)


  Adj.RSQ <- sum_reg$adj.r.squared

  result_colnames <- c('Intercept',
                       IndVarNames,
                       'Adj.R^2(%)')

  result <- matrix(NA,
                   nrow = 2,
                   ncol = length(result_colnames))
  colnames(result) <- result_colnames
  rownames(result) <- c('estimates', 't')
  result <- as.data.frame(result)

  ###### filling results #################
  result['estimates',] <- c(regression$coefficients, Adj.RSQ*100)*c(annualisation_factor, rep(scalar,number_of_columns-1), 1)
  if(NeweyWest){
    sum_reg_NW <- lmtest::coeftest(regression, vcov. = sandwich::NeweyWest(regression,
                                                                           lag = lag.cal(nrow(na.omit(DATA))),
                                                                           prewhite = FALSE))
    result['t', ] <- c(sum_reg_NW[,'t value'], NA)
  }else{
    result['t', ] <- c(sum_reg$coefficients[, 't value'], NA)
  }

  result <- as.data.frame(result)
  ############### staring ###########
  if(star_paren == TRUE){
    for(i in 1:number_of_columns){
      if(result['t', i] > -1.645 & result['t', i] < 1.645){
        result['estimates', i] <- format(round(as.numeric(result['estimates', i]),
                                               rounding_decimal),
                                         nsmall = rounding_decimal)
      }else if(result['t', i] > -1.96 & result['t', i] < 1.96){
        result['estimates', i] <- paste(format(round(as.numeric(result['estimates', i]),
                                                     rounding_decimal),
                                               nsmall = rounding_decimal),
                                        '*',
                                        sep = '')
      }else if(result['t', i] > -2.576 & result['t', i] < 2.576){
        result['estimates', i] <- paste(format(round(as.numeric(result['estimates', i]),
                                                     rounding_decimal),
                                               nsmall = rounding_decimal),
                                        '**',
                                        sep = '')
      }else{
        result['estimates', i] <- paste(format(round(as.numeric(result['estimates', i]),
                                                     rounding_decimal),
                                               nsmall = rounding_decimal),
                                        '***',
                                        sep = '')
      }
      result['t', i] <- paste('(',
                              format(round(as.numeric(result['t', i]),
                                           2),
                                     nsmall = 2),
                              ')',
                              sep = '')
    }
    result['estimates', 'Adj.R^2(%)'] <- format(round(as.numeric(result['estimates', 'Adj.R^2(%)']),
                                                      rounding_decimal),
                                                nsmall = rounding_decimal)
  }

  return(result)
}
