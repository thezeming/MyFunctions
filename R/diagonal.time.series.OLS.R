#'Generating a diagonal-style table from time-series OLS analysis
#'
#'This function produces a table where indipendent variables are used by the OLS analysis first once a time and then all together.
#'
#'@param Dep A xts which is the dependent variable.
#'
#'@param Indep A named list of xts objects which are independent variables.
#'
#'@param lagged A Boolean value indicating whether to lag the independent variable by one lag.
#'
#'@param ... Other arguments taken by OLS.REG.
#'
#'@export
diagonal.time.series.OLS <- function(Dep,
                                     Indep,
                                     lagged = FALSE,
                                     ...){
  ## individual analyses
  indiv_reg_results <- Indep %>% lapply(function(x){
    if(lagged){
      DATA <- xts::merge.xts(Dep,
                             x %>% xts::lag.xts())
    }else{
      DATA <- xts::merge.xts(Dep,
                             x)
    }
    A <- DATA %>% na.omit %>% OLS.REG(...)

    Intercept <- dplyr::select(A, 'Intercept')

    Coeffis <- dplyr::select(A,  'V1')

    R_squared <- dplyr::select(A,  'Adj.R^2(%)')
    return(list(Intercept,
                Coeffis,
                R_squared))
  })

  ### put individual results all together
  Intercepts <- indiv_reg_results %>% lapply(function(x){
    x[[1]]
  }) %>% do.call(cbind,.)

  COEFFIS <- seq_along(indiv_reg_results) %>% lapply(function(x){
    a <- indiv_reg_results[[x]][[2]]
    b <- cbind(number=c(x,x), a)
    return(b)
  }) %>%
    Reduce(function(x,y){
      dplyr::full_join(x, y, by = 'number')
    },.) %>% dplyr::select(-'number')
  colnames(COEFFIS) <- colnames(Intercepts)

  RSQUAREDS <- indiv_reg_results %>% lapply(function(x){
    x[[3]]
  }) %>% do.call(cbind,.)
  colnames(RSQUAREDS) <- colnames(Intercepts)

  indiv_results <- rbind(Intercepts,
                         COEFFIS,
                         RSQUAREDS)

  ### collective analysis
  Indep2 <- Indep %>% do.call(xts::merge.xts, .)
  if(lagged){
    DATA <- xts::merge.xts(Dep,
                           Indep2 %>% xts::lag.xts())
  }else{
    DATA <- xts::merge.xts(Dep,
                           Indep2)
  }
  A <- DATA %>% na.omit %>% OLS.REG(...)
  ALL <- A %>% seq_along %>% lapply(function(x){
    Dat <- A %>% dplyr::select(dplyr::all_of(x))
    colnames(Dat) <- 'ALL'
    return(Dat)
  }) %>% do.call(rbind,.)

  ### put all results together
  results <- cbind(indiv_results,
                   ALL)
  colnames(results) <- c(names(Indep),
                         'ALL')
  return(results)
}
