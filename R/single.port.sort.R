#' Single portfolio sort
#'
#' This functions is the building block of single portfolio sort analyses.
#' It sorts and returns the results for one portfolio sort at a given time t.
#' By iterating this function over time, normal portoflio sort analysis can be done.
#'
#' @param FACTOR The cross-section of the sorting variable at time t-1.
#'
#' @param WEIGHTING This variable specifies the weighting scheme.
#' For example, if you wish to do a value weighted portfolio sort, put the cross-sectional marcket cap here.
#' Otherwise, enter 'equal' as character, this will give you a equal weighted portfolio sort.
#'
#' @param HOLD This is the asset returns at t.
#'
#' @param SubPort_No A positive integer that specifies the number of portoflios to be formed.
#'
#' @param Output A character indicating whether to give just returns for each portfolio or detailed ranking of assets in a list.
#' Possible values are: 'returns' and 'lists'.
#'
#' @return Either a vector of sub-portfolio returns or a list of detailed ranking of assets.
#'
#' #' @export
single.port.sort <- function(FACTOR,
                             WEIGHTING = 'equal', # equal weight by default. Otherwise, put weights here.
                             HOLD,
                             SubPort_No = 10,
                             Output = 'returns'){ # possible values: 'returns' gives returns for each portfolio;
  #                  'lists' gives detailed lists after ranking for each portfolio
  if (identical(WEIGHTING,'equal')){
    WEIGHTING <- rep(1,(FACTOR %>% as.vector %>% length))
  }
  DATA_presort <- cbind(FACTOR %>% as.vector %>% as.data.frame(),
                        WEIGHTING %>% as.vector %>% as.data.frame(),
                        HOLD %>% as.vector %>% as.data.frame())
  rownames(DATA_presort) <- FACTOR %>% names
  DATA_presort <- DATA_presort %>% na.omit()
  colnames(DATA_presort) <- c('FACTOR',
                              'WEIGHTING',
                              'HOLD')
  if((DATA_presort %>% nrow) < SubPort_No){
    stop('number of assets is smaller than number of SubPorts.')
  }
  if(!(DATA_presort$FACTOR %>% is.numeric)){
    stop('Sorting variable may have been converted to non-numeric.')
  }
  DATA_sorted <- DATA_presort[order(DATA_presort$FACTOR),]
  sorted_list <- split(DATA_sorted, cut(seq_along(DATA_sorted$FACTOR),
                                        SubPort_No,
                                        labels =FALSE))

  # decide what to return
  if(Output == 'lists'){
    return(sorted_list)
  }else if(Output == 'returns'){
    returns <- sorted_list %>% lapply(function(x){
      weighted.mean(x$HOLD, x$WEIGHTING)
    }) %>% unlist
    return(returns)
  }else{
    stop('Invalid argument for \'Output\', use \'lists\' or \'returns\'')
  }
}
