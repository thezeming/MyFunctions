#' Assign Portfolios
#'
#' This function assigns each of the assets to a portfolio in the cross-section.
#' This function implements the portfolio sorting method as in Bali et al. (2016).
#' That is, it is possible for an asset to be assigned to more than one portfolios in a given cross-section.
#'
#' @param data A tibble that contains at least two variables, one is used to compute breakpoints and the other is used to compute portfolio membership.
#'
#' @param break_var The variable that is used to compute breakpoints.
#'
#' @param sort_var The variable that is used to compute portfolio membership.
#'
#' @param bps A numerical vector specifying customised preakpoints.
#' It should contain only the breakpoints not the end values.
#' For example, for a tercile sort, this value should be c(0.2, 0.8), not c(0, 0.2, 0.8, 1).
#'
#' @param n_portfolios A possitive number.
#' This is the umber of portfolios to which assets are evenly assigned.
#' Breakpoints are computed evenly across the range in each cross-section.
#'
#' @param P_prefix The prefix to be placed in front of portfolio numbers.
#' This will be part of the column names for the portfolio membership columns.
#' The default is 'P', which gives column names P1, P2, P3, ...
#'
#' @param bp_prefix Similar to P_prefix, this is the prefix to be placed in front of breakpoint numbers.
#' This will be part of the column names for the breakpoint columns.
#' The default is 'bp', which gives column names bp1, bp2, ...
#'
#' @param type An integer between 1 and 9 selecting one of the nine quantile algorithms used by quantile().
#' The default value is 2 which is the default algorithm used by SAS.
#'
#' @param show_bps A Boolean value indicating whether to retain breakpoints in the output dataframe.
#' When FALSE, the breakpoints will not be shown and the argument bp_prefix will be ignored.
#'
#' @return A tibble that contains two types of columns (variables), one is the portfolio membership columns, the other is the breakpoint columns.
#' Each portfolio membership column gives Boolean values indicating whether an asset belongs to that portfolio.
#' Each breakpint column gives the breakpoint used for computing membership, this will be one column more than the portfolio memebership columns.
#'
#'
#' @export
assign.portfolios <-
  function(data,
           break_var,
           sort_var,
           bps = NULL,
           n_portfolios = 10,
           P_prefix = 'P',
           bp_prefix = 'bp',
           show_bps = TRUE,
           type = 2) {
    OriginalVars <-
      colnames(data)
    
    if (is.null(bps)) {
      breakpoints_probs <- seq(0, 1, length.out = n_portfolios + 1)
    } else{
      breakpoints_probs <- c(0, bps, 1)
    }
    
    breakpoints <- data |>
      summarize(breakpoint = quantile({
        {
          break_var
        }
      },
      probs = breakpoints_probs,
      na.rm = TRUE,
      type = type)) |>
      pull(breakpoint) |>
      as.numeric()
    
    for (i in 1:n_portfolios) {
      data <-
        data |>
        mutate('{P_prefix}{i}' := if_else({
          {
            sort_var
          }
        } >= breakpoints[i] & {
          {
            sort_var
          }
        } <= breakpoints[i + 1],
        TRUE,
        FALSE))
    }
    
    if (show_bps) {
      for (i in 1:length(breakpoints)) {
        data <-
          data |>
          mutate('{bp_prefix}{i}' := breakpoints[i])
      }
      
    }
    
    Dat <-
      data |>
      select(-all_of(OriginalVars))
      
    return(Dat)
  }
