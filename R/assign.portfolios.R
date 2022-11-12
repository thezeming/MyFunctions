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
#' @param n_portfolios A possitive number.
#' This is the umber of portfolios to which assets are assigned.
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
           n_portfolios,
           P_prefix = 'P',
           bp_prefix = 'bp') {
    breakpoints <- data |>
      summarize(breakpoint = quantile({
        {
          break_var
        }
      },
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE)) |>
      pull(breakpoint) |>
      as.numeric()

    for(i in 1:n_portfolios){
      data <-
        data |>
        mutate('{P_prefix}{i}':= if_else({{ sort_var }} >= breakpoints[i] & {{ sort_var }} <= breakpoints[i+1],
                                TRUE,
                                FALSE))
    }

    for(i in 1:length(breakpoints)){
      data <-
        data |>
        mutate('{bp_prefix}{i}':=breakpoints[i])
    }

    Dat <-
      data |>
      select(paste0(P_prefix, 1):paste0(bp_prefix, length(breakpoints)))

    return(Dat)
  }
