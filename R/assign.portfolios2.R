#' Assign Portfolios
#'
#' This function is a simplified version of assign.portfolios.
#' It assigns each of the assets to a portfolio in the cross-section.
#' By default, it mutates two new columns indicating the group memberships of each row.
#' The first column gives results by (], whereas the second gives results by [).
#' Further operations required if the portfolio sorting method as in Bali et al. (2016) is to be implemented. (i.e., for a border asset to be assigned to two portfolios in a given cross-section.)
#' For example, one could replace the values in the second column with NA when it is the same with the first column and then pivot_longer with values_drop_na = TRUE. 
#' (Be careful not to do a second sort after this as number of observations, thus breakpoints, may change for the second sort. Finish all the sorts before this operation.)
#' 
#'
#' @param data A tibble that (normally) contains at least two variables, one is used to compute breakpoints and the other is used to compute portfolio membership.
#' In the case that the breakpoints are computed from the sorting variable itself, the tibble could potentially contain only one variable.
#'
#' @param break_var The variable that is used to compute breakpoints.
#'
#' @param sort_var The variable that is used to compute portfolio membership.
#'
#' @param bps A numerical vector specifying customised preakpoints.
#' It should contain only the breakpoints not the end values.
#' For example, for a tercile sort, this value should be c(0.2, 0.8), not c(0, 0.2, 0.8, 1).
#'
#' @param n_portfolios A possitive integer.
#' This is the umber of portfolios to which assets are evenly assigned.
#' Breakpoints are computed evenly across the range in each cross-section.
#' This argument will be ignored if the argument bps is provided.
#'
#' @param P_prefix The prefix to be placed in front of the two portfolio membership column names.
#'
#' @param bp_prefix This is the prefix to be placed in front of breakpoint numbers.
#' This will be part of the column names for the breakpoint columns.
#' The default is 'bp', which gives column names bp1, bp2, ...
#'
#' @param bp_type An integer between 1 and 9 selecting one of the nine quantile algorithms used by quantile(), see which for details.
#' The default value is 2 which is the default algorithm used by SAS.
#'
#' @param show_bps A Boolean value indicating whether to retain breakpoints in the output dataframe.
#' When FALSE, the breakpoints will not be shown and the argument bp_prefix will be ignored.
#' 
#'
#' @return A tibble that adds at least two columns that contains primary and secondary group memberships.
#' If show_bps is TRUE, then each breakpint column gives the breakpoint used for computing membership.
#'
#'
#' @export
assign.portfolios2 <-
  function(data,
           break_var,
           sort_var,
           bps = NULL,
           n_portfolios = 10,
           P_prefix = 'P',
           bp_prefix = 'bp',
           show_bps = FALSE,
           bp_type = 2
  ) {
    
    if (is.null(bps)) {
      breakpoints_probs <- seq(0, 1, length.out = n_portfolios + 1)
    } else {
      breakpoints_probs <- c(0, bps, 1)
      n_portfolios <- length(bps) + 1
    }
    
    breakpoints <- data |>
      reframe(breakpoint = quantile({
        {
          break_var
        }
      },
      probs = breakpoints_probs,
      na.rm = TRUE,
      type = bp_type)) |>
      pull(breakpoint) |>
      as.numeric()
    
    # replace the endpoints with -Inf and Inf, otherwise there may be problems when computing breakpoints using variables other than sorting variables.
    # E.g, the end points of NYSE breakpoints are not necessarily the end points of the actual sorting variable, which may be one from the overall sample.
    breakpoints[1] <- -Inf
    breakpoints[length(breakpoints)] <- Inf
    

      
    if (show_bps) {
      Dat <- 
        data |> 
        mutate(
          '{P_prefix}1' := {{sort_var}} |> 
            cut(breakpoints,
                labels = FALSE),
          '{P_prefix}2' := {{sort_var}} |> 
            cut(breakpoints,
                right = FALSE,
                labels = FALSE)
        )
      
      for (i in 1:length(breakpoints)) {
        Dat <-
          Dat |>
          mutate('{bp_prefix}{i}' := breakpoints[i])
      }
      
      return(Dat)
    }else{
      return(
        data |> 
          mutate(
            '{P_prefix}1' := {{sort_var}} |> 
              cut(breakpoints,
                  labels = FALSE),
            '{P_prefix}2' := {{sort_var}} |> 
              cut(breakpoints,
                  right = FALSE,
                  labels = FALSE)
          )
      )
    }
    

  }
