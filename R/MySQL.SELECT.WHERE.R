#' Selecting from MySQL
#'
#' This function executes the MySQL query SELECT...WHERE...AND...
#'
#' @param waht What to be selected, part of MySQL query that follows 'SELECT'
#' @param from.table the MySQL table from which the data is selected
#' @param myConnection The MySQL connection
#' @param ... The part of query that follows 'WHERE', and 'AND'
#' @param UNION_BY_FROM A logical argument, indicating whether to use 'UNION ALL'. Check the code for details.
#' @return The data fetched from DB.
#'
#'
#' @export
MySQL.SELECT.WHERE <- function(what,
                               from.table,
                               myConnection,
                               ...,
                               UNION_BY_FROM = FALSE){
  x <- list(...)
  if(length(x) == 0){
    if (UNION_BY_FROM){
      queries <- lapply(from.table,
                        function(from1){
                          base_query <- paste(sprintf('SELECT %s', what), sprintf('FROM %s', from1))

                          return(base_query)
                        })
      query <- paste(unlist(queries), collapse = ' UNION ALL ')

    }else{
      query <- paste(sprintf('SELECT %s', what), sprintf('FROM %s', from.table))
    }
  }else{
    y <- unlist(x)
    where <- y[1]
    z <- y[-1]
    if (UNION_BY_FROM){
      queries <- lapply(from.table,
                        function(from1){
                          base_query <- paste(sprintf('SELECT %s', what), sprintf('FROM %s', from1), sprintf('WHERE %s', where))
                          if(length(z) == 0){
                            query <- base_query
                          }else{
                            query <- paste(base_query, paste('AND', y, collapse = ' '))
                          }
                          return(query)
                        })
      query <- paste(unlist(queries), collapse = ' UNION ALL ')

    }else{
      base_query <- paste(sprintf('SELECT %s', what), sprintf('FROM %s', from.table), sprintf('WHERE %s', where))
      if(length(z) == 0){
        query <- base_query
      }else{
        query <- paste(base_query, paste('AND', y, collapse = ' '))
      }
    }
  }



  outcome <- dbGetQuery(myConnection, query)
  return(outcome)
}
