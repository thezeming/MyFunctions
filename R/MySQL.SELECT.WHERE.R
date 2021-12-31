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
