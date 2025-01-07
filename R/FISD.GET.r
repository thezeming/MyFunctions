#' Retrieve FISD data from WRDS
#'
#' This function demonstrates how you can connect to WRDS 
#' and query a single FISD table (e.g., \code{"fisd_mergedissue"}) in a similar 
#' style to your CRSP function. Adapt as necessary for your specific needs.
#'
#' @param tbl_name A character string specifying which FISD table to query. 
#'   Defaults to \code{"fisd_mergedissue"}, can also be \code{"fisd_mergedissuer"}.
#' @param vars A character vector specifying the variable names to retrieve,
#'   or \code{"ALL"} (the default) to select all columns.
#' @param usnm Your WRDS username.
#' @param pwd Your WRDS password (ignored if \code{pgpass} is \code{TRUE}).
#' @param pgpass A boolean indicating whether a \code{.pgpass} file is set up so that 
#'   you do not need to supply a password.
#'
#' @return A tibble containing the requested data from the specified table.
#'
#' @export
FISD.GET <- function(
  tbl_name = "fisd_mergedissue", 
  vars = "ALL",
  usnm = "username", 
  pwd = "password", 
  pgpass = FALSE
){
  # --- 0. Load Packages (Assuming you already have them loaded in your environment) ---
  # library(DBI)
  # library(RPostgres)
  # library(dplyr)
  # library(dbplyr)
  # library(lubridate)

  # --- 2. Set up WRDS connection ---
  if (pgpass) {
    wrds <- DBI::dbConnect(
      RPostgres::Postgres(),
      host   = "wrds-pgdata.wharton.upenn.edu",
      dbname = "wrds",
      port   = 9737,
      sslmode= "require",
      user   = usnm
    )
  } else {
    wrds <- DBI::dbConnect(
      RPostgres::Postgres(),
      host     = "wrds-pgdata.wharton.upenn.edu",
      dbname   = "wrds",
      port     = 9737,
      sslmode  = "require",
      user     = usnm,
      password = pwd
    )
  }

fisd_db <- dplyr::tbl(wrds,
                 dbplyr::in_schema("fisd",
                                   tbl_name))

if(length(vars) ==1 && vars == "ALL") {

    Dat <- 
    fisd_db |>
    dplyr::collect()
}else {
    # Check for valid variable names
    bad_vars <- setdiff(tolower(vars), tolower(colnames(fisd_db)))
    if (length(bad_vars) > 0) {
      stop(
        paste0(
          "Invalid variable(s) in 'vars': ",
          paste(bad_vars, collapse=", "), 
          "\nPossible columns in '", tbl_name, "' are:\n",
          paste(colnames(fisd_db), collapse=", ")
        )
      )
    }
    Dat <- 
      fisd_db |>
       dplyr::select(!!!dplyr::syms(vars)) |>
       dplyr::collect()
}

  # --- 4. Disconnect from WRDS ---
  DBI::dbDisconnect(wrds)

  return(Dat)
}
