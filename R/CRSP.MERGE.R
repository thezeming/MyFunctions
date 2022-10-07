#' Merging WRDS CRSP stock file and event file
#'
#' This function merges the stock file and the event file of CRSP it is an R translation of the SAS macro CRSPMERGE by WRDS:https://wrds-www.wharton.upenn.edu/pages/support/research-wrds/macros/wrds-macro-crspmerge/. It requires R version 4.2.0 or above as it uses the native pipe and its placeholder.
#'
#' @param freq The frequency of the data being retrieved. Passible values are 'd' for daily and 'm' for monthly.
#'
#' @param SFvars A character vector specifying variable names from the stock file.
#'
#' @param SEvars A character vector specifying variable names from the event file.
#'
#' @param START_date The start date of the sample.
#'
#' @param END_date The end date of the sample.
#'
#' @param usnm WRDS username.
#'
#' @param pwd WRDS password. When pgpass is TRUE, this argument can be ignored.
#'
#' @param pgpass A boolean value indicating whether a .pgpass file has been set up on local machine.
#'
#' @return A tbl_PqConnection object, on which further dplyr operations can be applied. Final results (data) can be obtained by calling the collect() function.
#'
#'
#' @export
CRSP.MERGE <-
  function(freq = 'd',
           SFvars = c('prc',
                      'ret',
                      'shrout'),
           SEvars = c('ticker',
                      'ncusip',
                      'exchcd',
                      'shrcd',
                      'siccd'),
           START_date = '2022-01-01',
           END_date = '2022-09-27',
           usnm = 'username',
           pwd = 'password',
           pgpass = TRUE){
    # setting up variables #
    start_date <- lubridate::ymd(START_date)
    end_date <- lubridate::ymd(END_date)

    sfvars <-
      c('permno',
        'date',
        SFvars)

    sevars <-
      c('permno',
        'date',
        'ticker',
        'ncusip',
        'exchcd',
        'shrcd',
        'siccd')

    # once you setup the pgpass.conf file as explained at https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-your-computer/, you don't need to enter your password anymore.
    if(pgpass){
      wrds <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = 'wrds-pgdata.wharton.upenn.edu',
        dbname = 'wrds',
        port = 9737,
        sslmode = 'require',
        user = usnm
      )
    }else{
      wrds <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = 'wrds-pgdata.wharton.upenn.edu',
        dbname = 'wrds',
        port = 9737,
        sslmode = 'require',
        user = usnm,
        password = pwd
      )
    }


    sf_db <-
      dplyr::tbl(wrds,
                 dbplyr::in_schema("crsp",
                    paste0(freq, 'sf')))

    seall_db <-
      dplyr::tbl(wrds,
                 dbplyr::in_schema('crsp',
                    paste0(freq, 'seall')))

    senames_db <-
      dplyr::tbl(wrds,
                 dbplyr::in_schema('crsp',
                    paste0(freq, 'senames')))


    e <-
      which(!sfvars %in% colnames(sf_db))

    if (length(e) != 0) {
      stop(sfvars[e] |>
             paste0(collapse = ' ') |>
             paste0('Invalid names in ',
                    paste0(freq, 'sf'),
                    ': ',
                    ... = _))
    }



    e <-
      which(!sevars %in% colnames(seall_db))

    if (length(e) != 0) {
      stop(sevars[e] |>
             paste0(collapse = ' ') |>
             paste0('Invalid names in ',
                    paste0(freq, 'seall'),
                    ': ',
                    ... = _))
    }


    ## get stock data ##
    senames_permnos <-
      senames_db |>
      dplyr::filter(end_date >= namedt,
             start_date <= nameendt) |>
      dplyr::distinct(permno) |>
      dplyr::pull()

    sfdata_db <-
      sf_db |>
      dplyr::select(!!!dplyr::syms(sfvars)) |>
      dplyr::filter(date >= start_date,
             date <= end_date,
             permno %in% senames_permnos)


    ## get event data ##
    sedata_db <-
      seall_db |>
      dplyr::select(!!!dplyr::syms(sevars)) |>
      dplyr::inner_join(
        senames_db |>
          dplyr::filter(end_date >= namedt,
                 start_date <= nameendt) |>
          dplyr::group_by(permno) |>
          dplyr::summarise(minnamedt = min(namedt)) |>
          dplyr::ungroup(),
        by = c('permno')
      ) |>
      dplyr::filter(date >= minnamedt,
             date <= end_date)

    ## merge stock and event data ##

    eventvars <-
      c(
        'ticker',
        'comnam',
        'ncusip',
        'shrout',
        'siccd',
        'exchcd',
        'shrcls',
        'shrcd',
        'shrflg',
        'trtscd',
        'nmsind',
        'mmcnt',
        'nsdinx'
      )

    fillvars <-
      sevars[sevars %in% eventvars]

    dat <-
      sedata_db |>
      dplyr::full_join(sfdata_db |>
                         dplyr::mutate(indicator = 1),
                by = c('permno',
                       'date')) |>
      dplyr::group_by(permno) |>
      dbplyr::window_order(date) |>
      tidyr::fill(!!!dplyr::syms(fillvars),
           .direction = 'down') |>
      dplyr::filter(indicator == 1) |>
      dplyr::select(-indicator,
                    -minnamedt) |>
      dplyr::distinct(!!!dplyr::syms(sevars),
               .keep_all = TRUE) |>
      dplyr::ungroup()

    return(dat)
  }

