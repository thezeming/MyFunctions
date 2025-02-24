#' Merging WRDS CRSP stock file and event file
#'
#' This function merges the stock file and the event file of CRSP.
#' It is essentially an R translation of the SAS macro CRSPMERGE by WRDS:https://wrds-www.wharton.upenn.edu/pages/support/research-wrds/macros/wrds-macro-crspmerge/, with some minor changes.
#' I thank the authors of the original SAS code for granting the right of using their logic.
#' It requires R version 4.2.0 or above as it uses the native pipe and its placeholder.
#'
#' @param freq The frequency of the data being retrieved. Passible values are 'd' for daily and 'm' for monthly.
#'
#' @param vars A character vector specifying variable names from the stock file. Please note that by default, 'permno' and 'date' are always included in the output and do not need to be specified here.
#'
#' @param START_date The start date of the sample, in the format 'YYYY-MM-DD'.
#'
#' @param END_date The end date of the sample, in the format 'YYYY-MM-DD'.
#'
#' @param usnm WRDS username.
#'
#' @param pwd WRDS password. When pgpass is TRUE, this argument can be ignored.
#'
#' @param pgpass A boolean value indicating whether a .pgpass file has been set up on local machine.
#'
#' @return A tibble that contains the merged data.
#'
#'
#' @export
CRSP.MERGE <-
  function(freq = 'd',
           vars = c('prc',
                      'ret',
                      'shrout',
                      'ticker',
                      'ncusip',
                      'exchcd',
                      'shrcd',
                      'siccd'),
           START_date = '2022-01-01',
           END_date = '2022-09-27',
           usnm = 'username',
           pwd = 'password',
           pgpass = FALSE){
    # setting up variables #
    start_date <- lubridate::ymd(START_date)
    end_date <- lubridate::ymd(END_date)

    if(!freq %in% c('d', 'm')){
        stop(
            'The argument "freq" must be either "d" or "m".'
        )
    }

    if(start_date > end_date){
      stop(
        'START_date must be smaller than or equal to END_date.'
      )
    }

    vars <- tolower(vars)

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

    SFVARs <- colnames(sf_db)

    SEVARs <- colnames(seall_db)

    e <-
      setdiff(vars, c(SFVARs, SEVARs))

    if(freq == 'd'){
        FRED <- 'daily'
    } else {
       FRED <- 'monthly'
    }

    if (length(e) != 0) {
      stop(e |>
             paste0(collapse = ' ') |>
             paste0('Invalid values in vars',
                    ': ',
                    ... = _) |>
            paste0('\n Possible options for ', FRED,' data are:\n',
                   paste0(setdiff(c(SFVARs, SEVARs), c('permno', 'date')),
                          collapse = ' '))
                   )
    }

    sfvars <- intersect(vars, SFVARs)  |> c('permno', 'date')
    sevars <- intersect(vars, setdiff(SEVARs, SFVARs)) |> c('permno', 'date')

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
             date <= end_date) |>
      dplyr::select(-minnamedt)

    ## merge stock and event data ##

    fillvars <-
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

    dat <-
      sedata_db |>
      dplyr::mutate(eventdata = 1) |>
      # note that full_joint, not right_joint, is required here.
      # (Imagine a permno of which the stock file has a gap in timestamps, and the variables in the event file change value during this gap.)
      dplyr::full_join(sfdata_db |>
                         dplyr::mutate(stockdata = 1),
                by = c('permno',
                       'date')) |>
      # I didn't order the stock and event files above as in the original SAS code, this is because the 'ORDER BY is ignored ... use window_order() instead?' warning.
      # Therefore I order the merged dataset by permno and date here.
      # This matters for the following filling of the even variables.
      dplyr::arrange(permno, date) |>
      dplyr::collect() |>
      dplyr::group_by(permno) |>
      # for sevars that are in fillvars, replace the missing value with Inf for numerical variable and '.' for character variables if eventdata == 1.
      # The reason of doing so is to allow for the fill function below to run without problems.
      # Otherwise this may be a problem. For example, for permno 10859, it has siccd as 5031 on 1972-12-14 and then as NA on 1973-05-04.
      # Without the aforementioned step, the siccd for permno 10859 on 1973-05-07 would be filled with 5031 where it really should be NA.
      # For detailed logic, see the original SAS file.
        dplyr::mutate(dplyr::across(.cols = tidyselect::any_of(fillvars),
                            .fn = ~ {
                                if(is.numeric(.x)){
                                      dplyr::case_when(eventdata == 1 & is.na(.x) ~ Inf,
                                                       TRUE ~ .x)
                                    }else if(is.character(.x)){
                                      dplyr::case_when(eventdata == 1 & is.na(.x) ~ '.',
                                                       TRUE ~ .x)
                                    }
                            })) |>
    tidyr::fill(tidyselect::any_of(fillvars),
           .direction = 'down') |>
      # The original SAS file deletes obs with eventdata == 1 & stockdata != 1.
      # Since there's no obs with eventdata != 1 & stockdata != 1, the above is equivlent to retain obs with stockdata == 1.
      dplyr::filter(stockdata == 1) |>
      dplyr::select(-c(eventdata,
                       stockdata)) |>
      # changing '.' and Inf back to NA.
      dplyr::mutate(dplyr::across(.cols = tidyselect::any_of(fillvars) &
                                    tidyselect::where(is.character),
                                  .fn = ~ dplyr::na_if(.x, '.'))) |>
      dplyr::mutate(dplyr::across(.cols = tidyselect::any_of(fillvars) &
                                    tidyselect::where(is.numeric),
                                  .fn = ~ dplyr::na_if(.x, Inf))) |>
      dplyr::distinct() |>
      dplyr::ungroup()

    return(dat)
  }
