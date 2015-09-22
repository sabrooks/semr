
#' Lag Data Frame
#'
#' Product data affects future energy data.  Suitable for processes where
#' production input is provided and process is longer than data interval.
#'
#' @param .data A data frame.
#' @param timer dataframe column providing time (sequence) information
#' @param nrows number of rows to lag by.
#' @param append T/F, lagged dataframe is appended to original dataframe
#' @param trim.na T/F, remove rows without "lag" values
#' @param ... Specify columns to lag
#'
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(days = mdy("10/14/1978") + days(1:5),
#' number = 2:6)
#' lag_df(df, days)
lag_df<- function(.data, timer, nrows = 1, append = TRUE, trim.na = TRUE, ...){

  timer <- substitute(timer)

  .data_lag <- .data%>%
    dplyr::arrange_(timer)%>% # Sorts data
    dplyr::select_(paste("-", timer, sep = ""))%>% #Prevents duplicating timer col
    mutate_each(funs(lag(.,nrows)))
  # Add "_lag" to column names
  names(.data_lag) <- paste(names(.data_lag), "_lag", nrows, sep = "")

  #If append == TRUE, bind new columns to original dataframe
  if(append){
    output <- bind_cols(.data, .data_lag)
  } else {
    output <- .data_lag
  }

  # "Trim" NA's ----
  if(trim.na == TRUE){

    output <- output%>%
      slice(-(1:nrows))
  }

  return(output)
}

#' Lead Data Frame
#'
#' Product data affects previous energy data. Suitable for processes where
#' production output is provided and process is longer than data interval.
#'
#' @param .data A data frame.
#' @param timer dataframe column providing time (sequence) information
#' @param nrows number of rows to lead by.
#' @param append T/F, lead dataframe is appended to original dataframe
#' @param trim.na T/F, remove rows without "lead" values
#' @param ... Specify columns to lead
#'
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(days = mdy("10/14/1978") + days(1:5),
#' number = 2:6)
#' lead_df(df, days)
lead_df<- function(.data, timer, nrows = 1, append = TRUE, trim.na = TRUE, ...){

  timer <- substitute(timer)

  .data_lead <- .data%>%
    dplyr::arrange_(timer)%>% # Sorts data
    dplyr::select_(paste("-", timer, sep = ""))%>% #Prevents duplicating timer col
    mutate_each(funs(lead(.,nrows)))

  names(.data_lead) <- paste(names(.data_lead), "_lead", nrows, sep = "")

  #If append == TRUE, bind new columns to original dataframe
  if(append){
    output <- bind_cols(.data, .data_lead)
  } else {
    output <- .data_lead
  }

  # "Trim" NA's ----
  if(trim.na == TRUE){

    rowsize = nrow(.data)

    output <- output%>%
      slice(-((rowsize - nrows+1):rowsize))
  }

  return(output)
}
