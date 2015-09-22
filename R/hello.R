# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

stat_cumsum <- function(.data, lm){
  # Use start date to calculate year
  start_date <- attr(.data, "start_date")
  # Add "yr" to .data
  .data <- .data%>%
    mutate(yr = (date - start_date)%/%lubridate::dyears(1))

  predict(lm, .data, interval = "prediction")%>%
    as.data.frame()%>%
    bind_cols(.data)%>%
    group_by(yr)%>%
    mutate(diff = fit - energy,
           .cumsum = cumsum(diff))

}

# Compare a list of lm models
lm_compare <- function(lm_list){

}

