stat_hqnorm <- function(lm){

  lm_df <- lm%>%
    broom::augment()%>%
    arrange(.hat)

  n <- nrow(lm_df)

  theoretical <- data_frame(theoretical = abs(qnorm(stats::ppoints(n))))%>%
    arrange(theoretical)

  bind_cols(lm_df, theoretical)

}


#' Plot Model leverage vs. Half-normal quantiles
#'
#' @param lm
#' @param label, number of labels to plot, NULL plots no labels
#'
#' @return plot of model leverage vs. half-normal quantiles
#' @export
#'
#' @examples
geom_hqnorm <- function(lm, nlabel = 0){

  # returns data_frame sorted by .hat (leverage)
  hqnorm <- stat_hqnorm(lm)

  labels <- hqnorm%>%
      top_n(label, .hat)

  hqnorm%>%
    ggplot2::ggplot(aes(x =theoretical, y = .hat))+
    geom_point()+
    geom_text(data = labels, aes(label = energy))+
    ylab("Leverage")

}

#Create a table (data_frame) of nrows with the greatest leverage
table_leverage <- function(lm, .data = NA, nrows = 5){

  lm%>%
    broom::augment()%>%
    top_n(nrows, .hat)%>%
    left_join(.data)%>%
    dplyr::select(-.fitted, -.sigma, -.cooksd, -.std.resid )%>%
    arrange(desc(.hat))

}

#Draws a calendar
geom_calendar <- function(.data){

  #create a data_frame of no vector days to fill in light grey
  date_range <-.data%>%
    summarise(start = floor_date(min(date), "day"),
              end = floor_date(max(date), "day"))

  #list of all days in data range
  n <- (date_range$start%--%date_range$end)%/%days(1) # number of days
  day_list <- (date_range$start + days(0:n))%>%
    as.data.frame()%>%
    dplyr::rename(., day = `.`)

  .data%>%
    mutate(day = floor_date(date, "day"))%>%
    full_join(day_list, by = "day")%>%
    mutate(wk = (floor_date(day, "week")-origin)/dweeks(1),
           wkday = wday(day, label = TRUE),
           mn = floor_date(day, "month"),
           yr = year(day),
           mnday = mday(day))%>%
    ggplot(aes(x = wkday, y = wk, fill = energy))+
    facet_wrap(~ mn, scales = "free_y")+
    geom_tile(aes(fill = energy), color = "white")+
    scale_fill_continuous(na.value = "grey")+
    scale_y_reverse()+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())

}


#' Weekstrip Plot
#' Inspired by the "Public Contributions" graph at github.com/
#'
#' @param .data dataframe with datetime data in column "date"
#'
#' @return
#' @export
#'
#' @examples
geom_wstrip <- function(.data){
  assertthat::validate_that(is.data.frame(.data)) #Check for data.frame

  #Calculate First and Last day
  date_range <-.data%>%
    summarise(start = floor_date(min(date), "day"),
              end = floor_date(max(date), "day"))

  #Generate list of all days in data range
  n <- (date_range$start%--%date_range$end)%/%days(1) # number of days
  day_list <- (date_range$start + days(0:n))%>%
    as.data.frame()%>%
    dplyr::rename(., day = `.`)

  .data%>%
    mutate(day = floor_date(date, "day"))%>%
    full_join(day_list, by = "day")%>%
    mutate(wk = (floor_date(floor_date(day, "month"), "week")%--%floor_date(day, "week"))%/%dweeks(1),
           wkday = wday(day, label = TRUE),
           mn = month(day, label = TRUE),
           yr = year(day),
           mnday = mday(day))%>%
    ggplot(aes(x = wk, y = wkday, fill = energy))+
    facet_grid(yr ~ mn)+
    geom_tile(aes(fill = energy), color = "white")+
    scale_fill_continuous(na.value = "#CCCCCC")+
    scale_y_discrete(breaks = c("Mon", "Wed", "Fri"),
                     labels = c("M", "W", "F"))+
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color = "#777777"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          panel.margin.x = unit(0, "cm"),
          strip.text = element_text(color = "#777777"),
          legend.position = "bottom")

}
