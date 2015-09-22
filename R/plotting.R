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
