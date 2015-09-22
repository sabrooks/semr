geom_cor <- function(lm){
  broom::augment(lm)%>%
    mutate(.resid_lag = lag(.resid))%>%
    ggplot2::ggplot(aes(x = .resid, y = .resid_lag))+
    geom_point(alpha = 0.2)
}
