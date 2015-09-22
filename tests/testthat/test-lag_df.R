context("lead/lag")

library(lubridate)
library(dplyr)


# Create Test Data Frame

df <- data.frame(days = mdy("10/14/1978") + days(1:5),
                 number = 2:6)

df%>%
  lag_df(days)

test_that("Default lag", {
  expect_equivalent(df%>%lag_df(days),
               df%>%slice(-1)%>%mutate(number_lag1 = as.integer(number - 1)))
})

test_that("Lag = 2", {
  expect_equivalent(df%>%lag_df(days, nrows = 2),
               df%>%slice(-(1:2))%>%mutate(number_lag2 = as.integer(number - 2)))
})

test_that("Append == FALSE", {
  expect_equivalent(df%>%lag_df(days, append = FALSE),
                    df%>%slice(-1)%>%transmute(number_lag2 = as.integer(number - 1)))
})

test_that("Default lead", {
  expect_equivalent(lead_df(df, days),
                    df%>%slice(-5)%>%mutate(number_lead1 = as.integer(number + 1)))
})

test_that("Lead = 2", {
  expect_equivalent(lead_df(df, days, nrows = 2),
                    df%>%slice(-(4:5))%>%mutate(number_lead2 = as.integer(number + 2)))
})

test_that("Append == FALSE", {
  expect_equivalent(df%>%lead_df(days, append = FALSE),
                    df%>%slice(-5)%>%transmute(number_lead2 = as.integer(number + 1)))
})
