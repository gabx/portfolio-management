



##############################################
## to work with PerformanceAnalytics ##
# change tibble to a time serie object
daily_prices_close_xts <- xts(daily_prices_close %>% select(-open_time), order.by = daily_prices_close$open_time)
# compute returns
Return.calculate(daily_prices_close_xts, method = 'discrete')