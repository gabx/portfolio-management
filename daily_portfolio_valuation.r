

###############################################################################
# compute daily portfolio valuation

# return token price at specific time
trade_list_value <- trade_list %>%
  mutate(token_valuation = map2_dbl(symbol, time, get_token_price))


latest_row <- trade_list %>%
  slice(which.max(time))
valuation_time <- latest_row[[1]]

# complete the dates
trade_list_final <- trade_list_final %>%
  mutate(date = as.Date(time))

full_dates <- tibble(date = seq(min(trade_list_final$date), max(trade_list_final$date), by = "day"))
# Complete the data with one row per day
completed_tb <- trade_list_final %>%
  right_join(full_dates, by = "date") %>%
  arrange(date)

# Set timestamp to 20:00:00 for missing timestamps
completed_tb <- completed_tb %>%
  mutate(
    time = if_else(
      is.na(time),
      as.POSIXct(paste(date, "18:00:00"), tz = "UTC"),
      time
    )
  )

# Reorder or clean up if needed
completed_tb <- completed_tb %>%
  select(-date)  # Remove helper date column if not needed

##############################################
## to work with PerformanceAnalytics ##
# change tibble to a time serie object
daily_prices_close_xts <- xts(daily_prices_close %>% select(-open_time), order.by = daily_prices_close$open_time)
# compute returns
Return.calculate(daily_prices_close_xts, method = 'discrete')