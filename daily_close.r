# write a data.frame with daily close of token

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')
library(purrr)
library(lubridate)
library(PerformanceAnalytics)


binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
tokens <- read.table('assets.csv')

# replace BTC with USDT at the end of the token names
tokens_usdt <- tokens%>%
  mutate(V1 = sub("BTC$|USDC", "USDT", V1))
# remove duplicate
tokens_usdt <- token_usdt %>%
  distinct()

# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
end_date   <- as.POSIXct("2025-03-08", tz = "UTC")


get_price <- function(my_token, start_date, end_date) {
  klines <- binance_klines(my_token, interval = '1h', start_time = as.Date(start_date))
}

# Generate sequence of dates
date_seq <- seq(start_date, end_date, by = "day")
# Fetch data for each date
daily_prices <- map_dfr(date_seq, ~ get_price('BTCUSDT', .x))

# daily_prices <- daily_prices %>%
#   mutate(open_time = as.POSIXct(open_time, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
# remove duplicates
daily_prices_unique <- daily_prices_close %>%
  distinct(open_time, .keep_all = TRUE)
# keep price @ 06:00 each day
daily_prices_close <- daily_prices_unique %>%
  filter(format(open_time, "%H:%M:%S") == "06:00:00") %>%
  select(open_time, close)  # Keep only relevant columns

## to work with PerformanceAnalytics ##
# change tibble to a time serie object
daily_prices_close_xts <- xts(daily_prices_close %>% select(-open_time), order.by = daily_prices_close$open_time)


# compute returns
Return.calculate(daily_prices_close_xts, method = 'discrete')
