# write a data.frame with daily close of token
# we will keep 18:59:59 as close time

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')
library(purrr)
library('lubridate')
library('PerformanceAnalytics')


binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
token <- read.table('assets.csv') # !! no SUSDT price before 01-16. Removed from csv

# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
end_date   <- as.POSIXct(today(), tz = 'UTC')

## portfolio token as a tibble ##
# replace BTC with USDC at the end of the token names
token_usdc <- token %>%
  mutate(V1 = sub("BTC$", "USDC", V1))
# remove duplicate
token_usdc <- token_usdc %>%
  distinct()
# transform to a tibble
token_usdc <- unnest(token_usdc, cols = V1)

# function to get prices
get_price <- function(my_token, start_date) {
  klines <- binance_klines(my_token, interval = '6h', start_time = as.Date(start_date))
}

# Fetch data for each date/token @18:59:59 and return in a tibble
token_daily_close <- token_usdc %>%
  mutate(data = map(V1, ~ get_price(.x, start_date))) %>%
  mutate(token = V1) %>%
  select(-V1) %>%
  unnest(data) %>%
  filter(format(close_time, "%H:%M:%S") == "18:59:59") %>%
  select(token, close_time, close)
# put token names as column names
token_daily_close <- token_daily_close %>%
  pivot_wider(names_from = token, values_from = close) 
 
token_daily_close <- token_daily_close %>%
 rename(time = close_time)



