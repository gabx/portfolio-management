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
# replace BTC with USDT at the end of the token names
token_usdt <- token %>%
  mutate(V1 = sub("BTC$|USDC", "USDT", V1))
# remove duplicate
token_usdt <- token_usdt %>%
  distinct()
# transform to a tibble
token_usdt <- as_tibble(token_usdt)

# function to get prices
get_price <- function(my_token, start_date) {
  klines <- binance_klines(my_token, interval = '6h', start_time = as.Date(start_date))
}


# Fetch data for each date/token @18:59:59 and return in a tibble
token_daily_close <- token_usdt %>%
  mutate(data = map(V1, ~ get_price(.x, start_date))) %>%
  mutate(token = V1) %>%
  select(-V1) %>%
  unnest(data) %>%
  filter(format(close_time, "%H:%M:%S") == "18:59:59") %>%
  select(token, close_time, close)
# put token names as column names
price_wide <- token_daily_close %>%
  select(close_time, token, close) %>%   # on garde uniquement les colonnes utiles
  pivot_wider(names_from = token, values_from = close)


# remove duplicates
daily_prices_unique <- daily_prices_close %>%
  distinct(open_time, .keep_all = TRUE)

##############################################
## to work with PerformanceAnalytics ##
# change tibble to a time serie object
daily_prices_close_xts <- xts(daily_prices_close %>% select(-open_time), order.by = daily_prices_close$open_time)
# compute returns
Return.calculate(daily_prices_close_xts, method = 'discrete')
