## Create a data frame with daily close of portfolio tokens ##

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')
library(purrr)
library(lubridate)



binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
tokens <- read.table('assets.csv')

# replace BTC with USDT at the end of the token names
tokens_usdt <- tokens %>%
  mutate(V1 = sub("BTC$|USDC", "USDT", V1))
# remove duplicate
tokens_usdt <- tokens_usdt %>%
  distinct()
# make a list
tokens_usdt_ls <- as.list(tokens_usdt)

# Define start date in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC') 
# end_date   <- as.POSIXct("2025-03-12", tz = "UTC")
# date_seq <- seq(start_date, end_date, by = "day")

# get daily close for our token
# !! pour l'écriture du code !!
# sel_tok <- token_usdt[1:2, ]
# interval at 6h allows to stay in the limit of 500 values
get_price <- function(my_token, start_date) {
  klines <- binance_klines(my_token, interval = '6h', start_time = as.Date(start_date))
}

# !! results are limited to 500 values

daily_prices <- map(tokens_usdt_ls$V1, ~ get_price(.x, start_date = start_date))
                                        
# keep close at 7:00 each day
keep_close <- function(df) {
  df %>%
    filter(format(open_time, "%H:%M:%S") == "07:00:00") %>%
    select(open_time, close)
}


# this is the final list with daily closes for each token on the period
df_list_close <- lapply(daily_prices, keep_close)
# transform to a list of tibbles
df_list_close <- map(df_list_close, as_tibble)
# combine data frames of the list in one df by the common column, open_time
df_combined <- reduce(df_list_close, full_join, by = "open_time")
# name columns
colnames(df_combined)[-1] <- tokens_usdt_ls$V1



