## Create a data frame with positions, token daily closes, PL, BTC as benchmark ##

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

# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-17", tz = 'UTC') 
end_date   <- as.POSIXct("2025-03-12", tz = "UTC")
date_seq <- seq(start_date, end_date, by = "day")

# get daily close for our token
# !! pour l'écriture du code !!
# sel_tok <- token_usdt[1:2, ]

get_price <- function(my_token, start_date, end_date) {
  klines <- binance_klines(my_token, interval = '1h', start_time = as.Date(start_date))
}
daily_prices <- map(tokens_usdt_ls$V1, ~ get_price(.x, start_date = start_date, end_date = end_date))
                                        
# keep close at 6:00 each day
keep_close <- function(df) {
  df %>%
    filter(format(open_time, "%H:%M:%S") == "06:00:00") %>%
    select(open_time, close)
}


# this is the final list with daily closes for each token on the period
df_list_close <- lapply(daily_prices, keep_close)
# combine data frames of the list in one df by the common column, open_time
df_combined <- reduce(df_list_close, full_join, by = "open_time")
# name columns
colnames(df_combined)[-1] <- tokens_usdt_ls$V1




# daily_prices <- map_dfr(date_seq, ~ get_price(c('BTCUSDT', 'SUIUSDT'), .x))
# remove duplicates
# daily_prices_unique <- token_daily %>%
#   distinct(open_time, .keep_all = TRUE)
# daily_prices_unique <- map(token_daily, ~ distinct(.x, open_time, .keep_all = TRUE))
# keep price @ 06:00 each day
daily_prices_close <- daily_prices_unique %>%
  filter(format(open_time, "%H:%M:%S") == "06:00:00") %>%
  select(open_time, close)  # Keep only relevant columns

# Define processing function
process_df <- function(df) {
  df %>%
    distinct(open_time, .keep_all = TRUE) %>%  # Remove duplicates
    mutate(close_adjusted = close * 1.01)      # Example: Adjust price
}



res_close <- res_unique %>%
  filter(format(open_time, "%H:%M:%S") == "06:00:00") %>%
  select(open_time, close)
result_unique <- results %>%
  distinct(open_time, .keep_all = TRUE)
df_list_cleaned <- map(results, ~ distinct(.x, open_time, .keep_all = TRUE))
