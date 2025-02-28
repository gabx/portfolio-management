# get_journal
# compute the portfolio PL by token 

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')

binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 


tokens <- read.table('assets.csv')
# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
end_date   <- as.POSIXct("2025-02-17", tz = "UTC")
# convert dates into POSIXct and then into milliseconds since the Unix epoch. 
# The Binance API expects timestamps in milliseconds
# start_ms <- as.numeric(start_date) * 1000
# end_ms   <- as.numeric(end_date) * 1000


# Create a sequence of days between the specified dates
date_seq <- seq(start_date, end_date, by = 'day')
# Convert dates to milliseconds since the Unix epoch
# dates_in_ms <- as.numeric(date_seq) * 1000


# page through the history a day at a time !!
# trades <- binance_all_orders(symbol = "BTCUSDT", start_time = start_date)

# # Function to retrieve trades for a given day and symbol
# get_trades_for_day <- function(day, symbol) {
#   # Define start of day (UTC)
#   start_time <- as.POSIXct(day, tz = "UTC")
#   # Define end of day: one second before the next day begins
#   end_time <- as.POSIXct(day + 1, tz = "UTC") - 1
#   
#   # Retrieve trades for BTCUSDT on that day using binance_mytrades
#   trades <- binance_mytrades(
#     symbol = symbol,
#     start_time = start_time,
#     end_time = end_time
#   )
#   return(trades)
# }

# Function to fetch trades for a specific day
get_trades <- function(date, token) {
  start_time <- as.Date(date)  # Define start_time
  end_time <- start_time + 1   # Compute next day
  
  binance_all_orders(token, start_time = start_time, end_time = end_time)
}
# same but we Convert to milliseconds (* 1000): Since Binance APIs usually expect timestamps in milliseconds.
# get_trades <- function(date, token) {
#   # Convert date to POSIXct
#   start_time <- as.POSIXct(date, tz = "UTC")  # Ensure UTC time
#   end_time <- start_time + 86400  # Add 1 day (86400 seconds)
#   
#   # Convert to milliseconds since Unix epoch
#   start_time_ms <- as.numeric(start_time) * 1000
#   end_time_ms <- as.numeric(end_time) * 1000
#   
#   # Call Binance API function
#   binance_all_orders(token, start_time = start_time_ms, end_time = end_time_ms)
# }

# Convert to a Character Vector (Recommended for mapply)
# token_list <- tokens$V1  
token_list <- as.list(tokens$V1)
# Create all combinations of dates and tokens
# Ensures mapply() gets correctly paired inputs (date and token have the same length)
params <- expand.grid(date = date_seq, token = token_list, stringsAsFactors = FALSE)
# daily_trades_full <- mapply(get_trades, date = params$date, token = params$token, SIMPLIFY = FALSE)
trades_list <- mapply(get_trades, date = params$date, token = params$token, SIMPLIFY = FALSE)


# my_trades_full <- lapply(assets, function(x) daily_trades(x))

# trade_list_noempty <- Filter(function(df) nrow(df) > 0 | ncol(df) > 0, trades_list)
# 
# library(purrr)
# trade_list_noempty <- keep(trades_list, ~ nrow(.x) > 0)
# 
# trade_list_noempty <- lapply(trades_list, function(df) if (nrow(df) > 0) df)
# trade_list_noempty <- trade_list_noempty[!sapply(trade_list_noempty, is.null)]

# remove empty data.frames
trade_list_noempty <- Filter(function(df) nrow(df) > 0 && all(dim(df) > 0), trades_list)

# keep specific columns. We use dplyr
daily_trades_filter <- lapply(trade_list_noempty, function(df) df %>% select(any_of(c('symbol', 
                       'executed_qty', 'cummulative_quote_qty', 'status', 'side', 'time'))))
# remove CANCELED orders             
daily_trades <- lapply(daily_trades_filter, function(df) {
  if("status" %in% names(df)) {
    df %>% dplyr::filter(status != "CANCELED")
  } else {
    df
  }
})
# make one data.frame with all data.frame from the list
trade_list <- data.table::rbindlist(daily_trades, use.names = TRUE, fill = TRUE)
# order by timestamp
trade_list  <- trade_list  %>% arrange(time)
# add a new column price
trade_list <- trade_list %>% mutate(price = cummulative_quote_qty / executed_qty )
# round price to 6 digits
trade_list <- trade_list %>% mutate(price = round(price, 6))

# write the journal
my_journal <- journal(timestamp = trade_list$time, instrument = trade_list$symbol,
                       amount = trade_list$executed_qty, price = trade_list$price)



