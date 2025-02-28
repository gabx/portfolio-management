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
# Create a sequence of days between the specified dates
date_seq <- seq(start_date, end_date, by = 'day')


# Function to fetch trades for a specific day
get_trades <- function(date, token) {
  start_time <- as.Date(date)  # Define start_time
  end_time <- start_time + 1   # Compute next day
  
  binance_all_orders(token, start_time = start_time, end_time = end_time)
}


# Convert to a Character Vector (Recommended for mapply)
token_list <- as.list(tokens$V1)
# Create all combinations of dates and tokens
# Ensures mapply() gets correctly paired inputs (date and token have the same length)
params <- expand.grid(date = date_seq, token = token_list, stringsAsFactors = FALSE)

# return trades for assets for a specific period
trades_list <- mapply(get_trades, date = params$date, token = params$token, SIMPLIFY = FALSE)



## SOME CLEANING AND REORDER ##
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



