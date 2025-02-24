# get_journal
# compute the portfolio PL by token 

library('PMwR')
library('binancer')
library('rjson')

binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 

# write portfolio on 2024-12-16
my_journal <- journal(timestamp = '2104-12-07', instrument = c('AAVE', 'BTC', 'ENA', 'USDC'),
                      amount = c(835.68, 20.04765, 432616, 0.23190), price = c(387.87, 107078.55, 1.1853, 1))
# nice formating
my_journal_org <- kable(my_journal, format = "org")



# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
end_date   <- as.POSIXct("2025-02-17", tz = "UTC")
# convert dates into POSIXct and then into milliseconds since the Unix epoch. 
# The Binance API expects timestamps in milliseconds
start_ms <- as.numeric(start_date) * 1000
end_ms   <- as.numeric(end_date) * 1000


# Create a sequence of days between the specified dates
date_seq <- seq(start_date, end_date, by = 'day')
# Convert dates to milliseconds since the Unix epoch
dates_in_ms <- as.numeric(date_seq) * 1000


# page through the history a day at a time !!
trades <- binance_mytrades(symbol = "BTCUSDT", start_time = as.numeric(start_date), end_time = as.numeric(end_date))
trades <- binance_mytrades(symbol = "BTCUSDT", start_time = start_date, end_time = end_date)

# Function to retrieve trades for a given day and symbol
get_trades_for_day <- function(day, symbol) {
  # Define start of day (UTC)
  start_time <- as.POSIXct(day, tz = "UTC")
  # Define end of day: one second before the next day begins
  end_time <- as.POSIXct(day + 1, tz = "UTC") - 1
  
  # Retrieve trades for BTCUSDT on that day using binance_mytrades
  trades <- binance_mytrades(
    symbol = symbol,
    start_time = start_time,
    end_time = end_time
  )
  return(trades)
}



# Apply the function to each day using lapply
daily_trades <- lapply(date_seq, get_trades_for_day)
get.journal <- function(number_of_days) {


    
    end.day = as.Date(format(Sys.Date(), "%Y-%m-%d"))
    number_of_days <- 20
    start.day <- end.day - number_of_days
    
# assets
    my.assets <- read_table('assets.csv', col_names = 'assets')

    add_row(my.assets, assets = 'USDC')

# get a list of days to apply binance_all_orders()
# no more than 24 hours between start and end time
    trading_days <- seq(start_day, by = 'day', length.out = number_of_days)

    my_trades <- function(x) {
    Map(function(y,z) binance_all_orders(x, start_time = y, end_time = z), head(trading_days, -1), tail(trading_days, -1))
}
    my.trades.full <- lapply(mlc$asset.usdt, function(x) my_trades(x))
    non_empty_df <- function(l) {
    lapply(l, function(df) df[sapply(df, function(df) nrow(df) !=0)])
    # return(l)
}
# remove any empty list or data frame
    my.trades <- non_empty_df(my.trades.full)
    my.trades <- Filter(function(x) length(x) > 0, my.trades)

# filter status, keep only needed columns, replace buy and sell by 1 and -1
    transform_function <- function(df) {
        df <- filter(df, status == 'FILLED')
        df <- select(df, symbol, price, executed_qty, cummulative_quote_qty, side, time)
        df <- df |> mutate(price = cummulative_quote_qty / executed_qty)
        df <- df |> mutate(executed_qty = if_else(side == 'BUY', executed_qty, executed_qty * -1))
    }                    
    #df <- df |> mutate(executed_qty = ifelse(side == 'BUY', executed_qty, ifelse(side == 'SELL', executed_qty * -1, executed_qty)))
    # cummulative_quote_qty = if_else(side == 'BUY', cummulative_quote_qty, cummulative_quote_qty * -1))
    # df <- df |> mutate(side = as.numeric(ifelse(side == 'BUY', 1, ifelse(side == 'SELL', -1, side))))
    # df <- df |> mutate(side = if_else(side == 'BUY', 1, -1))
    # df <- df |> mutate(cummulative_quote_qty = cummulative_quote_qty * side)
    # df <- df |> select(symbol, executed_qty, cummulative_quote_qty)

# result is a list of data frames with all FILLED trades for the past week
my.trades.transformed <- lapply(my.trades, function(lst) {
         lapply(lst, transform_function)
})

# convert our list of df to one tbl
my.trades.week <- as_tibble(my.trades.transformed |> bind_rows())

# change some columns
my.trades.week <- my.trades.week |> select(symbol, price, executed_qty, side, time)
my.trades.week <- my.trades.week |> mutate(time = as.Date(time))
my.trades.week <- my.trades.week |> rename(instrument = symbol, amount = executed_qty)
# from tibble to journal class
my.journal <- as.journal(my.trades.week)
my.journal <- sort(my.journal, by = 'timestamp', decreasing = TRUE)

}

# my.journal.org <- toOrg(my.journal)
# aggregate(. ~ asset, data = my.trades.week, FUN = sum)
