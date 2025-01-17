# create a trading journal with PMwR package

library('PMwR')
library('binancer')
library('zoo')

binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 

first.day = as.Date('2024-12-15')
last.day = as.Date(format(Sys.Date(), "%Y-%m-%d"))


# example
# j <- journal(timestamp = 1:3,
#             amount = c(1,2,3),
#             price = 101:103,
#             instrument = c("Stock A", "Stock A", "Stock B"))

# need first to download Binance trades
# Fetch all orders from the Binance account
  





binance_all_orders(symbol, order_id, start_time, end_time, limit)

# keep symbol, executed_qty, time, cumulative_quote_qy
# price (= cummulative_quote_qy/executed_qy)
# filter status, keep only needed columns, replace buy and sell by 1 and -1
transform_function <- function(df) {
  df <- filter(df, status == 'FILLED')
  df <- select(df, symbol, price, executed_qty, cummulative_quote_qty, side, time)
  df <- df |> mutate(price = cummulative_quote_qty / executed_qty)
  df <- df |> mutate(executed_qty = if_else(side == 'BUY', executed_qty, executed_qty * -1))
}

# return orders in a list of data frames
btc.orders <- lapply(my.portfollio, function(x) binance_all_orders(x))



# get a list of days to apply binance_all_orders()
# no more than 24 hours between start and end time
trading.days <- seq(start.day, by = 'day', length.out = number_of_days)

my_trades <- function(x) {
  Map(function(y,z) binance_all_orders(x, start_time = y, end_time = z), head(trading.days, -1), tail(trading.days, -1))
}

btc.orders <- binance_all_orders('BTCUSDT')
