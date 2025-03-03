# write_journal
# compute the portfolio PL by token 
# https://enricoschumann.net/notes/

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')


binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
tokens <- read.table('assets.csv')

# journal au 16 dec
my_journal_orig <- tibble(
  timestamp = rep(as.POSIXct('2024-12-16 00:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 4), 
  instrument = c('AAVEUSDT', 'BTCUSDT', 'ENAUSDT', 'USDT'),
  amount = c(835.68, 20.04765, 432616, 0.23190), 
  price = c(387.87, 107078.55, 1.1853, 1))

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

# return trades for assets for a specific period. A list of data frames
trades_ls <- mapply(get_trades, date = params$date, token = params$token, SIMPLIFY = FALSE)



## SOME CLEANING AND REORDER ##
# remove empty data.frames from the list
trade_list_noempty <- Filter(function(df) nrow(df) > 0 && all(dim(df) > 0), trades_ls)

# keep specific columns. We use dplyr
daily_trades_filter <- lapply(trade_list_noempty, function(df) df %>% select(any_of(c('symbol', 
                       'executed_qty', 'cummulative_quote_qty', 'status', 'side', 'time'))))
# remove CANCELED orders 
# we need to keep canceled as they are partially field
# daily_trades <- lapply(daily_trades_filter, function(df) {
#   if("status" %in% names(df)) {
#     df %>% dplyr::filter(status != "CANCELED")
#   } else {
#     df
#   }
# })
# make one data.frame with all data.frame from the list
trade_list <- data.table::rbindlist(daily_trades_filter, use.names = TRUE, fill = TRUE)
# order by timestamp
trade_list  <- trade_list  %>% arrange(time)
# add a new column price
trade_list <- trade_list %>% mutate(price = cummulative_quote_qty / executed_qty )
# round price to 6 digits
trade_list <- trade_list %>% mutate(price = round(price, 6))

# make executed_qty negative when side is SELL
trade_list_final <- trade_list %>%
  mutate(executed_qty = ifelse(side == "SELL", -abs(executed_qty), executed_qty))

# transform into a tibble
trade_list_tb <- as_tibble(trade_list_final)
# Remove rows where any column has NaN
trade_list_tb <- trade_list_tb %>%
  filter(if_all(everything(), ~ !is.nan(.)))
# replace USDC by USDT
trade_list_tb <- trade_list_tb %>%
  mutate(symbol = str_replace(symbol, "USDC", "USDT"))

# we need BTCUSDT price for these dates
dates <- c('2025-01-15 22:04:45', '2025-01-19 15:05:23', '2025-01-19 15:07:30', '2025-01-19 20:36:24', 
           '2025-01-20 01:31:45', '2025-01-20 02:04:42', '2025-01-20 02:32:15', '2025-01-28 11:19:10', 
           '2025-01-30 07:00:43','2025-02-11 18:58:38')
            
           
           
# return BTC prices for specific dates
btc_price_date <- sapply(dates, function(dt) get_binance_price("BTCUSDT", dt))

btc_price <- as_tibble(c(rep(1, 20), 99623.99, rep(1, 3),104567.99, 104619.91, 106170.25, 100241.60, 100821.97, 
                         101940.72, rep(1,9), 102814.36, rep(1,8),  105250.71, rep(1,5), 95469.28, 1, 1))

# join the two tibbles
joined_tibble <- bind_cols(trade_list_tb, btc_price)

# price in USDT 
final_tb <- joined_tibble %>%
  mutate(mul = price * value) %>%
  select(-price, -value)
# rename mul column
final_tb <- final_tb %>%
  rename(price = mul)
# remove FTM
final_tb <- final_tb %>% slice(-9)
final_tb <- final_tb %>% slice(-14)
# keep specific columns and rename them
final_tb <- final_tb %>%
  select(timestamp = time, instrument = symbol, amount = executed_qty, price = price)
# add the portfolio day 1
journal_tb <- rbind(my_journal_orig, final_tb)

# we need to register a sell or buy of BTC for token/BTC trades
# 2025-02-11 18:58:38 SUIBTC SELL BTC 2.9723382 @ 95469
# 2025-01-30 07:00:43 SUIBTC SELL BTC 2.79999512 @ 105250
# 2025-01-28 11:19:10 SUIBTC SELL BTC 0.000038 @ 102814
# 2025-01-20 02:32:15 AAVEBTC BUY BTC 2.61437528 @ 101940
# 2025-01-20 02:04:42 AAVEBTC SELL BTC 2.60432852 @ 100821
# 2025-01-20 01:31:45 AAVEBTC SELL BTC 0.003003 @ 100241
# 2025-01-19 20:36:24 AAVEBTC SELL BTC 2.61438091 @ 106170
# 2025-01-19 15:07:30 AAVEBTC BUY BTC 0.64521457 @ 104619
# 2025-01-19 15:05:23 AAVEBTC BUY BTC 0.003020 @ 104567
# 2025-01-15 22:04:45 AAVEBTC SELL BTC 2.66999289 @ 99623
my_BTC <- tibble(
  timestamp = as.POSIXct(c('2025-02-11 18:58:38', '2025-01-30 07:00:43', '2025-01-28 11:19:10', 
                           '2025-01-20 02:32:15', '2025-01-20 02:04:42', '2025-01-20 01:31:45',
                           '2025-01-19 20:36:24', '2025-01-19 15:07:30', '2025-01-19 15:05:23',
                           '2025-01-15 22:04:45'),
                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  instrument = rep('BTCUSDT', 10),
  amount = c(-2.9723382, -2.79999512, -0.000038, 2.61437528, -2.60432852, -0.003003,
             -2.61438091, 0.64521457, 0.003020, -2.66999289), 
  price = c(95469, 105250, 102814, 101940, 100821, 100241, 106170, 104619, 104567, 99623))

# change instrument name in first column
journal_tb[[1]] <- ifelse(final_tb[[1]] == "AAVEBTC", "AAVEUSDT", final_tb[[1]])
journal_tb[[1]] <- ifelse(final_tb[[1]] == "SUIBTC", "SUIUSDT", final_tb[[1]])
# join journal_tb with my_BTC
my_journal_tb <- bind_rows(journal_tb, my_BTC)
# order by timestamp
my_journal_tb <- my_journal_tb  %>% arrange(timestamp)


# write the journal
my_journal <- as.journal(my_journal_tb)




