# write_journal
# https://enricoschumann.net/notes/

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')



# prevent scientific notation for numbers 8.8214000e+02
options(scipen = 999, digits = 8)
# get our token list
token <- read.table('assets.csv')
# Convert to a Character Vector (Recommended for mapply)
token_list <- as.list(token$V1)

############# PART II ############
binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
start_date2 <- as.POSIXct("2025-01-04", tz = 'UTC')
trade_ls2 <- mapply(binance_all_orders, start_time = start_date2, symbol = token_list, SIMPLIFY = FALSE)
trade_ls_noempty2 <- Filter(function(df) nrow(df) > 0 && all(dim(df) > 0), trade_ls2)
trade_list_filter2 <- lapply(trade_ls_noempty2, function(df) df %>% select(any_of(c('symbol', 
                      'executed_qty', 'cummulative_quote_qty', 'status', 'side', 'time'))))
trade_list2 <- data.table::rbindlist(trade_list_filter2, use.names = TRUE, fill = TRUE)
trade_list2  <- trade_list2  %>% arrange(time)
trade_list2 <- trade_list2 %>% mutate(price = cummulative_quote_qty / executed_qty )
trade_list2 <- trade_list2 %>% mutate(price = round(price, 6))
trade_list_final2 <- trade_list2 %>%
  mutate(executed_qty = ifelse(side == "SELL", -abs(executed_qty), executed_qty))
trade_list_tb2 <- as_tibble(trade_list_final2)
trade_tb2 <- trade_list_tb2 %>%
  filter(if_all(everything(), ~ !is.nan(.)))
trade_tb2 <- trade_tb2 %>%
mutate(symbol = sub("(USDT|BTC)$", "USDC", symbol))


###### Join the two periods and portfolio at day 1
all_trade <- bind_rows(trade_tb_start, trade_tb1, trade_tb2)
# order by timestamp
all_trade  <- all_trade %>% arrange(time)


###############################################################################
# we need here to retrieve BTC value for token/BTC before we replace BTC
get_btc_price <- function(my_time) {
  klines <- binance_klines("BTCUSDT", interval = '1m', limit = 1, start_time = my_time)
  return(klines$close)
}
get_token_price <- function(token, my_time) {
  klines <- binance_klines(token, interval = '1m', limit = 1, start_time = my_time)
  return(as.numeric(klines$close))
}

# date for rows with pair token/BTC
time_ls <- trade_list_final %>%
  filter(str_detect(symbol, "BTC$")) %>%
  pull(time)  
# add a column with BTCUSDT price
trade_list_with_btcusdt <- trade_list_final %>%
  mutate(
    BTCUSDT_price = ifelse(str_detect(symbol, "BTC$"), map_dbl(time, get_btc_price), 1)
  )
# for price in BTC, change it to USDT
trade_list_with_btcusdt <- trade_list_with_btcusdt %>%
  mutate(price = price * BTCUSDT_price)
# for lines with a BTCUSDT price, replace cummulative_quote_qty by executed_qty * ABS(price)
trade_list_with_btcusdt <- trade_list_with_btcusdt %>%
  mutate(cummulative_quote_qty = if_else(
    BTCUSDT_price != 1,
    abs(executed_qty) * price,
    cummulative_quote_qty
  ))
# remove BTCUSDT_price, status, side columns
trade_list <- trade_list_with_btcusdt %>% select(-BTCUSDT_price, -status, -side)
# replace USDT by USDC
# TIP: BTC$ & USDT$ ensures only elements ending with "BTC" or 'USDC' are modified.
trade_list <- trade_list %>%
  mutate(symbol = str_replace(symbol, "(USDT|BTC)$", "USDC"))
# # make cummulative_quote_qty negative when executed_qty is negative
# trade_list_usdc <- trade_list_usdc %>%
#   mutate(cummulative_quote_qty = if_else(
#     executed_qty < 0,
#     -abs(cummulative_quote_qty),
#     cummulative_quote_qty
#   ))

