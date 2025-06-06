# PART II
  
# write trades 
# https://enricoschumann.net/notes/

# we want finally all paires against USDC, replacing USDT and BTC

################### STARTER #####################################

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')
library(stringr)
library(tibble)

# prevent scientific notation for numbers 8.8214000e+02
options(scipen = 999, digits = 8)
# get our token list
token <- read.table('assets.csv')
# Convert to a Character Vector (Recommended for mapply)
token_list <- as.list(token$V1)
# journal au 16 dec
# trade_tb_start <- tibble(
#   time = as.POSIXct('2024-12-16 18:02:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 
#   as.POSIXct('2024-12-16 18:04:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
#   as.POSIXct('2024-12-16 18:06:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
#   as.POSIXct('2024-12-16 18:08:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
#   as.POSIXct('2024-12-16 18:10:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
#   as.POSIXct('2024-12-16 18:12:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 
#   symbol = c('FTMUSDC', 'SUIUSDC', 'ETHUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC' ),
#   executed_qty = c(284270, 80024, 91.8981, 207836, 9.3372107, 1152.058), 
#   price = c(1.382, 4.7256, 4038, 1.1855, 107012, 386.55),
#   status = rep('FILLED', 6),
#   side = rep('BUY', 6),
#   cummulative_quote_qty = executed_qty * price) 

##########################

# we replaced FTM by S (Sonic) as FTM changed to S 
trade_tb_start <- tibble(
  time = as.POSIXct(c(
    '2024-12-16 18:02:00',
    '2024-12-16 18:04:00',
    '2024-12-16 18:06:00',
    '2024-12-16 18:08:00',
    '2024-12-16 18:10:00',
    '2024-12-16 18:12:00'
  ), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  symbol = c('SUSDC', 'SUIUSDC', 'ETHUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC'),
  executed_qty = c(284270, 80024, 91.8981, 207836, 9.3372107, 1152.058),
  price = c(1.382, 4.7256, 4038, 1.1855, 107012, 386.55),
  status = rep('FILLED', 6),
  side = rep('BUY', 6)
) |>
  mutate(cummulative_quote_qty = executed_qty * price)
########################################################

##################### FETCH TRADES ##############################

############# PERIOD I ##########################
binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
# Define start and end dates in POSIXct format (UTC)
#start_date <- as.numeric(as.POSIXct("2024-12-16", tz = 'UTC'))
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
# return trades for assets for a specific period. A list of data frames
trade_ls <- mapply(binance_all_orders, start_time = start_date, symbol = token_list, SIMPLIFY = FALSE)
############# PERIOD II ##########################
binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
start_date2 <- as.POSIXct("2025-03-04", tz = 'UTC')
trade_ls2 <- mapply(binance_all_orders, start_time = start_date2, symbol = token_list, SIMPLIFY = FALSE)

############# CLEAN REORDER #######################
# make one list
two_period_trade <- c(trade_ls, trade_ls2)
# remove empty data.frames from the list
trade_ls_noempty <- Filter(function(df) nrow(df) > 0 && all(dim(df) > 0), two_period_trade)
# keep specific columns. We use dplyr
trade_list_filter <- lapply(trade_ls_noempty, function(df) df %>% select(any_of(c('symbol', 'order_id',
        'executed_qty', 'cummulative_quote_qty', 'status', 'side', 'time'))))
# make one data.frame with all data.frame from the list
trade_list <- data.table::rbindlist(trade_list_filter, use.names = TRUE, fill = TRUE)
# replace FTMUSDT by SUSDT
trade_list <- trade_list %>%
  mutate(symbol = if_else(symbol == "FTMUSDT", "SUSDT", symbol))
# remove duplicate
trade_list_unique <- trade_list[!duplicated(order_id)]
# order by timestamp
trade_list_unique   <- trade_list_unique %>% arrange(time)
# add a new column price
trade_list_unique  <- trade_list_unique  %>% mutate(price = cummulative_quote_qty / executed_qty )
# round price to 6 digits
trade_list_unique  <- trade_list_unique  %>% mutate(price = round(price, 6))
# make executed_qty negative when side is SELL
trade_list_final <- trade_list_unique %>%
  mutate(executed_qty = ifelse(side == "SELL", -abs(executed_qty), executed_qty))
# make cumulative_quote_qty negative when side is SELL
trade_list_final <- trade_list_final %>%
  mutate(cummulative_quote_qty = ifelse(side == "SELL", -abs(cummulative_quote_qty), cummulative_quote_qty))
# transform into a tibble
trade_list_tb <- as_tibble(trade_list_final)
# Remove rows where any column has NaN
trade_list_tb <- trade_list_tb %>%
  filter(if_all(everything(), ~ !is.nan(.)))

################ JOIN ########################

# add day 1 portfolio
all_trade <- bind_rows(trade_tb_start, trade_list_tb)

################ ALL PAIRS AGAINST USDC ################

# date for rows with pair token/BTC
btc_time_ls <- all_trade %>%
  filter(str_detect(symbol, "BTC$")) %>%
  pull(time) 
# as tibble
btc_time_tb <- as_tibble(btc_time_ls)

# function to retrieve BTC price @ specific time.
# round_date round to the nearest unit
get_btc_price <- function(my_time) {
  kline <- binance_klines("BTCUSDT", interval = '1m', start_time = round_date
                          (as.POSIXct(my_time, tz = 'UTC') - 3600, unit = 'minute'), end_time = as.POSIXct(my_time, tz = 'UTC') + 60)
}  
# apply the function to our dates
btc_with_price <- btc_time_tb %>%
  mutate(kline = map(value, get_btc_price))
# unnest to get the full table
btc_with_price <- btc_with_price %>%
  unnest(kline)
# keep only first line for every date, open_time and open columns, rename them
btc_value <- btc_with_price %>%
  group_by(value) %>%
  slice(1) %>%
  ungroup() %>%
  select(open_time, open) %>%
  rename_with(~c('time', 'btc_price'))


# we add BTC price for lines with paires against BTC
# Extraire les lignes concern�es par les symboles en BTC
btc_trade_rows <- which(str_detect(all_trade$symbol, "BTC$"))
# Créer un vecteur de la même taille que all_trade avec que des NA
btc_ref_price_col <- rep(NA_real_, nrow(all_trade))
# Injecter les prix dans les lignes correspondant aux symboles BTC
btc_ref_price_col[btc_trade_rows] <- btc_value$btc_price[seq_along(btc_trade_rows)]
# Ajouter la colonne à la tibble
all_trade <- all_trade %>%
  mutate(btc_price = btc_ref_price_col)
#all_trade <- all_trade %>%
  #select(-btc_reference_price)
# replace btc price by USDT
all_trade_no_btc <- all_trade %>%
  mutate(
    cummulative_quote_qty = if_else(
      !is.na(btc_price),
      executed_qty * price * btc_price,
      cummulative_quote_qty
    )
  )
# replace BTC by USDC
# TIP: BTC$ & USDC$ ensures only elements ending with "BTC" or 'USDC' are modified.
all_trade_no_btc <- all_trade_no_btc %>%
  mutate(symbol = str_replace(symbol, "BTC$|USDT", "USDC"))
# remove uneeded columns
# here is our final trade table with all pairs and prices in USDC
all_trade_final <- all_trade_no_btc %>%
  select(-status, -side, -btc_price, -order_id)

# clean environment
rm('btc_ref_price_col', 'btc_with_price', 'btc_time_tb', 'btc_time_ls', 'trade_list_unique',
   'all_trade_no_btc', 'btc_trade_rows', 'trade_ls_noempty', 'trade_list_final',
  'trade_list_tb', 'all_trade', 'btc_value', 'token', 'token_list', 'token_list_filter',
  'trade_ls', 'trade_ls2', 'two_period_trade', 'trade_list', 'trade_list_filter')





