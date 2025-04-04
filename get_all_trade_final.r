# write trades 
# https://enricoschumann.net/notes/

# we want finally all paires against USDC, replacing USDT and BTC

################### STARTER #####################################

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')
library(stringr)

# prevent scientific notation for numbers 8.8214000e+02
options(scipen = 999, digits = 8)
# get our token list
token <- read.table('assets.csv')
# Convert to a Character Vector (Recommended for mapply)
token_list <- as.list(token$V1)
# journal au 16 dec
trade_tb_start <- tibble(
  time = rep(as.POSIXct('2024-12-16 18:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 6), 
  symbol = c('FTMUSDC', 'SUIUSDC', 'ETHUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC' ),
  executed_qty = c(284270, 80024, 91.8981, 207836, 9.3372107, 1152.058), 
  price = c(1.382, 4.7256, 4038, 1.1855, 107012, 386.55),
  status = rep('FILLED', 6),
  side = rep('BUY', 6),
  cummulative_quote_qty = executed_qty * price) 

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






