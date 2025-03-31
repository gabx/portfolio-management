# we split the script in 2 parts due to changing binance credentials #

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
# journal au 16 dec
trade_tb_start <- tibble(
  time = rep(as.POSIXct('2024-12-16 18:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 6), 
  symbol = c('FTMUSDC', 'SUIUSDC', 'ETHUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC' ),
  executed_qty = c(284270, 80024, 91.8981, 207836, 9.3372107, 1152.058), 
  price = c(1.382, 4.7256, 4038, 1.1855, 107012, 386.55),
  status = rep('FILLED', 6),
  side = rep('BUY', 6),
  cummulative_quote_qty = executed_qty * price) 

################## WRITE ONE DATA FRAME WITH ALL OUR TRADES #################
# we have to split in 2 parts due to the change of credentials the 01-05

################## PART I ################
binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')

# return trades for assets for a specific period. A list of data frames
trade_ls <- mapply(binance_all_orders, start_time = start_date, symbol = token_list, SIMPLIFY = FALSE)

## SOME CLEANING AND REORDER ##
# remove empty data.frames from the list
trade_ls_noempty <- Filter(function(df) nrow(df) > 0 && all(dim(df) > 0), trade_ls)
# keep specific columns. We use dplyr
trade_list_filter <- lapply(trade_ls_noempty, function(df) df %>% select(any_of(c('symbol', 
                                                                                  'executed_qty', 'cummulative_quote_qty', 'status', 'side', 'time'))))
# make one data.frame with all data.frame from the list
trade_list <- data.table::rbindlist(trade_list_filter, use.names = TRUE, fill = TRUE)
# order by timestamp
trade_list  <- trade_list  %>% arrange(time)
# add a new column price
trade_list <- trade_list %>% mutate(price = cummulative_quote_qty / executed_qty )
# round price to 6 digits
trade_list <- trade_list %>% mutate(price = round(price, 6))
# make executed_qty negative when side is SELL
trade_list_final <- trade_list %>%
  mutate(executed_qty = ifelse(side == "SELL", -abs(executed_qty), executed_qty))
# make cumulative_quote_qty negative when side is SELL
trade_list_final <- trade_list_final %>%
  mutate(cummulative_quote_qty = ifelse(side == "SELL", -abs(cummulative_quote_qty), cummulative_quote_qty))
# transform into a tibble
trade_list_tb <- as_tibble(trade_list_final)
# Remove rows where any column has NaN
trade_tb1 <- trade_list_tb %>%
  filter(if_all(everything(), ~ !is.nan(.)))
# replace USDT with USDC
trade_tb1 <- trade_tb1 %>%
  mutate(symbol = sub("USDT$", "USDC", symbol))
