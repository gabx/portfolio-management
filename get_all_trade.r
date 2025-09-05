# PART II
  
# write trades 
# https://enricoschumann.net/notes/

############################# FETCH TRADES ##################################
### return trades for assets for a specific period. A list of data frames ###

get_all_trades <- function(tokens,
                           start_date = Sys.getenv("START_DATE"),
                           mid_date   = Sys.getenv("MID_DATE")) {

############# PERIOD I ##########################
binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
trade_ls1 <- mapply(binance_all_orders, start_time = start_date, symbol = token_list, SIMPLIFY = FALSE)
############# PERIOD II ##########################
binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
trade_ls2 <- mapply(binance_all_orders, start_time = mid_date, symbol = token_list, SIMPLIFY = FALSE)

############# CLEAN REORDER #######################

############# CLEAN REORDER #######################
# make one list
whole_trade <- c(trade_ls1, trade_ls2)
# remove empty data.frames from the list
whole_trade_noempty <- Filter(function(df) nrow(df) > 0 && all(dim(df) > 0), whole_trade)
# keep specific columns. We use dplyr
whole_trade_filter <- lapply(whole_trade_noempty, function(df) df %>% select(any_of(c('symbol', 'order_id',
                                                                                     'executed_qty', 'cummulative_quote_qty', 'status', 'side', 'time'))))
# make one data.frame with all data.frame from the list
trade_df<- data.table::rbindlist(whole_trade_filter, use.names = TRUE, fill = TRUE)
# replace FTMUSDT by SUSDT
trade_df <- trade_df %>%
  mutate(symbol = if_else(symbol == "FTMUSDT", "SUSDT", symbol))
# remove duplicate
trade_df_unique <- trade_df[!duplicated(order_id)]
# order by timestamp
trade_df_unique   <- trade_df_unique %>% arrange(time)
# add a new column price
trade_df_unique  <- trade_df_unique  %>% mutate(price = cummulative_quote_qty / executed_qty )
# round price to 6 digits
trade_df_unique  <- trade_df_unique  %>% mutate(price = round(price, 6))
# make executed_qty negative when side is SELL
trade_df_final <- trade_df_unique %>%
  mutate(executed_qty = ifelse(side == "SELL", -abs(executed_qty), executed_qty))
# make cumulative_quote_qty negative when side is SELL
trade_df_final <- trade_df_final %>%
  mutate(cummulative_quote_qty = ifelse(side == "SELL", -abs(cummulative_quote_qty), cummulative_quote_qty))
# transform into a tibble
trade_tb <- as_tibble(trade_df_final)
# Remove rows where any column has NaN
trade_tb <- trade_tb %>%
  filter(if_all(everything(), ~ !is.nan(.)))

################ JOIN ########################

# add day 1 portfolio
all_trade <- bind_rows(trade_tb_start, trade_tb)

################ ALL PAIRS AGAINST USDC ################

# date for rows with pair token/BTC
btc_dates <- all_trade %>%
  filter(str_detect(symbol, "BTC")) %>%      # contient "BTC" n'importe o�
  distinct(time) %>%                         # optionnel: �viter les doublons
  arrange(time) %>%                          # optionnel: trier
  pull(time)

# as tibble
btc_dates_tb <- as_tibble(btc_dates)

# function to retrieve BTC price @ specific time.
# round_date round to the nearest unit
get_btc_price <- function(my_time) {
  kline <- binance_klines("BTCUSDT", interval = '1m', start_time = round_date
                          (as.POSIXct(my_time, tz = 'UTC') - 3600, unit = 'minute'), end_time = as.POSIXct(my_time, tz = 'UTC') + 60)
}  
# apply the function to our dates
btc_with_price <- btc_dates_tb %>%
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
# Extraire les lignes concern\ufffdes par les symboles en BTC
btc_trade_rows <- which(str_detect(all_trade$symbol, "BTC$"))
# Cr�er un vecteur de la m�me taille que all_trade avec que des NA
btc_ref_price_col <- rep(NA_real_, nrow(all_trade))
# Injecter les prix dans les lignes correspondant aux symboles BTC
btc_ref_price_col[btc_trade_rows] <- btc_value$btc_price[seq_along(btc_trade_rows)]
# Ajouter la colonne � la tibble
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
rm('btc_ref_price_col', 'btc_with_price', 
   'all_trade_no_btc', 'btc_trade_rows', 
   'all_trade', 'btc_value', 'token_list',
   'trade_ls2', 'trade_ls1', 'trade_df', 'trade_df_unique',
   'whole_trade', 'whole_trade_noempty')
}
