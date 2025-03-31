# write journal with all trades
# https://enricoschumann.net/notes/

# we want finally all paires against USDC, replacing USDT and BTC

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
trade_list_tb_orig <- tibble(
  time = rep(as.POSIXct('2024-12-16 18:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 6), 
  symbol = c('FTMUSDT', 'SUIUSDT', 'ETHUSDT', 'ENAUSDT', 'BTCUSDT', 'AAVEUSDT' ),
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
trade_list_tb <- trade_list_tb %>%
  filter(if_all(everything(), ~ !is.nan(.)))

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
trade_list_final2 <- trade_list_final2 %>%
  mutate(cummulative_quote_qty = ifelse(side == "SELL", -abs(cummulative_quote_qty), cummulative_quote_qty))
trade_list_tb2 <- as_tibble(trade_list_final2)
trade_list_tb2 <- trade_list_tb2 %>%
  filter(if_all(everything(), ~ !is.nan(.)))


###### Join the two periods
all_trade <- bind_rows(trade_list_tb_orig, trade_list_tb, trade_list_tb2)
# order by timestamp
all_trade  <- trade_list_final  %>% arrange(time)


###############################################################################

##############################  TO DO   #######################################
# Retrieve BTC value for token/BTC before we replace BTC by USDC

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
all_trade <- all_trade %>%
  select(-btc_reference_price)
# replace btc price by USDT
all_trade_no_btc <- all_trade %>%
  mutate(
    cummulative_quote_qty = if_else(
      !is.na(btc_price),
      executed_qty * price * btc_price,
      cummulative_quote_qty
    )
  )
# replace USDC by USDT
# TIP: BTC$ & USDC$ ensures only elements ending with "BTC" or 'USDC' are modified.
all_trade_no_btc <- all_trade_no_btc %>%
  mutate(symbol = str_replace(symbol, "BTC$", "USDC"))
# remove uneeded columns
all_trade_final <- all_trade_no_btc %>%
  select(-status, -side, -btc_price)








 #################################################### 
  
# btc_value_extended <- btc_value %>%
#   mutate(
#     symbol = NA_character_,
#     executed_qty = NA_real_,
#     price = NA_real_,
#     status = NA_character_,
#     side = NA_character_,
#     cummulative_quote_qty = NA_real_
#   ) %>%
#   select(names(all_trade), btc_price)  # réorganiser les colonnes dans le bon ordre
# # Ajouter btc_value � la fin de all_trade
# combined <- bind_rows(all_trade, btc_value_extended)
# # on trie par date
# combined_sorted <- combined %>%
#   arrange(time)

####################################################
# return BTC price @ 18:59:59
get_btc_price <- function(my_time) {
  klines <- binance_klines("BTCUSDT", interval = '1m', start_time = as.POSIXct(my_time, tz = 'UTC'),
                           end_time = as.POSIXct(my_time, tz = 'UTC') + 60)
}

# Horodatage réel en UTC
my_time <- as.POSIXct("2025-01-15 21:04:45", tz = "UTC") 
# round à la minute la plus proche
round_time <- round_date(my_time, unit = "minute")

# Fetch la bougie de 1 minute correspondante
price_data <- binance_klines(
  symbol = "BTCUSDT",
  interval = "1m",
  start_time = floored_time,
  end_time = floored_time + 60  # 60 secondes = 1 minute
)

print(price_data)



1 2025-01-15 21:04:45
2 2025-01-19 14:05:23
3 2025-01-19 14:07:30
4 2025-01-19 19:36:24





# example
symbol <- c("BTCUSDT", "BTCUSDT", "ETHUBTC", "AAVEBTC", "ENAUSDT")
time <- c("2024-12-17 10:01:30", "2024-12-18 21:32:53", "2025-01-02
10:34:49", "2025-01-02 11:14:43", "2025-01-02 11:15:22")

get_btc_price <- function(my_time) {
  klines <- binance_klines("BTCUSDT", start_time = my_time, end_time = my_time)
}





df <- all_trade %>%
  mutate(
    BTCUSDT_price = ifelse(str_detect(symbol, "BTC$"), map_dbl(time, get_btc_price), NA_real_)
  )



bk <- binance_klines('SBTC', interval = '1h', start_time = '2025-02-21', end_time = '2025-02-22')
  
  
# replace USDC by USDT
# TIP: BTC$ & USDC$ ensures only elements ending with "BTC" or 'USDC' are modified.
trade_list_tb <- trade_list_tb %>%
  mutate(symbol = str_replace(symbol, "(USDC|BTC)$", "USDT"))

# fetch price 
get_price <- function(my_token, my_time) {
  klines <- binance_klines(my_token, interval = '1h', start_time = as.Date(my_time), end_time = as.Date(my_time) + 1)
}



###################################
# mettre les token en nom de columns, en dessous les cummulative_quote_qty par token et date
df2_wide <- df2 %>%
  select(time, symbol, cummulative_quote_qty) %>%
  pivot_wider(
    names_from = symbol,
    values_from = cummulative_quote_qty
  )

time	SUIUSDC	SUSDC	BTCUSDC
2025-03-05 14:38:47	250010	NA	NA
2025-03-05 20:54:02	250000	NA	NA
2025-03-06 17:50:47	NA	175826	NA
#####################################
# fetch btcusdt price
# Function to get btc  price at 18:00 

# return BTC prices for specific dates
btc_dated_price <- get_price('2025-02-21') # example2025-02-11 


# fetch btcusdt price

 

new.column <- mapply(
  function(symbol, mytime) {
    if (grepl("BTC$", symbol)) {
      get_btc_price(mytime)
    } else
      1
  },
  trade_list_tb$symbol,
  trade_list_tb$time
)


# we need BTCUSDT price for these dates
# find pairs against BTC 
btc_dates <- trade_list_tb %>%
  filter(str_detect(symbol, "BTC$")) %>%
  pull(time)
           

btc_price <- as_tibble(c(rep(1, 20), 99623.99, rep(1, 3),104567.99, 104619.91, 106170.25, 100241.60, 100821.97, 
                         101940.72, rep(1,9), 101841, rep(1,8), 103650, rep(1,5), 97332,  rep(1, 4), 98217, rep(1, 6) ))

# join the two tibbles
joined_tibble <- bind_cols(trade_list_tb, btc_price)

## reorganize the table and rename using journal col names; prices in USDT
# TIP: BTC$ & USDC$ ensures only elements ending with "BTC" or 'USDC' are modified.
complete_trade <- joined_tibble %>%
  mutate(price = price * value) %>%
  select(timestamp = time, instrument = symbol, amount = executed_qty, price = price) %>%
  mutate(instrument = str_replace(instrument, "(USDC|BTC)$", "USDT"))

# add the portfolio day 1
# we have all trades for the period
journal_tb <- rbind(my_journal_orig, complete_trade )
# order by timestamp
journal_tb   <- journal_tb   %>% 
  arrange(timestamp)


# we need to register a sell or buy of BTC for token/BTC trades
# 2025-02-21 06:32:01 SBTC  SELL BTC 2.79999512 @ 105375.70
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

# BTCUSDT sold/bought when trading token/BTC
my_BTC <- tibble(
  timestamp = as.POSIXct(c('2025-01-15 22:04:45', '2025-01-19 15:05:23', '2025-01-19 15:07:30', '2025-01-19 20:36:24',
                           '2025-01-20 01:31:45', '2025-01-20 02:04:42', '2025-01-20 02:32:15', '2025-01-28 11:19:10',
                           '2025-01-30 07:00:43', '2025-02-11 18:58:38', '2025-02-21 06:32:01'), 
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  instrument = rep('BTCUSDT', 11),
  amount = c(-2.66999289, 0.003020, 0.64521457, -2.61438091, -0.003003, -2.60432852, 
             2.61437528, -0.000038, -2.79999512, -2.9723382, -2.79999512),  
  # price = c(95469, 105250, 102814, 101940, 100821, 100241, 106170, 104619, 104567, 99623))
  price = c(99623, 104567, 104619, 106170, 100241, 100821, 101940, 102814, 105250, 95469,105375))

# join journal_tb with my_BTC and original ptf
my_journal_tb <- bind_rows(journal_tb, my_BTC)

# order by timestamp
my_journal_tb <- my_journal_tb  %>% arrange(timestamp)

 my_journal <- journal(
   timestamp = my_journal_tb$timestamp,
   instrument = my_journal_tb$instrument,
   amount = my_journal_tb$amount,
   price = my_journal_tb$price
 )
 
 # Change column order 
 my_journal_tb <- my_journal_tb %>%
   select(amount, price, timestamp, instrument) 
# write the journal
my_journal <- as.journal(my_journal_tb)

## create a tibble for last trading day of journal ##
# list elements
my_position <- position(my_journal)
token <- instrument(my_position)
# as a list
token_ls <- as.list(token)
# pass the list to get_price()
fun <- function(x) get_price(x) # broken
get_AAVE <- 211.85
get_BTC <- 88628.01
get_ENA <- 0.3797
get_ETH <- 2191.2
get_FTM <- NULL
get_SUI <- 2.8162
get_SUSDT <- 0.5451

my_ptf_instrument <- instrument(my_position)
my_ptf_price <- c(211.85, 88628.01, 0.3797, 2191.2, 0, 2.8162, 0.5451)
my_ptf_amount <- c(0, 13.61925879, 419796.64, 0, 0, 107599, 323689.3)
my_ptf_timestamp <- rep(as.POSIXct('2024-12-16 18:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 7)

# get closing price
get_AAVE <- get_price('AAVEUSDT', '2025-03-06')


# end_elements <- unique(as.list(my_journal_tb$instrument))
# get elements
end_elements <- my_journal_tb[,4] %>%
  distinct(instrument)

my_journal_end <- journal(instrument = my_ptf_instrument, 
  timestamp = my_ptf_timestamp, 
  amount = my_ptf_amount, 
  price = my_ptf_price)

my_journal_all <- c(my_journal, my_journal_end)

############# NICE FORMATS ###################################################
# Print the journal in nice formats
# library(kableExtra)
my_journal_org <- knitr::kable(my_journal, format = "org")
# write the journal to a file in md format
my_journal_md <- knitr::kable(my_journal, format = "markdown", caption = 'Trading journal')
writeLines(my_journal_md, "journal.md")

my_journal_html <- knitr::kable(my_journal, format = "html", caption = 'Trading journal')
writeLines(my_journal_html, "journal.html")
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
my_journal %>%
  kbl() %>%
  kable_styling()
my_journal %>%
  kbl() %>%
  kable_classic_2(full_width = F, html_font = 'Inconsolata')

my_journal_rst <- knitr::kable(my_journal, format = "rst", caption = 'Trading journal')
writeLines(my_journal_rst, 'journal.rst')
