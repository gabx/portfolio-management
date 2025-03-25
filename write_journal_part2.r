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
trade_list_tb2 <- trade_list_tb2 %>%
  filter(if_all(everything(), ~ !is.nan(.)))


###### Join the two periods
trade_list_final <- bind_rows(trade_list_tb_orig, trade_list_tb, trade_list_tb2)
# order by timestamp
trade_list_final  <- trade_list_final  %>% arrange(time)


###############################################################################

##############################  TO DO   #######################################
# we need here to retrieve BTC value for token/BTC before we replace BTC
# Filtrer les lignes o� `column_name` se termine par "BTC" et extraire `value_column`

# example
symbol <- c("BTCUSDT", "BTCUSDT", "ETHUBTC", "AAVEBTC", "ENAUSDT")
time <- c("2024-12-17 10:01:30", "2024-12-18 21:32:53", "2025-01-02
10:34:49", "2025-01-02 11:14:43", "2025-01-02 11:15:22")

get_btc_price <- function(my_time) {
  klines <- binance_klines("BTCUSDT", start_time = my_time, end_time = my_time)
}


# date for rows with pair token/BTC
time_ls <- trade_list_final %>%
  filter(str_detect(symbol, "BTC$")) %>%
  pull(time)  # Remplace `value_column` par la colonne � r�cup�rer


df <- trade_list_final %>%
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

get_btc_price <- function(my_time) {
  klines <- binance_klines("BTCUSDT", interval = '1h', start_time = as.Date(my_time), end_time = as.Date(my_time) + 1)
  return(as.numeric(klines$close[[18]]))
}


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
