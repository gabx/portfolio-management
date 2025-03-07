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
  timestamp = rep(as.POSIXct('2024-12-16 18:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 6), 
  instrument = c('FTMUSDT', 'SUIUSDT', 'ETHUSDT', 'ENAUSDT', 'BTCUSDT', 'AAVEUSDT' ),
  amount = c(284270, 80024, 91.8981, 207836, 9.3372107, 1152.058), 
  price = c(1.382, 4.7256, 4038, 1.1855, 107012, 386.55))

# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
end_date   <- as.POSIXct("2025-03-03", tz = "UTC")
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

##############################  TO DO   #######################################
# we need here to retrieve BTC value for token/BTC before we replace BTC
# Filtrer les lignes o� `column_name` se termine par "BTC" et extraire `value_column`

# example
symbol <- c("BTCUSDT", "BTCUSDT", "ETHUBTC", "AAVEBTC", "ENAUSDT")
time <- c("2024-12-17 10:01:30", "2024-12-18 21:32:53", "2025-01-02
10:34:49", "2025-01-02 11:14:43", "2025-01-02 11:15:22")



# date for rows with pair token/BTC
time_ls <- trade_list_tb %>%
  filter(str_detect(symbol, "BTC$")) %>%
  pull(time)  # Remplace `value_column` par la colonne � r�cup�rer


df <- trade_list_tb %>%
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
  klines <- binance_klines(my_token, start_time = my_time, end_time = my_time)
}


# fetch btcusdt price
# Function to get btc  price at 18:00 
get_btc_price <- function(my_time) {
  # my_time_ms <- as.integer(as.numeric(my_time) * 1000)
  klines <- binance_klines("BTCUSDT", interval = '1h', start_time = as.Date(my_time), end_time = as.Date(my_time) + 1)
  return(as.numeric(klines$close[[18]]))
}


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
           
# return BTC prices for specific dates
btc_dated_price <- get_price('2025-02-21') # example2025-02-11 

get_price <- function(my_token, my_time) {
  klines <- binance_klines(my_token, start_time = as.Date(my_time), end_time = as.Date(my_time) + 1)
  #return(klines[[5]])
}

# fetch btcusdt price
get_btc_price <- function(my_time) {
  # my_time_ms <- as.integer(as.numeric(my_time) * 1000)
  klines <- binance_klines("BTCUSDT", interval = '1h', start_time = as.Date(my_time), end_time = as.Date(my_time) + 1)
  return(as.numeric(klines$close[[18]]))
}


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
my_journal_tb <- bind_rows(journal_tb, my_BTC, my_journal_orig)

# order by timestamp
my_journal_tb <- my_journal_tb  %>% arrange(timestamp)

 
# write the journal
my_journal <- as.journal(my_journal_tb)






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
