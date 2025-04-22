# write a data.frame with daily close of token
# we will keep 23:59:59 UTC as close time

library('PMwR')
library('binancer')
library('rjson')
library('dplyr')
library(purrr)
library('lubridate')
library('PerformanceAnalytics')


binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
token <- read.table('assets.csv') # !! no SUSDT price before 01-16. Removed from csv

# Define start and end dates in POSIXct format (UTC)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
# end_date   <- as.POSIXct(today(), tz = 'UTC')

## portfolio token as a tibble ##
# replace BTC & USDT with USDC at the end of the token names
token_usdc <- token %>%
  mutate(V1 = sub("BTC$|USDT$", "USDC", V1))
# remove duplicate
token_usdc <- token_usdc %>%
  distinct()
# transform to a tibble
token_usdc <- unnest(token_usdc, cols = V1)
# we need to remove FTM from 2025-01-13 (i = 3) as it doesn't exist anymore and
# split an error and remove SUSDC as it exists much later
token_usdc <- token_usdc %>% 
  filter(!V1 %in% c("FTMUSDC", 'SUSDC'))

# function to get prices
get_price <- function(my_token, start_date) {
  klines <- binance_klines(my_token, interval = '1h', start_time = start_date)
  return(klines)
}

# we need to call 6 times the get_price() to get all the needed days
# thus the fun() get_daily_close()
    get_daily_close <- function(i) {
        # base_date <- as.POSIXct('2024-12-15 23:59:59', tz = 'UTC')
       base_date <- as.Date('2024-12-16')
        start_date <- base_date + (i - 1) * 20
        result <- token_usdc %>%
          mutate(data = map(V1, ~ get_price(.x, start_date))) %>%
          mutate(token = V1) %>%
          select(-V1) %>%
          unnest(data) %>%
          filter(format(close_time, "%H:%M:%S") == "23:59:59") %>%
          select(token, close_time, close)
        assign(paste0("get_daily_close", i), result, envir = .GlobalEnv)
        return(result)
    }

################################################
# periods of 19 days
# token_daily_close1 : 2024-12-16 --> 2025-01-04
# token_daily_close1 : 2025-01-05 --> 2025-01-24
#                      2025-01-25 --> 2025-02-13
#                      2025-02-14 --> 2025-03-05
#                      2025-03-06 --> 2025-03-25
#                      2025-03-26 --> 2025-04-14
################################################

token_daily_close <- bind_rows(get_daily_close1, get_daily_close2, get_daily_close3,
                               get_daily_close4, get_daily_close5, get_daily_close6)
token_daily_close <- token_daily_close %>%
       arrange(token, close_time)

# remove duplicate
token_daily_close <- token_daily_close %>%
  distinct(token, close_time, .keep_all = TRUE)
# put token as column names
# we finally have our data frame with closing prices @ 23:59:59
token_daily_close_final <- token_daily_close %>%
  pivot_wider(names_from = token, values_from = close)

# we shall put FTMUSDC & SUSDC in our token list
token_usdc <- token_usdc %>%
  add_row(V1 = 'SUSDC')


############## add data for FTM and S ###################################
#### FTM ######
# we nned prices from 12-16 till 2025-01-15 include
ftm1 <- get_price('FTMUSDC', '2024-12-16')
ftm2 <- get_price('FTMUSDC', '2025-01-05') # ends 01-13 03:00. Thus we need NA till
# 01-15 
ftm <- as_tibble(bind_rows(ftm1, ftm2))
# remove duplicate
ftm <- ftm %>%
  distinct()
# change FTMUSDC to SUSDC
ftm_to_s <- ftm %>%
  mutate(symbol = if_else(symbol == "FTMUSDC", "SUSDC", symbol))

# add some rows with NA from 01-13 03:00 to 01-16 08:00
add_missing_rows <- function(df, last_open, until_open) {
  # Crée une séquence de temps horaires
  new_open_times <- seq(from = as.POSIXct(last_open, tz = "UTC"),
                        to   = as.POSIXct(until_open, tz = "UTC"),
                        by   = "1 hour")
  # Calcule les close_time
  new_close_times <- new_open_times + minutes(59) + seconds(59)
  # Récupère les noms de colonnes
  cols <- names(df)
  # Crée une tibble vide avec les colonnes attendues
  na_part <- tibble(open_time  = new_open_times,
                    high       = NA_real_,
                    low        = NA_real_,
                    close      = NA_real_,
                    open       = NA_real_,
                    volume     = NA_real_,
                    close_time = new_close_times,
                    quote_asset_volume          = NA_real_,
                    trades                      = NA_integer_,
                    taker_buy_base_asset_volume = NA_real_,
                    taker_buy_quote_asset_volume= NA_real_,
                    symbol     = "SUSDC")
  # Réordonne les colonnes comme dans df
  na_part <- na_part[, cols]
  # Combine les deux
  bind_rows(df, na_part)
}

ftm_to_s_extended <- add_missing_rows(ftm, last_open = "2025-01-13 03:00:00", until_open = "2025-01-16 08:00:00")

# select 23:59:59 and specific columns
s_close <- ftm_to_s_extended %>%
  filter(format(close_time, "%H:%M:%S") == "23:59:59") %>%
  select(symbol, close_time, close) %>%
  rename(token = symbol)
# put token as column names
s_daily_close_final <- s_close %>%
  pivot_wider(names_from = token, values_from = close)

##### FINALLY ##########
# join s closing prices with other otken
all_token_daily_close_final <- left_join(s_daily_close_final, token_daily_close_final,  by = "close_time")
# replace NA in SUSDC column by values from s_daily_close_final
token_daily_close_final <- rows_update(token_daily_close_final, s_daily_close_final, by = "close_time", unmatched = 'ignore')






