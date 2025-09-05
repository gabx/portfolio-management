# write a data.frame with daily close of token
# we will keep 23:59:59 UTC as close time


binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
token <- read.table('assets.csv') # !! no SUSDT price before 01-16. Removed from csv




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
  filter(!V1 %in% c("FTMUSDC"))



# function to get prices
get_price <- function(my_token, start_date) {
  klines <- binance_klines(my_token, interval = '1h', start_time = start_date)
  return(klines)
}

# we need to call x times the get_price() to get all the needed days
# thus the fun() get_daily_close()
    get_daily_close <- function(i) {
        # base_date <- as.POSIXct('2024-12-15 23:59:59', tz = 'UTC')
       base_date <- start_date
        first_day <- base_date + (i - 1) * 20
        result <- token_tib %>%
          mutate(data = map(symbol, ~ get_price(.x, first_day))) %>%
          mutate(token = symbol) %>%
          select(-symbol) %>%
          unnest(data) %>%
          filter(format(close_time, "%H:%M:%S") == "23:59:59") %>%
          select(token, close_time, close)
        assign(paste0("get_daily_close", i), result, envir = .GlobalEnv)
        return(result)
    }

    
# sort by time and regroup token  
token_daily_close <- token_daily_close %>%
       arrange(token, close_time)

# remove duplicate
token_daily_close <- token_daily_close %>%
  distinct(token, close_time, .keep_all = TRUE)
# put token as column names
# we finally have our data frame with closing prices @ 23:59:59
token_daily_close <- token_daily_close %>%
  pivot_wider(names_from = token, values_from = close)

# replace NA with 0.6979 price for SUSDC
all_token_daily_close <- token_daily_close_final %>%
  mutate(SUSDC = if_else(is.na(SUSDC), 0.6979, SUSDC))








