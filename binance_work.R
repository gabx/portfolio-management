# modify the my_journal: move prices in BTC to USDT; remove FTM

# return price for a pair at a specific timestamp
get_binance_price <- function(pair, datetime) {
  # Convert to POSIXct timestamp
  timestamp <- as.numeric(as.POSIXct(datetime, tz = "UTC")) * 1000
  
  # Fetch historical candle (Kline) data
  candles <- binance_klines(symbol = pair, interval = "1m", start_time = timestamp, limit = 1)
  
  if (nrow(candles) > 0) {
    return(as.numeric(candles$close))
  } else {
    return(NA)  # Return NA if no data found
  }
}

# Example usage
price <- get_binance_price("BTCUSDT", "2025-02-27 12:00:00")

# we need BTCUSDT price for these dates
dates <- c('2025-01-19 20:36:24', '2025-01-20 02:04:42', '2025-01-20 02:32:15', '2025-01-30 07:00:43',
           '2025-02-11 18:58:38')
# return BTC prices for specific dates
btc_prices <- sapply(dates, function(dt) get_binance_price("BTCUSDT", dt))

# BTCUSDT prices for list of instruments
prices <- c(99623.99, 104619.91, 106170.25, 100821.97, 101940.72, 105250.71, 95469.28)
instruments <- c('AAVEBTC', 'AAVEBTC', 'AAVEBTC', 'AAVEBTC', 'AAVEBTC', 'SUIBTC', 'SUIBTC' )
# Define instruments and prices as a tibble
price_tibble <- tibble(instrument = instruments, price_match = prices)



# # Merge with the original tibble
# tb <- my_journal_tb %>% left_join(price_tibble, by = "instrument")
# 
# price_ <- tibble(instrument = instruments, price_match = prices)

## write the new journal with all prices in USDT ##
my_journal_tb <- as_tibble(my_journal)

btc_price <- as_tibble(c(rep(1, 20), 99623.99, rep(1, 3), 104619.91, 106170.25, 100821.97, 
               101940.72, rep(1,17), 105250.71, rep(1,5), 95469.28, 1, 1))
# join the two tibbles
joined_tibble <- bind_cols(my_journal_tb, btc_price)
# price in USDT 
final_tb <- joined_tibble %>%
  mutate(mul = price * value) %>%
  select(-price, -value)

my_journal_usd <- final_tb %>%
  rename(price = mul)

# change instrument name in first column
my_journal_usd[[1]] <- ifelse(my_journal_usd[[1]] == "AAVEBTC", "AAVEUSDT", my_journal_usd[[1]])
my_journal_usd[[1]] <- ifelse(my_journal_usd[[1]] == "SUIBTC", "SUIUSDT", my_journal_usd[[1]])
my_journal_usd[54, 1] <- 'BTCUSDT'
my_journal_usd[39, 1] <- 'AAVEUSDT'
# remove FTM
my_journal_usd <- my_journal_usd %>% slice(-14)
# change BTCUSDC to BTCUSDT on row 42
my_journal_usd[42, 1] <- 'BTCUSDT'

