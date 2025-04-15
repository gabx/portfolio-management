# this is a short of our real final journal
# will keep trades from 2024-12-17 to 2025-01-06
# https://enricoschumann.net/notes/computing-portfolio-pl.html
# https://enricoschumann.net/notes/valuing-positions.html

library(zoo)
library(PMwR)
library(dplyr)
library(tidyr)
library(purrr)

## HINT : manage dates ##
# as.numeric(as.POSIXct("2025-01-29 10:25:09", tz = "UTC"))
# as.numeric(as.Date("2024-12-16")) ---> 20073
# as.POSIXct(1738142709.422, origin = "1970-01-01", tz = "UTC")


all_trade_short <- all_trade_final %>%
  slice(7:18)
# BTC, ETH, AAVE, ENA, FTM, SUI
token_daily_close_short <- token_daily_close_short %>%
  select(close_time, BTCUSDC, ETHUSDC, AAVEUSDC, ENAUSDC, FTMUSDC, SUIUSDC)

# 1.1 journal component
instrument <- all_trade_short$symbol
journal_timestamp <- all_trade_short$time
amount <- all_trade_short$executed_qty
price <- all_trade_short$price

# 1.2 build journal
j <- journal(
  amount = all_trade_short$executed_qty,
  instrument = all_trade_short$symbol,
  price = all_trade_short$price,
  #timestamp = all_trade_short$time
  timestamp = as.POSIXct(all_trade_short$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
)

# 2. our valuation day
#timestamp <- seq(from = as.Date('2024-12-17'),
#                  to = as.Date('2025-01-06'),
#                   by = '1 day')
timestamp <- seq(from = as.POSIXct("2024-12-16", tz = 'UTC'),
                 to   = as.POSIXct("2025-01-06", tz = 'UTC'),
                 by   = "1 day") 

# 3. closing prices of token @ 23:59:59
# we will work with token_daily_close_final
# token : BTC, ETH, AAVE, ENA, FTM, SUI

my_sequence <- 20074:20094 # dates
as.Date(20074:20094)


J <- journal(instrument = c("BTCUSDC",  'ETHUSDC', 'AAVEUSDC',
                              'ENAUSDC', "FTMUSDC", 'SUIUSDC'),
      timestamp = as.POSIXct(c("2024-12-17 09:01:30 UTC", "2024-12-18 20:32:53 UTC", "2025-01-02 09:33:18 UTC",
      "2025-01-02 09:34:49 UTC", "2025-01-02 10:14:43 UTC", '2025-01-02 10:15:12 UTC', "2025-01-02 10:15:22 UTC",
      "2025-01-02 10:15:34 UTC", "2025-01-02 10:15:45 UTC", "2025-01-02 10:15:55 UTC",
      "2025-01-06 15:32:43 UTC", "2025-01-06 15:34:09 UTC"),format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      amount = all_trade_short$executed_qty,
      price = all_trade_short$price)
 
# dates de valorisation
t.valuation <- structure(c(1734479999, 1734566399, 1734652799, 1734739199, 1734825599,
                           1734911999, 1734998399,1735084799, 1735171199, 1735257599,
                           1735343999, 1735430399, 1735516799, 1735603199, 1735689599,
                           1735775999, 1735862399, 1735948799, 1736035199, 1736121599,
                           1736207999),
                         class = c("POSIXct", "POSIXt"), tzone = "")

# prix de valorisation        
BTCUSDC <- structure(token_daily_close_short$BTCUSDC,
                     .Dim = c(21L, 1L),
                     .Dimnames = list(NULL, 'BTCUSDC'),
                     index = structure(
                       my_sequence, class = "Date"),
                     class = "zoo")   

ETHUSDC <- structure(token_daily_close_short$ETHUSDC,
                     .Dim = c(21L, 1L),
                     .Dimnames = list(NULL, 'ETHUSDC'),
                     index = structure(
                       my_sequence, class = "Date"),
                     class = "zoo")   

AAVEUSDC <- structure(token_daily_close_short$AAVEUSDC,
                      .Dim = c(21L, 1L),
                      .Dimnames = list(NULL, 'AAVEUSDC'),
                      index = structure(
                        my_sequence, class = "Date"),
                      class = "zoo")   

ENAUSDC <- structure(token_daily_close_short$ENAUSDC,
                     .Dim = c(21L, 1L),
                     .Dimnames = list(NULL, 'ENAUSDC'),
                     index = structure(
                       my_sequence, class = "Date"),
                     class = "zoo")   

FTMUSDC <- structure(token_daily_close_short$FTMUSDC,
                     .Dim = c(21L, 1L),
                     .Dimnames = list(NULL, 'FTMUSDC'),
                     index = structure(
                       my_sequence, class = "Date"),
                     class = "zoo")   

SUIUSDC <- structure(token_daily_close_short$SUIUSDC,
                     .Dim = c(21L, 1L),
                     .Dimnames = list(NULL, 'SUIUSDC'),
                     index = structure(
                       my_sequence, class = "Date"),
                     class = "zoo")   


P <- pricetable(BTCUSDC, ETHUSDC, AAVEUSDC, ENAUSDC, FTMUSDC, SUIUSDC,
                instrument = c("BTCUSDC",  'ETHUSDC', 'AAVEUSDC',
                               'ENAUSDC', "FTMUSDC", 'SUIUSDC'))
P <- P[t.valuation, c("BTCUSDC",  'ETHUSDC', 'AAVEUSDC',
                    'ENAUSDC', "FTMUSDC", 'SUIUSDC'), missing = "previous"]  

PL <- pl(J, along.timestamp = t.valuation, vprice = P)
VALO <- valuation(position(J, when = t.valuation), vprice = P, use.names = TRUE)

# we now have the cumulative P/L of the assets for every day, stored in a matrix PL. 
PL <- sapply(PL, `[[`, "pl")
tail(PL, 5)

# monthly PL
library("datetimeutils")
ii <- c(1, nth_day(timestamp.P,  ## extract position of last day of month
                   period = "month", n = "last",
                   index = TRUE))
diff(PL[ii, ])


