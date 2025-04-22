# this script will allow to build a valuation table with daily portfolio valuation,
# dates and assets.
# A first script, journal_work_short, has been writing to test the code on a smaller
# set of data.
# code is based on work of enrico schuman
# https://enricoschumann.net/notes/computing-portfolio-pl.html
# https://enricoschumann.net/notes/valuing-positions.html
################################################################################
## HINT : manage dates ##
# as.numeric(as.POSIXct("2025-01-29 10:25:09", tz = "UTC"))
# as.numeric(as.Date("2024-12-16")) ---> 20073
# as.POSIXct(1738142709.422, origin = "1970-01-01", tz = "UTC")
# as.Date(18387) --> "2020-05-05"
################################################################################
# our work will be based on token_daily_close_final (result of get_daily_close),
# all_trade_final (get_all_trade).
# This code is for 2024-12-16 to 2025-04-10 period

library(zoo)
library(PMwR) 
library(dplyr)
library(tidyr)
library(purrr)


# 1- journal
J <- journal(
  timestamp = as.POSIXct(all_trade_final$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  instrument = all_trade_final$symbol,
  amount = all_trade_final$executed_qty,
  price = all_trade_final$price
)

utils::View(J) # we need to run View to see J with RStudio. A probable bug
# prevents to see J when 

my_sequence <- as.Date(20073:20189) # dates 2024-12-16 --> 2025-04-10, 115 days for 116 dates
t.valuation <- as.POSIXct(paste(my_sequence, "23:59:59"), tz = "UTC")
# !! the table token_daily_close_final end on 04-14, sp we must remove 4 days to have same 
# number of days with my_sequence
token_daily_close_final <- token_daily_close_final %>%
  slice(1:(n() - 4))

# # 2- valuation prices 
# we first create zoo objects for each token
get_token_zoo <- function(token_name) {
  zoo(token_daily_close_final[[token_name]], order.by = my_sequence)
}
# apply the function to our token, then save in our env each zoo object
token_zoo_list <- sapply(token_usdc$V1, get_token_zoo, simplify = FALSE)
tokens <- pull(token_usdc, V1)
names(token_zoo_list) <- tokens
list2env(token_zoo_list, envir = .GlobalEnv)

# create matrix of prices
P <- pricetable(BTCUSDC, SUIUSDC, ENAUSDC, AAVEUSDC, ETHUSDC, LINKUSDC, SUSDC, OMUSDC, FTMUSDC,
                instrument = c('BTCUSDC', 'SUIUSDC', 'ENAUSDC', 'AAVEUSDC', 'ETHUSDC', 
                               'LINKUSDC', 'SUSDC', 'OMUSDC', 'FTMUSDC'))

P <- P[my_sequence, c('AAVEUSDC', 'BTCUSDC', 'ENAUSDC', 'ETHUSDC', 'FTMUSDC', 
                      'LINKUSDC', 'OMUSDC', 'SUIUSDC', 'SUSDC'), missing = "previous"]  


PL <- pl(J, along.timestamp = my_sequence, vprice = P)
PL <- pl(J, along.timestamp = t.valuation, vprice = P)

PL <- pl(J, along.timestamp = timestamp, vprice = P)


VALO <- valuation(position(J, when = t.valuation), vprice = P, use.names = TRUE)

JJ <- read.table(text = "
    instrument,timestamp,amount,price
    A,2025-04-08 15:00:00,  10, 10
    A,2025-04-08 16:00:00,  -5, 20
    B,2025-04-08 22:00:00, 100, 5
    ", sep = ",", header = TRUE)
J$timestamp <- as.POSIXct(J$timestamp)

########### small exemple #########################################################
# token : BTCUSDC, AAVEUSDC

all_trade_final_short <- all_trade_final %>%
  filter(symbol %in% c("BTCUSDC", "AAVEUSDC")) %>%
  filter(time <= as.POSIXct("2025-01-02", tz = "UTC")) %>%
  select(-cummulative_quote_qty)


dput(all_trade_final_short)
structure(list(time = structure(c(1734372600, 1734372720, 1734426090.135, 
  1734553973.032, 1735810398.277, 1735812883.798, 1735812912.427
), tzone = "UTC", class = c("POSIXct", "POSIXt")), symbol = c("BTCUSDC", 
    "AAVEUSDC", "BTCUSDC", "BTCUSDC", "BTCUSDC", "AAVEUSDC", "BTCUSDC"
), executed_qty = c(9.3372107, 1152.058, 0.04811, 0.12324, -0.06504, 
    94.408, 0.96557), price = c(107012, 386.55, 107358.81, 101422.93, 
96408.23, 332.340557, 96606.12)), row.names = c(NA, -7L), class = c("tbl_df", 
      "tbl", "data.frame"))

J <- journal(
  timestamp = as.POSIXct(all_trade_final_short$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  instrument = all_trade_final_short$symbol,
  amount = all_trade_final_short$executed_qty,
  price = all_trade_final_short$price
)
                                                                   
dput(J)
structure(list(instrument = c("BTCUSDC", "AAVEUSDC", "BTCUSDC", 
  "BTCUSDC", "BTCUSDC", "AAVEUSDC", "BTCUSDC"), timestamp = structure(c(1734372600, 
1734372720, 1734426090.135, 1734553973.032, 1735810398.277, 1735812883.798, 
1735812912.427), tzone = "UTC", class = c("POSIXct", "POSIXt"
)), amount = c(9.3372107, 1152.058, 0.04811, 0.12324, -0.06504, 
94.408, 0.96557), price = c(107012, 386.55, 107358.81, 101422.93, 
  96408.23, 332.340557, 96606.12)), class = "journal")

my_sequence <- as.Date(20073:20090)
t.valuation <- as.POSIXct(paste(my_sequence, "23:59:59"), tz = "UTC")

token_daily_close_final_short <- token_daily_close_final %>%
  select(close_time, AAVEUSDC, BTCUSDC) %>%
  filter(close_time <= as.POSIXct("2025-01-02", tz = "UTC"))

token_zoo_list_short <- sapply(c('BTCUSDC', 'AAVEUSDC'), get_token_zoo, simplify = FALSE)
tokens <- c('BTCUSDC', 'AAVEUSDC')
names(token_zoo_list_short) <- tokens
list2env(token_zoo_list_short, envir = .GlobalEnv)

get_token_zoo <- function(token_name) {
  zoo(token_daily_close_final_short[[token_name]], order.by = my_sequence)
}

P <- pricetable(BTCUSDC, AAVEUSDC,
                instrument = c('BTCUSDC', 'AAVEUSDC'))

P <- P[my_sequence, c('AAVEUSDC', 'BTCUSDC'), missing = "previous"]  

dput(P)
structure(c(382.68, 361.93, 343.39, 319.26, 328.11, 298.39, 319.08, 
            388.12, 374.46, 367.89, 337.9, 323.5, 356.13, 329.39, 323.61, 
            309.04, 320.14, 382.68, 105848.87, 105961.05, 101128.08, 98300.3, 
            97408.97, 97088.03, 95443.06, 95064.95, 98696.01, 98736, 95712.33, 
            94301.25, 95148, 92948.39, 92626.69, 93310.64, 94596.01, 105848.87
), dim = c(18L, 2L), dimnames = list(c("2024-12-16", "2024-12-17", 
                                       "2024-12-18", "2024-12-19", "2024-12-20", "2024-12-21", "2024-12-22", 
                                       "2024-12-23", "2024-12-24", "2024-12-25", "2024-12-26", "2024-12-27", 
                                       "2024-12-28", "2024-12-29", "2024-12-30", "2024-12-31", "2025-01-01", 
                                       "2025-01-02"), c("AAVEUSDC", "BTCUSDC")))

PL <- pl(J, along.timestamp = t.valuation, vprice = P)
