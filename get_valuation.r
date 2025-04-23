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

## portfolio token as a tibble ##
# replace BTC & USDT with USDC at the end of the token names
token <- read.table('assets.csv')
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

# 1- journal
J <- journal(
  timestamp = as.POSIXct(all_trade_final$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  instrument = all_trade_final$symbol,
  amount = all_trade_final$executed_qty,
  price = all_trade_final$price
)

utils::View(J) # we need to run View to see J with RStudio. A probable bug
# prevents to see J when 

my_sequence <- as.Date(20073:20192) # dates 2024-12-16 --> 2025-04-14, 115 days for 116 dates
t.valuation <- as.POSIXct(paste(my_sequence, "23:59:59"), tz = "UTC")

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
P <- pricetable(BTCUSDC, SUIUSDC, ENAUSDC, AAVEUSDC, ETHUSDC, SUSDC, OMUSDC,
                instrument = c('BTCUSDC', 'SUIUSDC', 'ENAUSDC', 'AAVEUSDC', 'ETHUSDC', 
                                'SUSDC', 'OMUSDC'))

P <- P[my_sequence, c('AAVEUSDC', 'BTCUSDC', 'ENAUSDC', 'ETHUSDC', 
                       'OMUSDC', 'SUIUSDC', 'SUSDC'), missing = "previous"]  

PL <- pl(J, along.timestamp = t.valuation, vprice = P)




PL <- pl(J, along.timestamp = my_sequence, vprice = P)
PL <- pl(J, along.timestamp = timestamp, vprice = P)


VALO <- valuation(position(J, when = t.valuation), vprice = P, use.names = TRUE)

########### small exemple for testing purpose ############################
# token : 'SUIUSDC', 'ENAUSDC'  ---> good sur toute la periode
# 'BTCUSDC', 'AAVEUSDC'  ---> good
# 'ETHUSDC', 'LINKUSDC'
# 'SUSDC', 'OMUSDC' ---> good sur toute la periode
# period 12-16 --> 02-15

all_trade_final_short <- all_trade_final %>%
  filter(symbol %in%  c('ETHUSDC', 'SUSDC', 'SUIUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC', 'OMUSDC')) %>%
  select(-cummulative_quote_qty)

J <- journal(
  timestamp = as.POSIXct(all_trade_final_short$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  instrument = all_trade_final_short$symbol,
  amount = all_trade_final_short$executed_qty,
  price = all_trade_final_short$price
)
 

my_sequence <- as.Date(20073:20192)
t.valuation <- as.POSIXct(paste(my_sequence, "23:59:59"), tz = "UTC")

token_daily_close_final_short <- token_daily_close_final %>%
  select(close_time, SUSDC, ETHUSDC, SUIUSDC, ENAUSDC, BTCUSDC, AAVEUSDC, OMUSDC) 


token_zoo_list_short <- sapply(c('ETHUSDC', 'SUSDC', 'SUIUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC', 'OMUSDC') , get_token_zoo, simplify = FALSE)
tokens <- c('ETHUSDC', 'SUSDC', 'SUIUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC', 'OMUSDC')
names(token_zoo_list_short) <- tokens
list2env(token_zoo_list_short, envir = .GlobalEnv)

get_token_zoo <- function(token_name) {
  zoo(token_daily_close_final_short[[token_name]], order.by = my_sequence)
}

P <- pricetable(SUSDC,ETHUSDC,SUIUSDC,ENAUSDC,BTCUSDC,AAVEUSDC,OMUSDC,
                instrument = c('ETHUSDC', 'SUSDC', 'SUIUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC', 'OMUSDC'))

P <- P[my_sequence, c('ETHUSDC', 'SUSDC', 'SUIUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC', 'OMUSDC'), missing = "previous"]  

PL <- pl(J, along.timestamp = t.valuation, vprice = P)
