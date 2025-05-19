# PART I

# This is all needed packages and variables to write our trading journal, plus a few
# settings

# load libraries
library(zoo)
library(PMwR) 
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library('binancer')
library('rjson')
library('lubridate')
library('PerformanceAnalytics')
library(stringr)
library(tibble)

# prevent scientific notation for numbers 8.8214000e+02
options(scipen = 999, digits = 8)


### portfolio token as a tibble ###
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


### Binance credentials, return trades ###
# first period
binance_credentials(Sys.getenv('BINANCE_KEY'), Sys.getenv('BINANCE_SECRET')) 
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
# return trades for assets for a specific period. A list of data frames
trade_ls <- mapply(binance_all_orders, start_time = start_date, symbol = token_list, SIMPLIFY = FALSE)

# second period
binance_credentials(Sys.getenv('BINANCE_KEY2'), Sys.getenv('BINANCE_SECRET2')) 
start_date2 <- as.POSIXct("2025-03-04", tz = 'UTC')
trade_ls2 <- mapply(binance_all_orders, start_time = start_date2, symbol = token_list, SIMPLIFY = FALSE)









