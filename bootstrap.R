
# Reproductibilité
install.packages(c("renv","targets","tarchetypes","tidyverse","lubridate","tzdb","PMwR","binancer","arrow", "targets"))
renv::init()  # fige les deps

# Arborescence standard
dir.create("R", showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)

# load packages
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


# few settings
# prevent scientific notation for numbers 8.8214000e+02
options(scipen = 999, digits = 8)
start_date <- as.POSIXct("2024-12-16", tz = 'UTC')
mid_date <- as.POSIXct("2025-03-04", tz = 'UTC') # date of new account


# load the initial portfolio state
# # we replaced FTM by S (Sonic) as FTM changed to S 
trade_tb_start <- tibble(
  time = as.POSIXct(c(
    '2024-12-16 18:02:00',
    '2024-12-16 18:04:00',
    '2024-12-16 18:06:00',
    '2024-12-16 18:08:00',
    '2024-12-16 18:10:00',
    '2024-12-16 18:12:00'
  ), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  symbol = c('SUSDC', 'SUIUSDC', 'ETHUSDC', 'ENAUSDC', 'BTCUSDC', 'AAVEUSDC'),
  executed_qty = c(284270, 80024, 91.8981, 207836, 9.3372107, 1152.058),
  price = c(1.382, 4.7256, 4038, 1.1855, 107012, 386.55),
  status = rep('FILLED', 6),
  side = rep('BUY', 6)
) |>
  mutate(cummulative_quote_qty = executed_qty * price)

