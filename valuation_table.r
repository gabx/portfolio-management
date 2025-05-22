## Final script to build our trading journal

# we have:
# all_trade final, a tibble with all our trades in USDC; 
# token_daily_close,  a tibble with daily close of token.

# we want:
# a table with a line per day with open positions, daily valuation, return, benchmark (BTC), ptf 
# valuation in base 100


library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(PMwR)

# prevent scientific notation for numbers 8.8214000e+02
options(scipen = 999, digits = 8)

# rename collumn to BTCUSDC_close etc
# token_daily_close <- token_daily_close %>%
#   rename_with(~ paste0(.x, "_close"), .cols = -time)

# add rows with TOKEN_position

# get our token list
token <- read.table('assets.csv')
token_usdc <- token %>%
       mutate(V1 = sub("BTC$|USDT$", "USDC", V1))
token_usdc <- token_usdc %>%
       distinct()
token_usdc <- unnest(token_usdc, cols = V1)

# ????
empty_position<- tibble::tibble(!!!setNames(rep(list(logical()), nrow(token_usdc)), token_usdc$V1))
empty_position <- empty_position %>%
  rename_with(~ paste0(.x, "_position"))

# bind the two columns time of the two tibbles to get all dates, trades and
# closing prices
# add a source column
trade_df <- all_trade_final %>%
  mutate(source = "trade") %>%
  rename(value = time) 
close_df <- token_daily_close %>%
  mutate(source = "close") %>%
  rename(value = close_time)
# binds the two tibbles
time_all <- bind_rows(trade_df, close_df) %>%
  arrange(value)
# remove day column
# time_all <- time_all %>%
#   select(-day)
# add USDC column
# time_all <- time_all %>%
#   mutate(USDC = 1)
# add new columns with "TOKEN_position"
trade_position <- bind_cols(time_all, empty_position[rep(1, nrow(time_all)), ])
# add a column USDC_position
trade_position <- trade_position %>%
  mutate(USDC_position = 0)
# add a column with portfolio_value
trade_position <- trade_position %>%
  mutate(USDC_position = 0)
# # remove sum_cummulative_qty column
# trade_position <- trade_position %>%
#   select(-sum_cummulative_quote_qty)

###############################################################################
# TEST #
# btc_op are trades on BTCUSDC (source == trade)
btc_trade <- trade_position %>%
  filter(symbol == "BTCUSDC", source == 'trade')
btc_trade <- btc_trade[, 1:4]
btc_trade_jl <- journal(
  timestamp = btc_trade$value,
  amount = btc_trade$executed_qty,
  price = btc_trade$price,
  instrument = btc_trade$symbol,
)
# there is an issue with view and journal
utils::View(btc_trade_jl)

J <- read.table(text = "
    instrument,timestamp,amount,price
    A,2025-04-08 15:00:00,  10, 10
    A,2025-04-08 16:00:00,  -5, 20
    B,2025-04-08 22:00:00, 100, 5",
    sep = ",", header = TRUE, stringsAsFactors = FALSE)

J$timestamp <- as.POSIXct(J$timestamp, tz = 'UTC')
J <- as.journal(J)  

t.valuation <-
  as.POSIXct(paste(as.Date("2025-04-08") + 0:1, "17:59:59"), tz = 'UTC')

price <- read.table(text = "
    A,B
    15, 5
    15, 10",
    sep = ",", header = TRUE, stringsAsFactors = FALSE)

price <- read.table(text = "
instrument,timestamp,price
A,2025-04-08 17:59:59,15
B,2025-04-08 17:59:59,5
A,2025-04-09 17:59:59,15
B,2025-04-09 17:59:59,10",
sep = ",", header = TRUE, stringsAsFactors = FALSE)

price$timestamp <- as.POSIXct(price$timestamp, tz = 'UTC')


pl <- pl(J, along.timestamp = t.valuation, vprice = price)

btc_df <- journal(
  timestamp = as.Date(btc_op$timestamp),
  amount = btc_op$amount,
  price = btc_op$price,
  instrument = btc_op$instrument,
)

btc_journal <- as.journal(btc_df)

btc_df <- data.frame(
  instrument = btc_op$instrument,
  timestamp = as.POSIXct(btc_op$timestamp),
  amount = btc_op$amount,
  price = btc_op$price,
  stringsAsFactors = FALSE
)

btc_journal <- as.journal(
  instrument = as.character(btc_op$instrument),
  timestamp = as.POSIXct(btc_op$timestamp),
  amount = as.numeric(btc_op$amount),
  price = as.numeric(btc_op$price)
)

###########################################

library(PMwR)

btc_journal <- as.journal(
  data.frame(
    instrument = as.character(btc_op$instrument),
    timestamp = as.POSIXct(btc_op$timestamp),
    amount = as.numeric(btc_op$amount),
    price = as.numeric(btc_op$price),
    stringsAsFactors = FALSE
  )
)

####################################################


# keep everything regarding BTCUSDC
# 1- every day line for closing price
daily_position <- trade_position %>%
  filter(source == "close") %>%
  select(1:which(names(.) == "BTCUSDC_price"))
# 2- trade for BTCUSDT
btc_trades <- trade_position %>%
  filter(symbol == 'BTCUSDC') %>%
  select(1:which(names(.) == "cummulative_quote_qty")) # remove all col from cummulative_quote_qty
# 3 join
btc_op <- 


# # remplir les cases avec cumulative quantity
# for (i in seq_len(nrow(trade_position))) {
#   symbol <- trade_position$symbol[[i]]
#   if (!is.na(symbol) && trade_position$source[[i]] == "trade") {
#     col_name <- paste0(symbol, "_position")
#     trade_position[[col_name]][[i]] <- trade_position$cummulative_quote_qty[[i]]
#   }
# }

# tibble avec la somme des trades pour une journée
cummulative_qty_by_day <- trade_position %>%
  filter(source == "trade") %>%               # On ne garde que les lignes de type "trade"
  mutate(day = as.Date(value)) %>%            # On extrait la date depuis la colonne datetime "value"
  group_by(day) %>%                           # On groupe par jour
  summarise(total_cummulative_quote_qty = sum(cummulative_quote_qty, na.rm = TRUE)) %>% # On additionne
  ungroup()

#############  short for mailing list ################
trading_journal <- trade_position %>%
  select(1, day, everything())
trading_journal <- trading_journal %>%
  select(1:16)
trading_journal <- trading_journal %>%
  select(1:12)
trading_journal <- head(trading_journal, 50)


# Étape 1 : calculer la somme des cummulative_quote_qty par jour
# �tape 1 : somme des cummulative_quote_qty par jour (uniquement source == "trade")
# Étape 1 : somme par jour
quote_sum_by_day <- trade_position %>%
  filter(source == "trade") %>%
  mutate(day = as.Date(value)) %>%
  group_by(day) %>%
  summarise(total_quote = sum(cummulative_quote_qty, na.rm = TRUE), .groups = "drop")

# Étape 2 : join + conversion + affichage une seule fois par jour
library(dplyr)

trade_position <- trade_position %>%
  group_by(day) %>%
  mutate(
    total_quote = sum(cummulative_quote_qty, na.rm = TRUE),
    cummulative_quote_by_day = if_else(row_number() == 1, total_quote, NA_real_)
  ) %>%
  ungroup() %>%
  select(-total_quote)



trade_position <- trade_position %>%
  select (-total_cummulative_quote_qty) %>%                    # 1. Supprimer la colonne incorrecte
  group_by(day) %>%
  mutate(
    sum_cummulative_quote_qty = if_else(
      row_number() == 1,
      sum(cummulative_quote_qty, na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

sum_cq_by_day <- trade_position %>%
  filter(source == "trade") %>%
  mutate(day = as.Date(value)) %>%
  group_by(day) %>%
  summarise(sum_cummulative_quote_qty = sum(cummulative_quote_qty, na.rm = TRUE)) %>%
  ungroup()

trade_position1 <- trade_position %>%
  mutate(day = as.Date(value)) %>%
  left_join(sum_cq_by_day, by = "day")

trade_position1 <- trade_position1 %>%
  select(-sum_cummulative_quote_qty.y) %>%
  rename(sum_cummulative_quote_qty = sum_cummulative_quote_qty.x)

valo_cols <- paste0(token_usdc$V1, "_valo")
empty_valos <- as_tibble(setNames(replicate(length(valo_cols), NA_real_, simplify = FALSE), valo_cols))
trade_position_full <- bind_cols(trade_position, empty_valos)

trade_position_full <- trade_position %>%
  mutate(across(.cols = everything(), .fns = list)) %>%  # Pour éviter conflits éventuels
  bind_cols(as_tibble(setNames(replicate(length(valo_cols), NA_real_, simplify = FALSE), valo_cols)))


trade_position1 <- trade_position1 %>%
  group_by(day) %>%
  mutate(
    sum_cummulative_quote_qty = if_else(row_number() == 1, sum_cummulative_quote_qty, NA_real_)
  ) %>%
  ungroup()
####################################################
# donner un numéro à la tibble des tokens token_usdc 
token_usdc <- token_usdc %>%
  mutate(id = row_number())
# nos numéros:
token_usdc$id

# for printing convenience
for_printing <- trade_position_full %>%
     select(-c(7:15))
   
# somme des transactions par token et par jour      
daily_trades <- for_printing %>%
  filter(source == "trade") %>%
  group_by(day, symbol) %>%
  summarise(qty = sum(executed_qty, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = symbol, values_from = qty, values_fill = 0)
# compléter les dates manquantes et faire un cumul jour par jour
full_days <- for_printing %>%
  filter(source == "close") %>%
  select(day) %>%
  distinct() %>%
  arrange(day)

# On pr�pare un tibble avec tous les jours et tokens, m�me s\u2019il n\u2019y a pas eu de trades
token_symbols <- token_usdc$V1

cumulative_trades <- full_days %>%
  left_join(daily_trades, by = "day") %>%
  mutate(across(all_of(token_symbols), ~replace_na(., 0))) %>%
  arrange(day) %>%
  mutate(across(all_of(token_symbols), cumsum, .names = "{.col}_position"))


################################

# �tape 1 \u2014 Somme par jour et par token
daily_trades <- for_printing %>%
  filter(source == "trade") %>%
  group_by(day, symbol) %>%
  summarise(qty = sum(executed_qty, na.rm = TRUE), .groups = "drop")

# �tape 2 \u2014 Forcer toutes les combinaisons jour x token
all_days <- for_printing %>%
  filter(source == "close") %>%
  distinct(day)

token_symbols <- token_usdc$V1

daily_trades_full <- all_days %>%
  crossing(symbol = token_symbols) %>%
  left_join(daily_trades, by = c("day", "symbol")) %>%
  mutate(qty = replace_na(qty, 0)) %>%
  arrange(symbol, day) %>%
  group_by(symbol) %>%
  mutate(position = cumsum(qty)) %>%
  ungroup()

# �tape 3 \u2014 Injecter les positions dans les lignes "close"
for_printing_updated <- for_printing %>%
  left_join(
    daily_trades_full,
    by = c("day", "symbol")
  ) %>%
  mutate(position = if_else(source == "close", position, NA_real_))


# df_joined <- trade_position %>%
#   mutate(day = as.Date(value)) %>%
#   left_join(executed_qty_by_day, by = "day")

df_with_total <- trade_position %>%
  mutate(day = as.Date(value)) %>%
  left_join(cummulative_qty_by_day, by = "day") %>%
  filter(!duplicated(day))


# reshape la table en long format
# value    symbol   price   date
trade_position_long <- trade_position %>%
  select(value, ends_with("_price")) %>%
  pivot_longer(
    cols = -value,
    names_to = "symbol",
    values_to = "price"
  ) %>%
  mutate(
    # Enlève le suffixe "_price" pour garder uniquement le nom du token
    symbol = gsub("_price$", "", symbol),
    date = as.Date(value)
  )

dates_to_extract <- as.Date(c("2024-12-16", "2024-12-17"))
tokens_to_extract <- c("BTCUSDC", "ETHUSDC")
filtered_prices <- trade_position_long %>%
  filter(symbol %in% tokens_to_extract, date %in% dates_to_extract)




#############################################################
# second solution
# mailing list solution

library(xts)
library(zoo)

# (Re)d�finir la fonction proprement
trades_long_to_wide <- function(trades) {
  wide <- as.xts(read.zoo(trades, split = "symbol", tz = ""))
  
  # Nettoyer les noms de colonnes : price.symbol
  colnames(wide) <- gsub("^(price)\\.(.*)", "\\2", colnames(wide))
  
  # Colonnes de qty
  qty_cols <- grep("executed_qty", colnames(wide))
  wide$executed_qty <- 0
  
  for (j in qty_cols) {
    jcol <- wide[, j]
    jcol <- ifelse(is.na(jcol), 0, jcol)
    wide[, "executed_qty"] <- jcol + wide[, "executed_qty"]
  }
  
  wide <- wide[, -qty_cols]  # supprime les colonnes qty individuelles
  return(wide)
}

# S�lectionne les colonnes utiles
trades <- all_trade_final[, c("time", "symbol", "price", "executed_qty")]

# Applique la fonction
wide_trades_xts <- trades_long_to_wide(trades)

# Convertit proprement token_daily_close
token_xts <- as.xts(token_daily_close[, -1], order.by = token_daily_close$time)

# Merge
result_xts <- merge(wide_trades_xts, token_xts)

########################################################

# the tibble "time_all" will be the base to our portfolio calculations
# we create functions and then apply them to our values in the tibble.





############################




# �tape 1 : Transformer token_daily_close en long format

closing_long <- token_daily_close %>%
  pivot_longer(
    cols = ends_with("_close"),
    names_to = "symbol",
    values_to = "price_close"
  ) %>%
  mutate(
    symbol = gsub("_close$", "", symbol)
  )



# etape 2 : créer df_long: les prix de cloture en format long
df_long <- token_daily_close %>%
  pivot_longer(
    cols = ends_with("_close"),
    names_to = "symbol",
    values_to = "price_close"
  ) %>%
  mutate(
    symbol = gsub("_close", "", symbol)
  )

# etape 3 : preparer les données de trade
trades_ready <- all_trade_final %>%
  select(time, symbol, executed_qty, price) %>%
  mutate(price_close = NA_real_) %>%
  select(time, symbol, price_close, executed_qty, price)

portfolio_journal <- bind_rows(df_long, trades_ready) %>%
  arrange(time)

portfolio_journal <- portfolio_journal %>%
  mutate(position_qty = executed_qty * price) %>%
  select(-amount)
mutate(valuation = position_qty * price)

###################################################
# return dats (day + time) of trades + daily close
# closedate <- as_tibble(token_daily_close_short$time)
# tradedate <- as_tibble(all_trade_final_short$time)
# tradedate <- tradedate %>%
#    rename(time = value)
# closedate <- closedate %>%
#   rename(time = value)
# alldate <- bind_rows(closedate, tradedate) %>%
#   #distinct(time) %>%
#   arrange(time)

############### autre methode #####################

all_trade_final_wide <- all_trade_final %>%
  select(time, symbol, executed_qty) %>%
  pivot_wider(names_from = symbol, values_from = executed_qty)

all_trade_final_wide <- all_trade_final %>%
  select(time, symbol, price) %>%
  pivot_wider(names_from = symbol, values_from = price) # we use price 

all_trade_final_wide <- all_trade_final %>%
  select(time, symbol, cummulative_quote_qty) %>%
  pivot_wider(names_from = symbol, values_from = cummulative_quote_qty)




# time_all <- bind_rows(time_trade, time_close) %>%
#   distinct() %>%               # pour éviter les doublons si nécessaire
#   arrange(value) 
# 
# 
#  time_trade <- time_trade %>%
#        mutate(source = "trade")
# time_close <- time_close %>%
#      mutate(source = "close")
# time_all <- bind_rows(time_trade, time_close) %>%
#        arrange(value)



trade_complete <- all_trade_final |>
          left_join(all_trade_final__wide)

trade_complete2 <- all_trade_final |>
  left_join(token_daily_close)

trade_complete2 <- all_trade_final %>%
  left_join(
    token_daily_close %>%
      mutate(day = as.Date(time)),
    by = "day"
  )

