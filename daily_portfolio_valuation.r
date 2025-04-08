## Final script to build our trading journal

# we have:
# all_trade final, a tibble with all our trades in USDC; 
# token_daily_close,  a tibble with daily close of token.


# we must join both and compute daily valuation, returns, drawdown etc
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# rename collumn to BTCUSDC_close etc
# token_daily_close <- token_daily_close %>%
#   rename_with(~ paste0(.x, "_close"), .cols = -time)

# add rows with TOKEN_position
token_usdc <- token %>%
       mutate(V1 = sub("BTC$|USDT$", "USDC", V1))
token_usdc <- token_usdc %>%
       distinct()
token_usdc <- unnest(token_usdc, cols = V1)
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
  rename(value = time)
# binds the two tibbles
time_all <- bind_rows(trade_df, close_df) %>%
  arrange(value)
# remove day column
time_all <- time_all %>%
  select(-day)
# add USDC column
time_all <- time_all %>%
  mutate(USDC = 1)
# add new columns with "TOKEN_position"
trade_position <- bind_cols(time_all, empty_position[rep(1, nrow(time_all)), ])



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
# first solution
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

