# this script write a data table with daily valuations of our portfolio compared with BTC
# performance
# the table will show our portfolio token with quantity, daily valuation, ptf daily return,
# base 100 valuation, BTC return to compare

# we have:
# all_trade final, a tibble with all our trades in USDC; 
# token_daily_close_final,  a tibble with daily close of token.

# our portfolio token, which shall be equal to token_usdc (a tibble)
table_token <- colnames(token_daily_close_final)[-1]
table_token_tb <- tibble(table_token)
# test if both are same
token_usdc <- token_usdc |>
      arrange(token_usdc)
table_token_tb <- table_token_tb |>
     arrange(table_token_tb)
# test if equal
if (!all(table_token_tb == token_usdc)) {
  stop("Erreur : table_token_tb est différent de token_usdc.")
}

# create empty table with token name as column names
empty_position<- tibble::tibble(!!!setNames(rep(list(logical()), nrow(token_usdc)), token_usdc$V1))
empty_position <- empty_position |>
  rename_with(~ paste0(.x, "_position"))
# same as above
empty_price <- tibble::tibble(!!!setNames(rep(list(logical()), nrow(token_usdc)), token_usdc$V1))
empty_price <- empty_price |>
  rename_with(~ paste0(.x, "_closing_price"))


# bind the two columns time of the two tibbles to get all dates, trades and
# closing prices
# add a source column
trade_df <- all_trade_final |>
  mutate(source = "trade") |>
  rename(value = time) 
close_df <- token_daily_close |>
  mutate(source = "close") |>
  rename(value = close_time)
# change TZ for trade_df if we want to keep 00:59:59 for close_df 
trade_df <- trade_df |>
  mutate(value = as.POSIXct(value, tz = "Europe/Paris"))

# add new columns with "TOKEN_position"
trade_position <- bind_cols(trade_df, empty_position[rep(1, nrow(trade_df)), ])

#############################################
##### compute position cumul with time ######

library(dplyr)
library(stringr)
library(tidyr)

# Cr�e une colonne d'ordre
df <- trade_position |>
  mutate(row_id = row_number())

# On isole les colonnes _position
position_cols <- names(df)[str_ends(names(df), "_position")]

# Calcul des positions cumul�es par token
cumulative_positions <- df |>
  select(row_id, symbol, executed_qty) |>
  filter(!is.na(symbol)) |>
  group_by(symbol) |>
  mutate(cum_qty = cumsum(executed_qty)) |>
  ungroup() |>
  mutate(position_col = paste0(symbol, "_position")) |>
  select(row_id, position_col, cum_qty) |>
  pivot_wider(names_from = position_col, values_from = cum_qty)

# Nettoyage et jointure
df <- df |>
  select(-all_of(position_cols)) |>
  left_join(cumulative_positions, by = "row_id") |>
  select(-row_id)

# remove lines with source == close
trade_position_cumul <- df |>
  filter(source == "trade")

# remove some columns
trade_position_cumul <- trade_position_cumul |>
  select(-c('token', 'close', 'row_id'))

