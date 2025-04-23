# This script writes the method to print our journal and everything related in
# nice formats, like html or markdown.

# we start with PL, obtained from script get_valuation.r
# PL is a list of trades, one list for one token.
# then we split the list into a data.frame

# list of token in a tibble:
token_usdc

# one of data frame for token SUSDC
PL_df_susdc <- data.table(
  token = rep("SUSDC", length(PL[["SUSDC"]]$timestamp)),
  timestamp = PL[['SUSDC']]$timestamp,
  pl = as.numeric(PL[["SUSDC"]]$pl),
  realised = PL[["SUSDC"]]$realised,
  unrealised = PL[["SUSDC"]]$unrealised,
  volume = PL[["SUSDC"]]$volume
)
# create function for any token
PL_df <- function(token_name) {
  data.table(
    token = rep(token_name, length(PL[[token_name]]$timestamp)),
    timestamp = PL[[token_name]]$timestamp,
    pl = as.numeric(PL[[token_name]]$pl),
    realised = PL[[token_name]]$realised,
    unrealised = PL[[token_name]]$unrealised,
    volume = PL[[token_name]]$volume
  )
}
