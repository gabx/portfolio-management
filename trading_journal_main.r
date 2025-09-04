
# code is based on work of enrico schuman
# https://enricoschumann.net/notes/computing-portfolio-pl.html
# https://enricoschumann.net/notes/valuing-positions.html
################################################################################

# chosse the correct working directory
setwd('/development/language/r/portfolio-management')

# load package, settings
# build the portfolio day 1 : trade_tb_start
source('bootstrap.r')

# create a list or a tibble with our token
# token_list  <- load_assets("assets.csv", output = "list")
# token_tib   <- load_assets("assets.csv", output = "tibble")
source('load_assets.r')

# fetch all trades from day 1, Sys.getenv("START_DATE")
source('get_all_trades.r')




# get the PL by token
source('get_pl.r')

# write a table with daily valuation
source('valuation_table')