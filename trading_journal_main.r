

# chosse the correct working directory
setwd('/development/language/r/portfolio-management')

# load variables, package, settings
# fetch trades as lists: trade_ls, trade_ls2
source('environment_variables.r')

# fetch all trades
# result: all_trade_final
source('get_all_trade.r')

# get daily closing prices @ 00:59:59
# result: token_daily_close_final
source('get_daily_closes.r')