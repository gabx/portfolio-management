# portfolio-management

MLC portfolio management with R

This is en ensemble of scripts to build a trading journal, based on trades and open positions. 
The final result will show daily PL and valuation splited by token and for each day.
We will then build our daily valuation and some compute returns, contributions.
Finally, everything shall be presented witha nice formatting html or Markdown files.

Aside classic packages to work with data frame (dplyr, tydiverse, purr), our work will be based on 
[PMwR](https://cran.r-project.org/web/packages/PMwR/) and [binancer](https://github.com/daroczig/binancer)
packages. 

The portfolio is composed from BTC and some altcoins for [Mon Livret C](https://www.monlivretc.com/)

Each script has been assigned a specific task:

## 0- Main script
main_script.r

## 1- Environment variables
environment_variable.r 

- libraries to load

- environment variables

- few settings


## 2- get trades
[get_all_trade.r](#get-all-trade)

## 3- get daily closing prices for our portfolio tokens
get_all_trades.r

get_daily_closes.r

## 4- build the journal and the daily valuation
get_valuation.r

## 5- update the data with time
get_valuation_update.r

## 6- Valuations vizualisations
valuation_vizualisation.r




