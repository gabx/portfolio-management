# portfolio-management
MLC portfolio management with R

This project is based on the [PMwR](https://cran.r-project.org/web/packages/PMwR/),
Portfolio Management with R, package. The [manual](https://enricoschumann.net/R/packages/PMwR/manual)
contains everything we need to manage our portfolio.

The portfolio is made of BTC, altcoins and USDT. All positions are cash, long
only. The trades are executed on Binance, thus the use of the [binancer](https://github.com/daroczig/binancer)
package to retrieve trades, daily close values and compute P/L.

