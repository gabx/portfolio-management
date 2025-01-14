# portfolio-management

MLC portfolio management with R

### Packages

* [PMwR](https://cran.r-project.org/web/packages/PMwR/): Portfolio Manager with R  
* [binancer](https://github.com/daroczig/binancer): get all trades from Binance exchange
* [orgutils](https://cran.r-project.org/web/packages/orgutils/index.html): highly flexible 
structured plain text file format

### Manuals

* [PMwR manual](https://enricoschumann.net/R/packages/PMwR/manual)

### The journal
This is a list of vectors, very similar to a `data.frame` created through the function `journal`. Components of a 
journal should always be retrieved by name.

Fields are:

* amount
* timestamp
* price
* instrument

All fields except amount can be missing.

The function `journal()` creates `journal` objects.

The function `position()` gives the current balance of all instruments. By default, 
the function will aggregate all transactions of the journal, no matter when they occurred.

In general, a journal need not be sorted in any particular way. There is a `sort` 
method for journals, whose default is to sort by timestamp. 

