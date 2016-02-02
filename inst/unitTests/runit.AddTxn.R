# Author: Peter Carl, RUnit port by Ben McCann

# Functions ------------------------------------------------------------------ -

rm_portfolios <- function(x)
{
    #' @author cloudcello
    if(inherits(x, what="character")) {
        rm(list=paste0("portfolio.",x),pos=.blotter)
    } else {
        stop("portfolio name(s) must be supplied")
    }
}

rm_accounts <- function(x)
{
    #' @author cloudcello
    if(inherits(x, what="character")) {
        rm(list=paste0("account.",x),pos=.blotter)
    } else {
        stop("account name(s) must be supplied")
    }
}

ls_portfolios <- function()
{
    #' @author cloudcello
    ls(pattern=glob2rx("portfolio.*"),pos=.blotter)
}

ls_accounts <- function()
{
    #' @author cloudcello
    ls(pattern=glob2rx("account.*"),pos=.blotter)
}
# ---------------------------------------------------------------------------- -


test.addTxn <- function() {

  on.exit({
    # remove objects created by unit tests
    try(rm_currencies("USD"))
    try(rm_stocks(symbols))
    # try(rm(list=p, pos=.blotter))
    try(rm(list="pName"))
    ls(pos=.blotter)
    # try(rm(list="portfolio.runitAddTxn",pos=.blotter)) # TODO: write rm_portfolio(x)
    ls_portfolios()
    ls_accounts()
    try(rm_portfolios("runitAddTxn"))
    try(rm("IBM", pos = .GlobalEnv))
    # getPortfolio("runitAddTxn")
  })

  currency("USD")
  symbols <- c("IBM")
  for (symbol in symbols){
    stock(symbol, currency="USD", multiplier=1)
  }
  data("IBM", package="blotter")

  # Initialize a portfolio object 'p'
  # Creating portfolio:
  pName <- initPortf("runitAddTxn", symbols=symbols)

  # Trades must be made in date order.
  # Make a couple of trades in IBM
  addTxn(pName, "IBM", '2007-01-03',  50,  96.5,  TxnFees=-0.05 * 50)
  addTxn(pName, "IBM", '2007-01-04', -50,  97.1,  TxnFees=-0.05 * 50)
  addTxn(pName, "IBM", '2007-01-08', -10,  99.2,  TxnFees=-0.05 * 10)
  addTxn(pName, "IBM", '2007-01-09', -10, 100.1,  TxnFees=-0.05 * 10)
  addTxn(pName, "IBM", '2007-01-17', -10, 100.25, TxnFees=-0.05 * 10)
  addTxn(pName, "IBM", '2007-01-19',  30,  95,    TxnFees=-0.05 * 30)
  addTxn(pName, "IBM", '2007-01-22',  25,  96.3,  TxnFees=-0.05 * 25)
  addTxn(pName, "IBM", '2007-01-23',  25,  96.42, TxnFees=-0.05 * 25)
  addTxn(pName, "IBM", '2007-01-26', -25,  97.52, TxnFees=-0.05 * 25)
  addTxn(pName, "IBM", '2007-01-31', -25,  98.80, TxnFees=-0.05 * 25)

  portfolio <- getPortfolio(pName)
  transactions <- portfolio$symbols[["IBM"]]$txn
  checkEquals(-13, sum(transactions$Txn.Fees))
  checkEquals(0, sum(transactions$Txn.Qty))

  # TODO: fix bug in calcPortfSummary
  # summary <- calcPortfSummary(portfolio)
}

#----------------------------------------------------------------------------- -