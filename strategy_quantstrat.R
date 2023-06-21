#____Install blotter and quantstrat______
# First it is necessary install RTools 4.3 from:
# https://cran.r-project.org/bin/windows/Rtools/
#require(devtools)
#install_github("braverock/blotter")
#library(remotes)
#remotes::install_github("braverock/blotter")
#library(devtools)
#install_local(path = "blotter-master.zip")
#devtools::install_github("braverock/quantstrat")

#___Packages___
library(devtools)
library(blotter)
library(quantstrat)
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(tidyverse) 
library(dplyr) 
library(TTR)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(devtools)
library(yahoofinancer)

#rm(list = ls(all.names = T))
.blotter <- new.env() #create the required environments
.strategy <- new.env() #create the required environments

# ___Inputs___
initDate <- "2000-01-02" #Date of initiation
from <- "2016-06-03"
to <- "2023-06-09"
orderqty <- 1 # units stocks   orderqty = tradeSize / ClosePrice
initEq <- 100 #Initial equity USD
tradesize <- 276 #USD

symbols <- "MSFT"
getSymbols(symbols, from = from, to = to, adjust = TRUE)
datos <- MSFT

#set the currency and the stock we are interested
currency("USD")
Sys.setenv(TZ="UTC") #setting up the timezone
stock(symbols, currency="USD", multiplier=1)

#-------------Initiate portfolio and account-----------
strategy.st <- "strat1" #Name the strategy
account.st <- "strat1"
portfolio.st <- "strat1"
rm.strat(strategy.st)

#Initiate portfolio
initPortf(name=portfolio.st, 
          symbols=symbols, 
          initDate=initDate,
          currency = "USD"
          )

#Initiate account
initAcct(name=account.st, 
         portfolios=portfolio.st,    
         initDate=initDate, 
         initEq=initEq)

#Initiate orders
initOrders(portfolio=portfolio.st, 
           initDate=initDate
           )

#Store all the events in the strategy
strategy(strategy.st, store=TRUE)

### -------Add Indicators------
# DVO indicator
# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 20) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/(Hi(HLC) + Lo(HLC))/2
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  out
}

# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, 
              name = "DVO", 
              arguments = list(
                HLC = quote(HLC(mktdata)), 
                navg = 2, 
                percentlookback = 20),
              label = "DVO_2_126")


# SMA indicators
nfast <- 12
nslow <- 26
# Add fast SMA indicator
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)),
                n = nfast
              ),
              label="nFast"
            )
# Add slow SMA indicator
add.indicator(strategy.st, 
              name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)),
                n = nslow
              ),
              label="nSlow"
            )


# Use applyIndicators to test out your indicators
prueba <- applyIndicators(strategy = strategy.st, mktdata = HLC(datos))


##____Add Signals___
# Implement a sigThreshold which specifies that DVO_2_126 must be 
#less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            
                            # The threshold is 20
                            threshold = 20, 
                            
                            # We want the oscillator to be under this value
                            relationship = "lt", 
                            
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           
           # Label it longthreshold
           label = "longthreshold")


# Add a sigThreshold signal to your strategy that specifies that DVO_2_126 
#must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold", 
           
           # Reference the column of DVO_2_126
           arguments = list(column = "DVO_2_126", 
                            
                            # Set a threshold of 80
                            threshold = 80, 
                            
                            # The oscillator must be greater than 80
                            relationship = "gt", 
                            
                            # We are interested only in the cross
                            cross = TRUE), 
           
           # Label it thresholdexit
           label = "thresholdexit")


# Buy signal appears when SMA30 is greater than SMA200. longfilter is BUY
add.signal(strategy.st, 
           name='sigComparison',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gt"
           ),
           label='longfilter'
          )

# Sell signals appears when SMA30 is less than SMA200. filterexit is SELL
add.signal(strategy.st, 
           name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='filterexit'
          )

# Add a sigFormula signal to your code specifying that both longfilter and 
#longthreshold must be TRUE, label it longentry
add.signal(strategy.st, 
           name = "sigFormula",
           
           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold", 
                            
                            # Specify that cross must be TRUE
                            cross = TRUE),
           
           # Label it longentry
           label = "longentry")


prueba_init <- applyIndicators(strategy.st, mktdata = OHLC(datos))
prueba2 <- applySignals(strategy = strategy.st, mktdata = prueba_init)

###----RULES----
# Fill in the rule's type as exit
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='filterexit', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        replace=FALSE,
                        prefer = 'Open'
         ),
         type='exit',
         label='Exit2SHORT'
        )

osMaxDollar <- function (data, timestamp, orderqty, ordertype, orderside, portfolio, 
                         symbol, prefer = "Open", tradeSize, maxSize, integerQty = TRUE, 
                         ...) 
{
  pos <- getPosQty(portfolio, symbol, timestamp)
  if (prefer == "Close") {
    price <- as.numeric(Cl(mktdata[timestamp, ]))
  }
  else {
    price <- as.numeric(Op(mktdata[timestamp, ]))
  }
  posVal <- pos * price
  if (orderside == "short") {
    dollarsToTransact <- max(tradeSize, maxSize - posVal)
    if (dollarsToTransact > 0) {
      dollarsToTransact = 0
    }
  }
  else {
    dollarsToTransact <- min(tradeSize, maxSize - posVal)
    if (dollarsToTransact < 0) {
      dollarsToTransact = 0
    }
  }
  qty <- dollarsToTransact/price
  if (integerQty) {
    qty <- trunc(qty)
  }
  return(qty)
}

# Add a rule that uses an osFUN to size an entry position
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          
                          # Use the osFUN called osMaxDollar
                          osFUN = osMaxDollar,
                          
                          # The tradeSize argument should be equal to tradesize (defined earlier)
                          tradeSize = tradesize,
                          
                          # The maxSize argument should be equal to tradesize as well
                          maxSize = tradesize),
         type = "enter")


out <- applyStrategy(strategy.st, portfolio.st)


# Update
# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)

# Update your account (account.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
daterange
updateAcct(account.st, daterange)

updateEndEq(account.st)

# ------Profit Factor------
# A profit factor above 1 means your strategy is profitable.
# A profit factor below 1 means you should head back to the drawing board
# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor
# This percent positive statistic means that approximately 71% of your 
#trades returned a positive result.
tstats$Percent.Positive 

# plot
for(symbol in symbols) {
  chart.Posn(Portfolio=portfolio.st,
             Symbol=symbol,
             log=TRUE)
}

#details of the trading statistics of trading strategies such as 
#the number for trades, and profitability of trades.
tstats <- tradeStats(portfolio.st)
t(tstats) #transpose tstats

# evaluate the performance to see how is the return of the trading strategy
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
tab <- table.Arbitrary(rets,
                       metrics=c(
                         "Return.cumulative",
                         "Return.annualized",
                         "SharpeRatio.annualized",
                         "CalmarRatio"),
                       metricsNames=c(
                         "Cumulative Return",
                         "Annualized Return",
                         "Annualized Sharpe Ratio",
                         "Calmar Ratio"))
tab

charts.PerformanceSummary(rets, colorset = "#DB005B")


# Cash Sharpe ratio
portpl <- .blotter$portfolio.strat1$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric = FALSE)

# Returns Sharpe ratio 
# Get instrument returns
instrets <- PortfReturns(portfolio.st)
# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)

summary(getStrategy(portfolio.st))
trades <- getTxns(portfolio.st, symbols)
trades <- data.frame(trades)
veamos <- cbind(mktdata, trades)


