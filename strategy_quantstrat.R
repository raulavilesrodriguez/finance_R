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
#.blotter <- new.env() #create the required environments
#.strategy <- new.env() #create the required environments

# ___Inputs___
initDate <- "2017-01-02" #Date of initiation
from <- "2018-01-03"
to <- "2023-05-26"
initEq <- 1000 #Initial equity
orderqty <- 100

symbols <- c("MSFT", "IBM")
getSymbols(symbols, from = from, to = to, adjust = TRUE)

#set the currency and the stock we are interested
currency("USD")
stock(symbols, currency="USD", multiplier=1)
Sys.setenv(TZ="UTC") #setting up the timezone

#-------------Initiate portfolio and account-----------
strategy.st <- "luxor" #Name the strategy
account.st <- "luxor"
portfolio.st <- "luxor"
rm.strat("luxor")

#Initiate portfolio
initPortf(name=portfolio.st, 
          symbols=symbols, 
          #initDate=initDate, 
          currency='USD')

#Initiate account
initAcct(name=account.st, 
         portfolios=portfolio.st,    
         #initDate=initDate, 
         currency='USD', 
         initEq=initEq)

#Initiate orders
initOrders(portfolio=portfolio.st, 
           symbols=symbols,
           initDate=initDate)

#Store all the events in the strategy
strategy(strategy.st, store=TRUE)

### indicators
nfast <- 12
nslow <- 26
add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nfast
              ),
              label="nFast"
)
add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nslow
              ),
              label="nSlow"
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='buy'
)
add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='sell'
)

### rules
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='sell', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='buy' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='market', 
                        orderqty=orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

out <- try(applyStrategy(strategy.st, portfolio.st))
out

#update
updatePortf(strategy.st)
updateAcct(strategy.st)
updateEndEq(strategy.st)


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

charts.PerformanceSummary(rets, colorset = bluefocus)



