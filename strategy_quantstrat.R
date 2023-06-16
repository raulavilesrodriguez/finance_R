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
initDate <- "2020-01-02" #Date of initiation
from <- "2021-06-03"
to <- "2023-06-09"
orderqty <- 1 # units stocks   orderqty = tradeSize / ClosePrice
initEq <- 100 #Initial equity USD

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

### ---Add Indicators---
# DVO indicator
# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
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
                percentlookback = 126),
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

# Add RSI_3 indicator to strategy.st
add.indicator(strategy.st,
              name = "RSI",
              arguments = list(
                price = quote(Cl(mktdata)),
                n = 3     # RSI 3 function
              ),
              label = "RSI_3"
            )

# Write the calc_RSI_avg function
calc_RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  # Your output of RSI_avg needs a column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, 
              name = "calc_RSI_avg", 
              arguments = list(
                price = quote(Cl(mktdata)), 
                n1 = 3, 
                n2 = 4), 
              label = "RSI_3_4")


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

### rules
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='filterexit', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        replace=FALSE
         ),
         type='exit',
         label='Exit2SHORT',
         path.dep=TRUE
        )

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='longfilter', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market', 
                        orderqty=orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG',
         path.dep=TRUE
        )

out <- try(applyStrategy(strategy.st, portfolio.st))

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



