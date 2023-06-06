library(tidyverse) 
library(dplyr) 
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(yahoofinancer)
#library(lubridate)
library(rvest)
library(httr)


#---load data for a list of stocks---
# Web scraping to obtain symbols of stocks
page <- paste0('https://www.slickcharts.com/nasdaq100')
data_symbols <- read_html(page, encoding="UTF-8")
class(data_symbols)
html_text(data_symbols)
tabla <- data_symbols |> html_elements(xpath = '//div/table[@class="table table-hover table-borderless table-sm"]')
tabla <- tabla[[1]] |> html_table()
class(tabla)
symbols <- tabla |> select(Company, Symbol)

# ___Inputs___
from <- "2019-01-01"
to <- "2023-06-05"

stock <- symbols$Symbol[sample(1:nrow(symbols),1)]

# Market performance
market <- getSymbols("^NDX",from=from, to=to, src="yahoo", env = NULL)

# investment stock
prices <- getSymbols(stock,from=from, to=to, src="yahoo", env = NULL)

class(market)
class(prices)
colnames(prices) <- c('Open','High','Low','Close','Volume','Adjusted')
colnames(market) <- c('Open','High','Low','Close','Volume','Adjusted')


# Calculus of indicators
add_indicadores <- function(prices){
  # MACD
  macd <- MACD(prices$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
  colnames(macd) <- c('macd_value', 'macd_signal')
  prices <- cbind(prices, macd)
  # RSI
  rsi <- RSI(prices$Close, n = 14, maType = "SMA")
  prices <- cbind(prices, rsi)
  # SO Stochastic Oscillator
  so <- stoch(prices[,2:4], nFastK = 10, nFastD = 3, nSlowD = 3) * 100
  colnames(so)[1] <- 'so'
  prices <- cbind(prices, so)
  # OBV On Balance Volume
  obv <- OBV(prices[,4], prices[,5])
  obv$ema <- EMA(obv$obv,n=3) #compute the exponential moving average with n = 3
  obv$ema[is.na(obv$ema)] <- 0
  prices <- cbind(prices, obv)
  # Semidev Calculations
  semidev <- rep(NA, 13)
  for (i in 14:length(prices$Adjusted)) {
    semidev <- c(semidev, SemiDeviation(R = ROC(prices$Adjusted[(i-13):i], n = 1, type = "discrete"),
                                        MAR=0))
  }
  prices <- cbind(prices, semidev)
  prices
}

prices <- add_indicadores(prices)
#nrow(prices[is.na(prices$macd_signal), ])
head(prices)
summary(prices)

#Interactive Plot
highchart(type="stock") %>% 
  hc_add_series(prices[,1:6]) %>% 
  hc_add_series(SMA((Cl(prices)),n=12),name="SMA(12)") %>% 
  hc_add_series(SMA((Cl(prices)),n=26),name="SMA(26)") %>%
  hc_title(text=paste0("<b>Prices company: ", symbols[which(symbols$Symbol == stock),1], "</b>"))

chartSeries(prices[,1:6], subset = "2022-01::2023-06-05")
addTA(prices$rsi, type='S', col='blue', lty='dashed')

#returns
charts.PerformanceSummary(
  ROC(prices$Adjusted,n=1,type="discrete"),
  main=paste(stock,"Performancesummary")
)







