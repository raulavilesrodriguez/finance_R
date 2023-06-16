library(tidyverse) 
library(dplyr) 
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(yahoofinancer)
library(lubridate)
library(rvest)
library(httr)
#conflict_prefer("%>%", "dplyr") # to avoid conflicts with library(sets)

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
from <- "2023-01-01"
to <- "2023-06-05"

stock <- symbols$Symbol[sample(1:nrow(symbols),1)]

# Market performance
market <- getSymbols("^NDX",from=from, to=to, src="yahoo", env = NULL)

# investment stock
prices <- getSymbols(stock,from=from, to=to, src="yahoo", env = NULL)

class(market)
class(prices)
colnames(prices) <- c('Open','High','Low','Close','Volume','Adjusted')
#colnames(market) <- c('Open','High','Low','Close','Volume','Adjusted')


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
highchart(type="stock") |> 
  hc_add_series(prices[,1:6]) |> 
  hc_add_series(SMA((prices$Close),n=12),name="SMA(12)") |> 
  hc_add_series(SMA((prices$Close),n=26),name="SMA(26)") |>
  hc_title(text=paste0("<b>Prices company: ", symbols[which(symbols$Symbol == stock),1], "</b>"))

chartSeries(prices[,1:6], subset = "2022-01::2023-06-05")
addTA(prices$rsi, type='S', col='blue', lty='dashed')

#returns
charts.PerformanceSummary(
  ROC(prices$Adjusted,n=1,type="discrete"),
  main=paste(stock,"Performancesummary")
)

#-------STRATEGY-------
# Signals definition
signal_variables <- prices[,c('Adjusted')]

# signal MACD
signal_variables$macd_signal <- ifelse(prices$macd_value > prices$macd_signal, 1, -1)
table(signal_variables$macd_signal)
# we will assume 33 low and 66 high to MACD fuzzy
signal_variables$macd_signal_fuzzy <- signal_variables$macd_signal
signal_variables$macd_signal_fuzzy[which(signal_variables$macd_signal_fuzzy == 1)] <- 66
signal_variables$macd_signal_fuzzy[which(signal_variables$macd_signal_fuzzy == -1)] <- 33

# signal RSI
signal_variables$rsi_signal <- ifelse(prices$rsi > 70, 1, 0)
signal_variables$rsi_signal <- ifelse(prices$rsi < 30, -1, signal_variables$rsi_signal)
table(signal_variables$rsi_signal)
# values are ready scaled between 0 and 100 to RSI fuzzy
signal_variables$rsi_signal_fuzzy <- prices$rsi

# signal SO
signal_variables$so_signal <- ifelse(prices$so > 80, 1, 0)
signal_variables$so_signal <- ifelse(prices$so < 20, -1, signal_variables$so_signal)
table(signal_variables$so_signal)
# values are ready scaled between 0 and 100 to SO fuzzy
signal_variables$so_signal_fuzzy <- prices$so

# signal OBV
signal_variables$obv_signal <- ifelse(prices$ema > lag(prices$ema), 1, -1)
table(signal_variables$obv_signal)
# we will assume 33 low and 66 high to OBV fuzzy
signal_variables$obv_signal_fuzzy <- signal_variables$obv_signal
signal_variables$obv_signal_fuzzy[which(signal_variables$obv_signal_fuzzy == 1)] <- 66
signal_variables$obv_signal_fuzzy[which(signal_variables$obv_signal_fuzzy == -1)] <- 33

# signal SEMIDEV fuzzy
# long (buy) when below the 33th percentile, short (sell) when above the 66th
signal_variables$semidev_signal_fuzzy <- 50
signal_variables$semidev_signal_fuzzy[which(prices$semidev > quantile(prices$semidev, 0.7, na.rm = T))] <- 70
signal_variables$semidev_signal_fuzzy[which(prices$semidev < quantile(prices$semidev, 0.3, na.rm = T))] <- 30
table(signal_variables$semidev_signal_fuzzy)

tail(signal_variables)

#signal_variables[is.na(signal_variables)] <- 0

#------FUZZY MODEL---------
library(sets)

# assuming all values are between 1 and 100
sets_options("universe", seq(1, 100, 0.1))

sd_macd_fuzzy <- sd(na.omit(signal_variables$macd_signal_fuzzy))

# Fuzzy variables
variables <- set(
  macd = fuzzy_partition(varnames = c(low = 33, high = 66), sd = 4.99),
  rsi = fuzzy_partition(varnames = c(low = 15, med = 50, high = 85), sd = 5.0),
  so = fuzzy_partition(varnames = c(low = 10, med = 50, high = 90), sd = 5.0),
  obv = fuzzy_partition(varnames = c(low = 33, high = 66), sd = 4.98),
  semidev = fuzzy_partition(varnames = c(low = 30, med = 50, high = 70), sd = 5.0),
  signal = fuzzy_partition(varnames = c(sell = 10, hold = 50, buy = 90), FUN = fuzzy_cone, radius = 10)
)

# Fuzzy rules
rules<-set(
  fuzzy_rule(macd %is% high && rsi %is% low && so %is% low && obv %is% high, signal %is%
               buy),
  
  fuzzy_rule(macd %is% low && rsi %is% high && so %is% high && obv %is% low, signal %is%
               buy),
  
  fuzzy_rule(macd %is% high && rsi %is% med && so %is% med && obv %is% high , signal %is%
               buy),
  
  fuzzy_rule(macd %is% low && rsi %is% med && so %is% high && obv %is% low , signal %is%
               sell),
  
  fuzzy_rule(rsi %is% high && so %is% low && obv %is% low , signal %is% sell),
  
  fuzzy_rule(macd %is% low && rsi %is% high && so %is% high , signal %is% sell),
  fuzzy_rule(macd %is% low && rsi %is% high && so %is% med , signal %is% hold),
  
  fuzzy_rule(macd %is% high && rsi %is% med && so %is% med && obv %is% low , signal %is%
               hold)
)

# fuzzy model
model<-fuzzy_system(variables,rules)
model
plot(model)

#----Applying model to current data----
start_time <- Sys.time()

# Buy=1, Hold = 0, Sell = -1
final_signal <- c()
for (i in 1:nrow(signal_variables)) {
  if(any(is.na(signal_variables[i,]))){
    final_signal[i] <- 50
  } else {
    v1 <- as.numeric(signal_variables[i,]$macd_signal_fuzzy)
    v2 <- round(as.numeric(signal_variables[i,]$rsi_signal_fuzzy))
    v3 <- round(as.numeric(signal_variables[i,]$so_signal_fuzzy))
    v4 <- round(as.numeric(signal_variables[i,]$obv_signal_fuzzy))
    
    res <- fuzzy_inference(model, list(macd = v1, rsi = v2, so = v3, obv = v4))
    final_signal[i] <- gset_defuzzify(res, "largestofmax")
  }
}

end_time <- Sys.time()
delta_time <- end_time - start_time
delta_time

prices$final_signal <- final_signal
# correction for - Inf
prices$final_signal[which(is.infinite(prices$final_signal))] <- 50
summary(prices$final_signal)
table(prices$final_signal)

# Conversion to buy, hold, sell
prices$final_signal_adj <- ifelse(prices$final_signal <=20, -1, prices$final_signal )
prices$final_signal_adj <- ifelse(prices$final_signal_adj>61, 1, prices$final_signal_adj)
prices$final_signal_adj[prices$final_signal_adj !=1 & prices$final_signal_adj !=-1] <- 0
table(prices$final_signal_adj)

prices <- cbind(prices, market)

# RETURNS
# when it is -1 we sell, when 0 invert in NDX, when 1, we buy current stock
optimized_return <- c(0)
for(i in 2:nrow(prices)){
  if(prices$final_signal_adj[i-1]==0){
    optimized_return[i] <- as.numeric(prices$NDX.Adjusted[i]) / as.numeric(prices$NDX.Adjusted[i-1]) - 1
  }
  if(prices$final_signal_adj[i-1]==1){
    optimized_return[i] <- 1 * (as.numeric(prices$Adjusted[i]) / as.numeric(prices$Adjusted[i-1]) - 1)
  }
  if(prices$final_signal_adj[i-1]==-1){
    optimized_return[i] <- -1 * (as.numeric(prices$Adjusted[i]) / as.numeric(prices$Adjusted[i-1]) - 1)
  }
}

prices$optimized_return <- optimized_return
prices$current_return <- prices$Adjusted/lag(prices$Adjusted)-1
prices$current_return[1] <- 0

charts.PerformanceSummary(prices[,c("current_return",'optimized_return')])
table.Stats(prices[,c("current_return",'optimized_return')])
table.AnnualizedReturns(prices[,c("current_return",'optimized_return')])

# Plot final signal
chartSeries(prices[,1:6], subset = "2023-01::2023-06-05")
addTA(prices$final_signal_adj, type='S', col='#00DFA2')

#Interactive Plot
highchart(type="stock") |> 
  hc_yAxis_multiples(list(title = list(text = "Price"), opposite = FALSE),
                     list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Signal"))) |>
  hc_add_series(prices[,1:6]) |> 
  hc_add_series(SMA((prices$Close),n=12),name="SMA(12)") |> 
  hc_add_series(SMA((prices$Close),n=26),name="SMA(26)") |>
  hc_add_series(prices$final_signal_adj, name="Estrategia", yAxis = 1, color = "#FCC72C") |>
  hc_title(text=paste0("<b>Prices company: ", symbols[which(symbols$Symbol == stock),1], "</b>"))
  









