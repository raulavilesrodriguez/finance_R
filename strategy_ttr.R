library(tidyverse) 
library(dplyr) 
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(devtools)
library(yahoofinancer)

# ___Inputs___
from <- "2023-05-23"
to <- "2023-05-26"

getSymbols(c("AMZN","DAL"))
getSymbols("AAPL",from=from, to=to, src="yahoo", interval = "1m")
aapl <- Ticker$new('AAPL')
AAPL <- aapl$get_history(start = from, end = to, interval = '1m')

df <- AMZN
head(df)
df2 <- df

#____Indicators____
# Returns from Open to Close, Hi to Close, or Close to Close
# These functions are merely to speed the model specification process
df2$OpCl <- OpCl(df2)
df2$OpOp <- OpOp(df2) 
df2$HiCl <- HiCl(df2) 
df2$ClCl <- ClCl(df2) #for the changes in closing prices

# Calculate Percent Change
df2$pcntOpCl1 <- Delt(Op(df2),Cl(df2),k=1)
df2$pcntOpCl2 <- Delt(Op(df2),Cl(df2),k=2)
df2$pcntOpCl3 <- Delt(Op(df2),Cl(df2),k=3)

# Create a lagged series from data, with NA used to fill. Lag(retraso)
#One period lag of the close 
df2$lagCl <- Lag(Cl(df2)) 
df2$lag2Cl <- Lag(Cl(df2),2)  
df2$lag3Cl <- Lag(Cl(df2),3) 

# Move up the OpCl by one period
df2$nextOpCl <- Next(OpCl(df2)) 

head(df2)

#the MACD and RSI functions in “TTR” to generate the technical indicators.
#The dataset “macd” has two columns: macd and its 9-period moving average called ‘signal’

indicadores <- function(datos){
  macd <- MACD(datos, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
  rsi <- RSI(df2$AMZN.Adjusted, n = 14, maType = "SMA")
  list(macd, rsi)
}

results <- indicadores(df[,6])
macd <- results[[1]]
rsi <- results[[2]]
tail(macd)

# Plot data
options(repr.plot.width = 6, repr.plot.height = 3)
chartSeries(df, subset = "2018::2018-06",
            theme="white",  
            TA="addVo();addBBands();addCCI(); 
                addTA(OpCl(AMZN),col='blue', type='h')  ")

#_____STRATEGY_____
# Strategy 1: if macd>signal, enter and stay in the market. If macd<signal, exit the market.
strategy1 <- ifelse ((macd$signal < macd$macd) , 1, 0)
strategy1[is.na(strategy1)] <-0

# Strategy 2: if overbought, enter and stay in the market.
strategy2 <- ifelse ((macd$signal < macd$macd) & (rsi$rsi > 70), 1, 0)
strategy2[is.na(strategy2)] <-0

# Strategy 3: if oversold, enter and stay in the market.
strategy3 <- ifelse ((macd$signal > macd$macd) & (rsi$rsi < 30), 1, 0)
strategy3[is.na(strategy3)] <-0


# Buy-and-hold: keep it all time. So "1", not "0"
bh_strategy <- rep(1,dim(macd)[1])

# __Bactesting__
rtn.daily <- dailyReturn(df) # returns by day
# Put in a function
backtest <- function(df,from_date,to_date,strategy,strategy_name){
  trade_return <- rtn.daily[index(rtn.daily)<=to_date & index(rtn.daily)>=from_date]*lag(strategy)
  cumm_return <- Return.cumulative(trade_return)
  annual_return <- Return.annualized(trade_return) 
  summary(as.ts(trade_return))
  SharpeRatio <- SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")
  SharpeRatioAnnualized <- SharpeRatio.annualized(trade_return, Rf = 0)
  out <- as.data.frame(c(cumm_return,annual_return,SharpeRatio,SharpeRatioAnnualized))
  out <- round(out,2)
  colnames(out) <- strategy_name
  row.names(out) <- c('Cumulative Return','Annualized Return','Sharpe Ratio','Annualized Sharpe Ratio')
  
  return( out )
  
}

# Strategy 1
strategy1_performance <- backtest(df, from_date = '2007-01-03', to_date = '2015-12-31', strategy1,"Strategy1")
strategy1_performance

# Strategy 2
strategy2_performance <- backtest(df, from_date = '2007-01-03', to_date = '2015-12-31', strategy2,"Strategy2")
strategy2_performance

# Strategy 3
strategy3_performance <- backtest(df, from_date = '2007-01-03', to_date = '2015-12-31', strategy3,"Strategy3")
strategy3_performance

#____VIEW RESULTS OF STRATEGIES____
#Interactive Plot
highchart(type="stock") %>% 
  hc_yAxis_multiples(list(title = list(text = "Price"), opposite = FALSE),
                     list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Signal"))) %>%
  hc_add_series(df) %>% 
  hc_add_series(SMA((Cl(df)),n=12),name="SMA(12)") %>% 
  hc_add_series(SMA((Cl(df)),n=26),name="SMA(26)") %>% 
  hc_add_series(strategy1, name="Estrategia_1", yAxis = 1, color = "#5C469C") %>%
  hc_title(text= paste0("<b>", colnames(df)[4], "Price Candle Stick Chart</b>"))

chartSeries(df, subset = "2022-01::2023-05-19")
addTA(strategy1, type='S',col='red')
addTA(strategy2, type='S',col='#FFE569')
addTA(strategy3, type='S',col='#FDCEDF')

# Charting return strategies
return_df <- dailyReturn(Cl(df))
strategy1.ret <- return_df * strategy1
names(strategy1.ret) <- 'Estrategy 1'
strategy2.ret <- return_df * strategy2
names(strategy2.ret) <- 'Estrategy 2'
strategy3.ret <- return_df * strategy3
names(strategy3.ret) <- 'Estrategy 3'
retall <- cbind(strategy1.ret, strategy2.ret, strategy3.ret)
charts.PerformanceSummary(retall, main = "Compararing Strategies")



