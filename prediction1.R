library(quantmod)
library(highcharter) #Interactive Plot
library(tidyverse)

price_bbri <- getSymbols("BBRI.JK", auto.assign=FALSE, from="2014-01-01", to="2023-05-18")

# process to replace NA´s with the Last Non-Missing Value
price_bbri <- data.frame(price_bbri)
price_bbri <- price_bbri |> fill(colnames(price_bbri),.direction = "downup")
# transform to xts zoo form
price_bbri <- as.xts(price_bbri)
head(price_bbri)

# Candle stick chart
chartSeries(price_bbri, name = "BBRI Price 2014-2023")

# Take only the closing price
closing_pr <- Cl(to.monthly(price_bbri))

# Decompose it
dc <- decompose(as.ts(closing_pr, start=c(2014,1)))
plot(dc)

# Seasonal component 
dc$seasonal

#Interactive Plot
highchart(type="stock") %>% 
  hc_add_series(price_bbri) %>% 
  hc_add_series(SMA(na.omit(Cl(price_bbri)),n=50),name="SMA(50)") %>% 
  hc_add_series(SMA(na.omit(Cl(price_bbri)),n=200),name="SMA(200)") %>% 
  hc_title(text="<b>BBRI Price Candle Stick Chart 2014-2023</b>")


# Fetch BBNI, BMRI, and IHSG stock prices
price_bbni <- getSymbols("BBNI.JK",auto.assign=FALSE,from="2014-01-01",to="2023-05-18")
price_bmri <- getSymbols("BMRI.JK",auto.assign=FALSE,from="2014-01-01",to="2023-05-18")
price_ihsg <- getSymbols("^JKSE",auto.assign=FALSE,from="2014-01-01",to="2023-05-18")

# process to replace NA´s with the Last Non-Missing Value
price_bbni <- data.frame(price_bbni)
price_bbni <- price_bbni |> fill(colnames(price_bbni),.direction = "downup")
price_bbni <- as.xts(price_bbni)

price_bmri <- data.frame(price_bmri)
price_bmri <- price_bmri |> fill(colnames(price_bmri),.direction = "downup")
price_bmri <- as.xts(price_bmri)

price_ihsg <- data.frame(price_ihsg)
price_ihsg <- price_ihsg |> fill(colnames(price_ihsg),.direction = "downup")
price_ihsg <- as.xts(price_ihsg)


# this chart to compare our stock price with other stocks in the same sector
# Compare the stock prices
highchart(type="stock") %>% 
  hc_add_series(Cl(price_bbri), name="BBRI") %>% 
  hc_add_series(Cl(price_bbni), name="BBNI") %>% 
  hc_add_series(Cl(price_bmri), name="BMRI") %>% 
  hc_add_series(Cl(price_ihsg), name="IHSG") %>% 
  hc_title(text="<b>BBRI vs BBNI vs BMRI vs IHSG Closing Price</b>")

# RETURNS
# Calculate the stocks return
return_bbri <- dailyReturn(Cl(price_bbri))
return_bbni <- dailyReturn(Cl(price_bbni))
return_bmri <- dailyReturn(Cl(price_bmri))

# Combine the returns as one data frame
returns <- data.frame(return_bbri,return_bbni,return_bmri)
names(returns) <- c("return_bbri","return_bbni","return_bmri")
returns <- returns |> mutate(return_bbri = ifelse(return_bbri != Inf, return_bbri, 0),
                  return_bbni = ifelse(return_bbni != Inf, return_bbni, 0),
                  return_bmri = ifelse(return_bmri != Inf, return_bmri, 0)
                  )
returns <- as.xts(returns)

# Plot the returns
library(PerformanceAnalytics)
charts.PerformanceSummary(returns,main="Daily Return BBRI vs BBNI vs BMRI 2014-2019")

# ___Splitting The Data___
# Number of period we want to forecast
n <- 100

# Splitting the data
train <- head(Cl(price_bbri), length(Cl(price_bbri))-n)
test <- tail(Cl(price_bbri), n)

# 1. ___Naive Method___
library(forecast)

# Forecast the data
fc_na <- naive(train, h=n)

# Plot the result
autoplot(fc_na) +
  autolayer(ts(test, start=length(train)), series = "Test Data")








