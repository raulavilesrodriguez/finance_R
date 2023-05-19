library(quantmod)
library(highcharter) #Interactive Plot
library(tidyverse)

price_bbri <- getSymbols("BBRI.JK", auto.assign=FALSE, from="2014-01-01", to="2023-05-18")
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
  hc_title(text="<b>BBRI Price Candle Stick Chart 2014-2019</b>")










