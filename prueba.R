library(tidyverse) 
library(dplyr) 
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(devtools)
library(yahoofinancer)

price_bbri <- getSymbols("BBRI.JK", auto.assign=FALSE, from="2014-01-01", to="2023-05-19")
highchart(type="stock") %>% 
  hc_add_series(price_bbri) %>% 
  hc_add_series(SMA(na.omit(Cl(price_bbri)),n=50),name="SMA(50)") %>% 
  hc_add_series(SMA(na.omit(Cl(price_bbri)),n=200),name="SMA(200)") %>% 
  hc_title(text="<b>BBRI Price Candle Stick Chart 2014-2023</b>")

