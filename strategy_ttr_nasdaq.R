library(tidyverse) 
library(dplyr) 
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(devtools)
library(yahoofinancer)
library(lubridate)
library(rvest)
library(httr)
library(readxl)
library(sets) # to fuzzy model


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
to <- "2023-05-26"

stock <- symbols$Symbol[sample(1:nrow(symbols),1)]

# Market performance
market <- getSymbols("^NDX",from=from, to=to, src="yahoo", env = NULL)

# investment stock
prices <- getSymbols(stock,from=from, to=to, src="yahoo", env = NULL)

class(market)
class(prices)
colnames(prices) <- c('Open','High','Low','Close','Volume','Adjusted')
colnames(market) <- c('Open','High','Low','Close','Volume','Adjusted')

#Export tibble
writexl::write_xlsx(data.frame(market), 'market.xlsx')
writexl::write_xlsx(data.frame(prices), 'prices.xlsx')
market <- read_excel('market.xlsx')
prices <- read_excel('prices.xlsx')





