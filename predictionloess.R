library(tidyquant)
library(highcharter) #Interactive Plot
library(tidyverse)
library(caret)
library(readr)

# Inputs
from <- "2014-01-01"
to <- "2023-05-19"
n <- 10 # point to predict

df_price <- tq_get("BBRI.JK", from=from, to=to)

# delete rows with NA´s 
df_price <- na.omit(df_price)
head(df_price)

# Write to CSV file
write_csv(df_price, "data/bbri_jk.csv")
df_price <- read_csv('data/bbri_jk.csv')
df_price <- df_price[,-1]

# Splitting the data
set.seed(42)
level_test <- nrow(df_price)-n+1
test_set <- df_price[level_test:nrow(df_price), ]
level_train <- nrow(df_price)-n
train_set <- df_price[1:level_train, ]
nrow(train_set)
nrow(test_set)

# Loess model
set.seed(1)
train_loess <- train(close~date+volume,
                     method = "gamLoess",
                     data = train_set)
loess_preds <- predict(train_loess, test_set)
loess_preds <- unname(loess_preds)
test_set['prediction'] <- loess_preds
train_set['prediction'] <- 0
df_price <- rbind(train_set, test_set)


test_set |>
  ggplot(aes(date, prediction)) +
  geom_point(color = 'black') +
  geom_point(aes(date, close), color="red")


