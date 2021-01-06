## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(roller)

## ------------------------------------------------------------------------
fair_coin <- device() 
fair_coin

## ------------------------------------------------------------------------
weird_die <- device(
sides = c('i', 'ii', 'iii', 'iv'), 
prob = rep(1/4, 4))
weird_die

## ------------------------------------------------------------------------
is.device(weird_die)

## ------------------------------------------------------------------------
set.seed(123)
fair_50rolls <- roll(fair_coin, times = 50) 
fair_50rolls

## ------------------------------------------------------------------------
summary(fair_50rolls)

## ---- fig.show='hold'----------------------------------------------------
plot(fair_50rolls)

## ------------------------------------------------------------------------
fair_50rolls[1]
fair_50rolls[1] <- 1
fair_50rolls[1]
summary(fair_50rolls)
fair_60rolls <- fair_50rolls + 10
summary(fair_60rolls)

