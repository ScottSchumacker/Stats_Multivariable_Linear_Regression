# Scott Schumacker
# Multivariable Linear Regression

# Loading packages
library(tidyverse)
library(lmtest)

# Creating data set
n <- 200

mileage <- rnorm(n, mean = 50000, sd = 20000)
year <- round(rnorm(n, mean = 2015, sd = 5))
engine_size <- runif(n, min = 1.5, max = 4.0)
horsepower <- round(rnorm(n, mean = 200, sd = 50))
mileage <- pmax(mileage, 1000)
horsepower <- pmax(horsepower, 70)

true_coefficients <- c(1000, -0.1, 1500, 200, 50)
error_sd <- 3000

price <- true_coefficients[1] +
  true_coefficients[2] * mileage +
  true_coefficients[3] * (year - 2000) +
  true_coefficients[4] * engine_size +
  true_coefficients[5] * horsepower +
  rnorm(n, 0, error_sd)

car_data <- data.frame(price, mileage, year, engine_size, horsepower)