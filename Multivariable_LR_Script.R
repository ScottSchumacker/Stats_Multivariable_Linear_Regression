# Scott Schumacker
# Multivariable Linear Regression

# Loading packages
library(tidyverse)
library(lmtest)
library(gridExtra)

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

# Visualizing the relationship between our independent and dependent variables
# Visualizing data as scatter plot
P1 <- car_data %>% 
  ggplot(aes(mileage, price)) +
  geom_point() +
  theme_classic()

P2 <- car_data %>% 
  ggplot(aes(year, price)) +
  geom_point() +
  theme_classic()

P3 <- car_data %>% 
  ggplot(aes(engine_size, price)) +
  geom_point() +
  theme_classic()

P4 <- car_data %>% 
  ggplot(aes(horsepower, price)) +
  geom_point() +
  theme_classic()

grid.arrange(P1,P2,P3,P4, nrow = 2)

# Looking at model coefficients
lmModel <- lm(price ~ mileage + year + engine_size + horsepower, data = car_data)
summary(lmModel)

# Checking normality of residuals
hist(lmModel$residuals)
bptest(lmModel)