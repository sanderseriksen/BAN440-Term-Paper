#### Bresnahan & Reiss test document ####

# relevant libraries
library(tidyverse)
library(readxl)
library(fastDummies)
library(knitr)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data
Vinmonopolet_market <- read_excel("B&R_data.xlsx")

# Calculate rho, the raw correlation between Population and number of stores
rho <- cor(Vinmonopolet_market$Population, Vinmonopolet_market$Number_of_stores)



## Data preparation ############################################################

# Filtering data for B&R
br_data <- Vinmonopolet_market %>%
  filter(Population < 150000 & Area > 0 & Population > 0)

# Adding variables to the data
upperb <- 3

br_data <- br_data %>% 
  mutate(
    s = Population / 1000,
    log_s = log(s),
    density = Number_of_stores / Area,
    Number_of_stores = as.factor(ifelse(Number_of_stores <= upperb, Number_of_stores, upperb))
  ) %>% 
  dummy_cols(select_columns = "Number_of_stores") %>% 
  mutate_at(vars(starts_with("Number_of_stores")), as.factor)

# Scale the numeric variables
br_data <- br_data %>% 
  mutate_at(vars(Population, s, log_s, Area, Grensehandel, n_stays, Monthly_salary, Dist_nearest), scale)






## Some interesting statistics ################################################

# These justify adding the variables to the data

# Table of the number of stores per market
table(br_data$Number_of_stores)

# Consumption per capita, grouped by region
Vinmonopolet_market %>% 
  group_by(Region_Name) %>%
  summarise(
    consumption = sum(Sales) / sum(Population),
    Sales = sum(Sales)
  ) %>% 
  arrange(desc(consumption))





## Linear regression model #####################################################

# Regression model to test
reg <- lm(as.numeric(Number_of_stores) ~ Population + Area + Grensehandel + n_stays + Monthly_salary + Dist_nearest,
          data = Vinmonopolet_market)

summary(reg)







### Fitting models #############################################################

# For municipalities with population from 0 to 150,000

# Library necessary for the polr function. 
# select() does not work after this is loaded
library(MASS)

# Model 1: Bresnahan & Reiss
model_1 <- polr(Number_of_stores ~ s, data = br_data, method = "probit")

summary(model_1)

# Extract coefficients and cutoffs
lambda1 <- model_1$coefficients  # Estimate for s
theta1 <- model_1$zeta  # Cutoff points

# Compute S_N (Thresholds for s)
S_N1 <- exp(theta1)  # Since there is only one predictor, no need for mean adjustment

# Create labels for S_N
upperb1 <- length(theta1)  # Number of thresholds
slab1 <- paste0("$S_", 1:upperb1, "$")
names(S_N1) <- slab1

# Compute ETR_N using the cutoffs
ETR_N1 <- exp(theta1[2:upperb1] - theta1[1:(upperb1-1)]) * (1:(upperb1-1)) / (2:upperb1)

# Create labels for ETR_N
elab1 <- paste0("$s_", 2:upperb1, "/s_", 1:(upperb1-1), "$")
names(ETR_N1) <- elab1

# Print results
S_N1
ETR_N1

kable(S_N1, col.names = c("'000s"), digits = 4,
      caption = 'Entry thresholds',
      booktabs = TRUE)




model_2 <- polr(Number_of_stores ~ log(s) + density, data = br_data, method = "probit")

summary(model_2)


## Model 1: Bresnahan & Reiss ##

# Extract coefficients and cutoffs
lambda1 <- model_1$coefficients  # Estimate for s
theta1 <- model_1$zeta  # Cutoff points

# Compute S_N (Thresholds for s)
S_N1 <- exp(theta1)  # Since there is only one predictor, no need for mean adjustment

# Create labels for S_N
upperb1 <- length(theta1)  # Number of thresholds
slab1 <- paste0("$S_", 1:upperb1, "$")
names(S_N1) <- slab1

# Compute ETR_N using the cutoffs
ETR_N1 <- exp(theta1[2:upperb1] - theta1[1:(upperb1-1)]) * (1:(upperb1-1)) / (2:upperb1)

# Create labels for ETR_N
elab1 <- paste0("$s_", 2:upperb1, "/s_", 1:(upperb1-1), "$")
names(ETR_N1) <- elab1

# Print results
S_N1
ETR_N1

kable(S_N1, col.names = c("'000s"), digits = 4,
      caption = 'Entry thresholds',
      booktabs = TRUE)

## Model 2 ##

# Extract coefficients and cutoffs
lambda <- model_2$coefficients  # Estimates for s and density
theta <- model_2$zeta  # Cutoffs

# Compute S_N using the new predictors
S_N <- exp(theta - mean(br_data$density) * lambda["density"])

# Create labels for S_N
upperb <- length(theta)  # Number of thresholds
slab <- paste0("$S_", 1:upperb, "$")
names(S_N) <- slab

# Compute ETR_N using the cutoffs
ETR_N <- exp(theta[2:upperb] - theta[1:(upperb-1)]) * (1:(upperb-1)) / (2:upperb)

# Create labels for ETR_N
elab <- paste0("$s_", 2:upperb, "/s_", 1:(upperb-1), "$")
names(ETR_N) <- elab

# Print results
S_N
ETR_N

kable(S_N, col.names = c("'000s"), digits = 4,
             caption = 'Entry thresholds',
             booktabs = TRUE)

table(br_data$Number_of_stores)
