#### Demand estimation of sales ####

# relevant libraries
library(tidyverse)
library(readxl)
library(fastDummies)
library(knitr)
library(stargazer)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data
Vinmonopolet_market <- read_excel("B&R_data.xlsx")

# Calculate rho, the raw correlation between Population and number of stores
rho <- cor(Vinmonopolet_market$Population, Vinmonopolet_market$Number_of_stores)


## Data preparation ############################################################

# Filtering data to only contatin 0 or 1 stores
demand_data <- Vinmonopolet_market %>%
  filter(Number_of_stores < 2) %>% 
  mutate(Number_of_stores = as.integer(Number_of_stores))

demand_data2 <- Vinmonopolet_market %>%
  filter(Number_of_stores < 5) %>% 
  mutate(Number_of_stores = as.factor(Number_of_stores))

demand_data3 <- Vinmonopolet_market %>%
  filter(Number_of_stores < 5) %>% 
  mutate(Sales_per_store = Sales / Number_of_stores,
         Number_of_stores = as.factor(Number_of_stores))

## Regression analysis #########################################################

# Train and test split, training data all observations with a store
train_data <- demand_data %>%
  filter(Number_of_stores == 1)

# Test data all observations without a store
test_data <- demand_data %>%
  filter(Number_of_stores == 0) %>% 
  mutate(Number_of_stores = as.factor(Number_of_stores))

# Regression where Sale is estimated
reg <- lm(Sales ~ Population + Area + Grensehandel + n_stays + Monthly_salary + prop_spread,
          data = train_data)

summary(reg)

# Predict the sales for all of the data
reg1 <- lm(Sales ~ Population + Grensehandel + n_stays + Monthly_salary,
           data = demand_data2)

summary(reg1)


# Test the model on the test data
test_data$Sales_pred <- predict(reg1, newdata = test_data)

# Calculate the mean absolute error
MAE <- mean(abs(test_data$Sales_pred - test_data$Sales))

# Merge predicted data into the original data

# Deselect unnecessary columns
test_data <- test_data %>%
  select(Municipality_Code, Sales_pred)

# Merge the data frames
merged_data <- Vinmonopolet_market %>%
  left_join(test_data, by = "Municipality_Code") %>%
  mutate(Sales = ifelse(Sales == 0, Sales_pred, Sales)) %>%
  select(-Sales_pred) %>% 
  mutate(Sales = ifelse(Sales < 0, 0, Sales))



## Logit regression ############################################################

# Logit model for probability of having a store, for "demand_data"
logit <- glm(Number_of_stores ~ Population + Grensehandel + n_stays + Monthly_salary,
             data = demand_data, family = binomial)

summary(logit)

# Checking model accuracy
pred <- predict(logit, newdata = demand_data, type = "response")

# Add the predicted probabilities to the data
demand_data$prob <- pred









