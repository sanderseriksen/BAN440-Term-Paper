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
Vinmonopolet_market <- read_excel("demand_data.xlsx")

### Data preparation ###########################################################

# Narrowing down the data to only contain relevant markets
# Excluding the largest cities because they are not representative

# Filter out the largest cities
demand_data <- Vinmonopolet_market %>%
  filter(Population < 150) %>% 
  mutate(Number_of_stores = as.factor(Number_of_stores))

# Train and test split, training data all observations with a store
train_data <- Vinmonopolet_market %>%
  filter(Number_of_stores > 0)

# Test data all observations without a store
test_data <- Vinmonopolet_market %>%
  filter(Number_of_stores == 0)

### Model selection ###########################################################

# Forward selection
forward_model <- step(lm(Sales ~ 1, data = train_data), 
                      scope = ~ Population + Grensehandel + n_stays + Monthly_salary + Area + Number_of_stores + Spread,
                      direction = "forward")

summary(forward_model)

# Backward selection
backward_model <- step(lm(Sales ~ Population + Grensehandel + n_stays + Monthly_salary + Area + Number_of_stores + Spread, 
                          data = train_data), 
                       direction = "backward")

summary(backward_model)



# Linear regression model for predicting sales with all the variables
var_test <- lm(Sales ~ Population + Grensehandel + n_stays + Monthly_salary + Area +
            Number_of_stores + Spread,
          data = Vinmonopolet_market)

var_test1 <- lm(Sales ~ Population + Grensehandel + n_stays + Monthly_salary + Area +
                 Number_of_stores + Spread,
               data = demand_data)

stargazer(var_test, var_test1, type = "text")

# From these regressions we see that we want to remove the "Area" and "prop_spread" variables
# from the regressions as they are not significant.



### Demand estimation ##########################################################

## Linear regression

# Predicting sales using the training data
reg1 <- lm(Sales ~ Population + Grensehandel + n_stays + Monthly_salary,
           data = train_data)

summary(reg1)


# Applying the model on the test data
test_data$Sales_pred <- predict(reg1, newdata = test_data)

## Merge predicted data into the original data

# Deselect unnecessary columns to merge the data easier
test_data <- test_data %>%
  select(Municipality_Code, Sales_pred)

# Merge the data frames
predicted_data <- Vinmonopolet_market %>%
  left_join(test_data, by = "Municipality_Code") %>%
  mutate(Sales = ifelse(Sales == 0, Sales_pred, Sales)) %>%
  select(-Sales_pred) %>% 
  mutate(Sales = ifelse(Sales < 0, 0, Sales),
         Number_of_stores = as.integer(Number_of_stores)) %>% 
  filter(Number_of_stores < 2)



## Logit regression ############################################################

# Logit model for probability of having a store, for "demand_data" using Sales
logit <- glm(Number_of_stores ~ Sales + Population + Grensehandel + n_stays + Monthly_salary,
             data = predicted_data, family = binomial)

summary(logit)

# Checking model accuracy
pred <- predict(logit, newdata = predicted_data, type = "response")

# Add the predicted probabilities to the data
predicted_data$prob <- pred

# Filter the predicted data to only contain observations without a store and 
# more than 15 on "dist_to_nearest_store". Arrange descending by "prob"
predicted_data %>%
  filter(Number_of_stores == 0,
         Dist_nearest > 15) %>% 
  arrange(desc(prob)) %>%
  select(Mun_name, prob, Dist_nearest, Sales, Population, Region_Name)









