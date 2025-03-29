#### Demand estimation of sales ####

# relevant libraries
library(tidyverse)
library(readxl)
library(fastDummies)
library(knitr)
library(stargazer)
library(caret)

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


lm_Area <- lm(Sales ~ Area, data = train_data)

summary(lm_Area)


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

# 1) Make sure the factor for Number_of_stores has valid R variable names
#    that won't cause errors in caret. For instance, rename "0" -> "NoStore"
#    and "1" -> "OneStore".
data_for_logit <- predicted_data %>%
  mutate(Number_of_stores = as.factor(Number_of_stores))

# Rename factor levels (originally "0" and "1") to "NoStore" and "OneStore"
data_for_logit$Number_of_stores <- factor(
  data_for_logit$Number_of_stores,
  levels = c("0", "1"),
  labels = c("NoStore", "OneStore")
)

# 2) Set up k-fold cross-validation parameters
set.seed(123)  # for reproducibility

my_control <- trainControl(
  method = "cv",            # k-fold CV
  number = 5,               # 5 folds
  classProbs = TRUE,        # needed for probability output
  summaryFunction = twoClassSummary
)

# 3) Train the logistic model with cross-validation
cv_model <- train(
  Number_of_stores ~ Sales + Population + Grensehandel + n_stays + Monthly_salary,
  data = data_for_logit,
  method = "glm",
  family = binomial,
  trControl = my_control,
  metric = "ROC"            # use AUC (Area Under the Curve) as our metric
)

# 4) Review cross-validation results
print(cv_model)
print(cv_model$results)

# 5) Get predicted probabilities from the final trained model
#    caret retrains on the entire dataset after CV by default
predicted_data$prob_cv <- predict(cv_model, newdata = data_for_logit, type = "prob")[, "OneStore"]

# 6) Use the probabilities for your recommendations
recommended_stores <- predicted_data %>%
  mutate(Number_of_stores = as.integer(as.character(Number_of_stores))) %>%
  filter(Number_of_stores == 0, Dist_nearest > 15) %>%
  arrange(desc(prob_cv)) %>%
  select(Mun_name, prob_cv, Dist_nearest, Sales, Population, Region_Name)

head(recommended_stores, 10)  # for example, show top 10

# 7) Output the top 10 recommended stores as a nice table using kable 
# And save it 
kable(head(recommended_stores, 10), format = "markdown")