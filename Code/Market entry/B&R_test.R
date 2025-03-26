#### Bresnahan & Reiss test document ####

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

# Filtering data for B&R
br_data <- Vinmonopolet_market %>%
  filter(Population < 100000 & Area > 0 & Population > 0)

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

# Correlation matrix
cor(br_data[, c("s", "log_s", "Area", "Grensehandel", "n_stays", "Monthly_salary", "Dist_nearest")])





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

reg1 <- lm(as.numeric(Number_of_stores) ~ Population + Area + Grensehandel + n_stays + Monthly_salary + Dist_nearest,
          data = br_data)

summary(reg1)

stargazer(reg, reg1, type = "text")




# Regression model with intraction terms
reg_int <- lm(as.numeric(Number_of_stores) ~ log_s + Dist_nearest + log_s : Dist_nearest,
              data = br_data)

summary(reg_int)


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

# Display the results in a table
kable(S_N1, col.names = c("'000s"), digits = 4,
      caption = 'Entry thresholds',
      booktabs = TRUE)

# Optionally, display the ETR_N3 in a table as well
kable(ETR_N1, col.names = c("ETR"), digits = 4,
      caption = 'Entry Threshold Ratios',
      booktabs = TRUE)





# Fit the model with one predictor
model_2 <- polr(Number_of_stores ~ log_s, data = br_data, method = "probit")

# Display the summary of the model
summary(model_2)

# Extract coefficients and cutoffs
lambda2 <- model_2$coefficients # Estimate for log_s
theta2 <- model_2$zeta # Cutoffs

# Compute S_N using the predictor log_s
# Since there's only one predictor, use its mean directly
S_N2 <- exp(theta2 - mean(br_data$log_s) * lambda2["log_s"])

# Create labels for S_N
upperb2 <- length(theta2) # Number of thresholds
slab2 <- paste0("$S_", 1:upperb2, "$")
names(S_N2) <- slab2

# Compute ETR_N using the cutoffs
ETR_N2 <- exp(theta2[2:upperb2] - theta2[1:(upperb2-1)]) * (1:(upperb2-1)) / (2:upperb2)

# Create labels for ETR_N
elab2 <- paste0("$s_", 2:upperb2, "/s_", 1:(upperb2-1), "$")
names(ETR_N2) <- elab2

# Print results
S_N2
ETR_N2

# Display the results in a table
knitr::kable(S_N2, col.names = c("'000s"), digits = 4,
             caption = 'Entry thresholds for Model 2',
             booktabs = TRUE)

# Optionally, display the ETR_N2 in a table as well
knitr::kable(ETR_N2, col.names = c("ETR"), digits = 4,
             caption = 'Entry Threshold Ratios for Model 2',
             booktabs = TRUE)


## Model 3

# Fit the model with the specified predictors
model_3 <- polr(Number_of_stores ~ log_s + Monthly_salary + Grensehandel + n_stays,
                data = br_data, method = "probit")

# Display the summary of the model
summary(model_3)

# Extract coefficients and cutoffs
lambda3 <- model_3$coefficients # Estimates for log_s, Monthly_salary, Grensehandel, and n_stays
theta3 <- model_3$zeta # Cutoffs

# Compute S_N using the sample means of all predictors
X_bar3 <- colMeans(br_data[, c("log_s", "Monthly_salary", "Grensehandel", "n_stays")])
S_N3 <- exp(theta3 - X_bar3 %*% lambda3)

# Create labels for S_N
upperb3 <- length(theta3) # Number of thresholds
slab3 <- paste0("$S_", 1:upperb3, "$")
names(S_N3) <- slab3

# Compute ETR_N using the cutoffs
ETR_N3 <- exp(theta3[2:upperb3] - theta3[1:(upperb3-1)]) * (1:(upperb3-1)) / (2:upperb3)

# Create labels for ETR_N
elab3 <- paste0("$s_", 2:upperb3, "/s_", 1:(upperb3-1), "$")
names(ETR_N3) <- elab3

# Display the results in a table
knitr::kable(S_N3, col.names = c("'000s"), digits = 4,
             caption = 'Entry thresholds for Model 3',
             booktabs = TRUE)

# Optionally, display the ETR_N3 in a table as well
knitr::kable(ETR_N3, col.names = c("ETR"), digits = 4,
             caption = 'Entry Threshold Ratios for Model 3',
             booktabs = TRUE)






## Model 4: The "Dream" model

summary(Vinmonopolet_market$Dist_nearest)
summary(br_data$Dist_nearest)

# Fit the model with the specified predictors
model_4 <- polr(Number_of_stores ~ log_s + Monthly_salary + Grensehandel + n_stays + Dist_nearest,
                data = br_data, method = "probit")



## Model 5

# Fit the model with the specified predictors
model_5 <- polr(Number_of_stores ~ log_s + n_stays, data = br_data, method = "probit")

# Display the summary of the model
summary(model_5)

# Extract coefficients and cutoffs
lambda5 <- model_5$coefficients # Estimates for log_s and Area
theta5 <- model_5$zeta # Cutoffs

# Compute S_N using the sample means of the predictors
X_bar5 <- colMeans(br_data[, c("log_s", "n_stays")])
S_N5 <- exp(theta5 - X_bar5 %*% lambda5)

# Create labels for S_N
upperb5 <- length(theta5) # Number of thresholds
slab5 <- paste0("$S_", 1:upperb5, "$")
names(S_N5) <- slab5

# Compute ETR_N using the cutoffs
ETR_N5 <- exp(theta5[2:upperb5] - theta5[1:(upperb5-1)]) * (1:(upperb5-1)) / (2:upperb5)

# Create labels for ETR_N
elab5 <- paste0("$s_", 2:upperb5, "/s_", 1:(upperb5-1), "$")
names(ETR_N5) <- elab5

# Print results
S_N5
ETR_N5

# Display the results in a table
knitr::kable(S_N5, col.names = c("'000s"), digits = 4,
             caption = 'Entry thresholds for Model 5',
             booktabs = TRUE)

# Optionally, display the ETR_N5 in a table as well
knitr::kable(ETR_N5, col.names = c("ETR"), digits = 4,
             caption = 'Entry Threshold Ratios for Model 5',
             booktabs = TRUE)



## Model 6

# Fit the model with the specified predictors
model_6 <- polr(Number_of_stores ~ log_s + log(Dist_nearest) + Grensehandel + n_stays + Monthly_salary,
                data = br_data, method = "probit")

summary(model_6)

# Extract coefficients and cutoffs
lambda6 <- model_6$coefficients # Estimates for log_s, log(Dist_nearest), Grensehandel, n_stays, and Monthly_salary
theta6 <- model_6$zeta # Cutoffs

# Compute S_N using the sample means of all predictors
X_bar6 <- colMeans(br_data[, c("log_s", "Dist_nearest", "Grensehandel", "n_stays", "Monthly_salary")])
S_N6 <- exp(theta6 - X_bar6 %*% lambda6)

# Create labels for S_N
upperb6 <- length(theta6) # Number of thresholds
slab6 <- paste0("$S_", 1:upperb6, "$")
names(S_N6) <- slab6

# Compute ETR_N using the cutoffs
ETR_N6 <- exp(theta6[2:upperb6] - theta6[1:(upperb6-1)]) * (1:(upperb6-1)) / (2:upperb6)

# Create labels for ETR_N
elab6 <- paste0("$s_", 2:upperb6, "/s_", 1:(upperb6-1), "$")
names(ETR_N6) <- elab6

# Display the results in a table
knitr::kable(S_N6, col.names = c("'000s"), digits = 4,
             caption = 'Entry thresholds for Model 6',
             booktabs = TRUE)

# Optionally, display the ETR_N6 in a table as well
knitr::kable(ETR_N6, col.names = c("ETR"), digits = 4,
             caption = 'Entry Threshold Ratios for Model 6',
             booktabs = TRUE)


## Model 7

# Fit the model with the specified predictors
model_7 <- polr(Number_of_stores ~ log_s + n_stays,
                data = br_data, method = "probit")

summary(model_7)