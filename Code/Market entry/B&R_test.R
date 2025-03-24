### Bresnahan & Reiss test document ###

# relevant libraries
library(tidyverse)
library(readxl)
library(fastDummies)
library(knitr)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data
Vinmonopolet <- read_excel("final_data_mun.xlsx") %>% 
  select(-c(Store_ID, Store_Status, Postal_Code, GPS_Coordinates, Poststed,
            PostnummerKategoriKode, PostnummerKategori, Region_Code, 
            Municipality_Name)) %>%
  mutate(
    Municipality_Name = Mun_name,
    Region_Name = case_when(
      Region_Name == "AUST-AGDER" ~ "Agder",
      Region_Name == "VEST-AGDER" ~ "Agder",
      Region_Name == "AKERSHUS" ~ "Akershus",
      Region_Name == "OPPLAND" ~ "Innlandet",
      Region_Name == "BUSKERUD" ~ "Buskerud",
      Region_Name == "VESTFOLD" ~ "Vestfold",
      Region_Name == "FINNMARK" ~ "Finnmark",
      Region_Name == "HEDMARK" ~ "Innlandet",
      Region_Name == "M??RE OG ROMSDAL" ~ "M??re og Romsdal",
      Region_Name == "NORDLAND" ~ "Nordland",
      Region_Name == "OSLO" ~ "Oslo",
      Region_Name == "ROGALAND" ~ "Rogaland",
      Region_Name == "TELEMARK" ~ "Telemark",
      Region_Name == "TROMS" ~ "Troms",
      Region_Name == "S??R-TR??NDELAG" ~ "Tr??ndelag",
      Region_Name == "NORD-TR??NDELAG" ~ "Tr??ndelag",
      Region_Name == "SOGN OG FJORDANE" ~ "Vestland",
      Region_Name == "HORDALAND" ~ "Vestland",
      Region_Name == "??STFOLD" ~ "??stfold",
      is.na(Region_Name) & str_starts(Municipality_Code, "03") ~ "Oslo",
      is.na(Region_Name) & str_starts(Municipality_Code, "11") ~ "Rogaland",
      is.na(Region_Name) & str_starts(Municipality_Code, "15") ~ "M??re og Romsdal",
      is.na(Region_Name) & str_starts(Municipality_Code, "18") ~ "Nordland",
      is.na(Region_Name) & str_starts(Municipality_Code, "31") ~ "??stfold",
      is.na(Region_Name) & str_starts(Municipality_Code, "32") ~ "Akershus",
      is.na(Region_Name) & str_starts(Municipality_Code, "33") ~ "Buskerud",
      is.na(Region_Name) & str_starts(Municipality_Code, "34") ~ "Innlandet",
      is.na(Region_Name) & str_starts(Municipality_Code, "39") ~ "Vestfold",
      is.na(Region_Name) & str_starts(Municipality_Code, "40") ~ "Telemark",
      is.na(Region_Name) & str_starts(Municipality_Code, "42") ~ "Agder",
      is.na(Region_Name) & str_starts(Municipality_Code, "46") ~ "Vestland",
      is.na(Region_Name) & str_starts(Municipality_Code, "50") ~ "Tr??ndelag",
      is.na(Region_Name) & str_starts(Municipality_Code, "55") ~ "Troms",
      is.na(Region_Name) & str_starts(Municipality_Code, "56") ~ "Finnmark",
      TRUE ~ Region_Name  # Keep existing Region_Name if no conditions are met
    )
  )

# Aggregating per market data for the Bresnahan & Reiss model
Vinmonopolet_market <- Vinmonopolet %>%
  group_by(Municipality_Code) %>%
  summarise(
    Mun_name = first(Municipality_Name),
    Region_Name = first(Region_Name),
    Population = first(Population),
    Area = first(Area),
    Number_of_stores = sum(`2024` > 0),  # Count non-zero sales
    Sales = sum(`2024`)
  )

# Calculate rho, the raw correlation between Population and number of stores
rho <- cor(Vinmonopolet_market$Population, Vinmonopolet_market$Number_of_stores)

# Consumption per capita, grouped by region
Vinmonopolet_market %>% 
  group_by(Region_Name) %>%
  summarise(
    consumption = sum(Sales) / sum(Population),
    Sales = sum(Sales)
  ) %>% 
  arrange(desc(consumption))

# Filtering data for B&R
br_data <- Vinmonopolet_market %>%
  filter(Population < 150000 & Area > 0 & Population > 0)

# Table of the number of stores per market
table(br_data$Number_of_stores)

# Adding variables to the data
upperb <- 2

br_data <- br_data %>% 
  mutate(
    s = Population / 1000,
    density = Number_of_stores / Area,
    Number_of_stores = as.factor(ifelse(Number_of_stores <= upperb, Number_of_stores, upperb))
  ) %>% 
  dummy_cols(select_columns = "Number_of_stores") %>% 
  mutate_at(vars(starts_with("Number_of_stores")), as.factor)

str(br_data)

# Regression model to test
reg <- lm(as.numeric(Number_of_stores) ~ density, br_data)

summary(reg)

# Fitting the Bresnahan & Reiss model
library(MASS)

model_1 <- polr(Number_of_stores ~ s, data = br_data, method = "probit")

summary(model_1)


model_2 <- polr(Number_of_stores ~ s + density, data = br_data, method = "probit")

summary(model_2)


## Model 1 ##

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
