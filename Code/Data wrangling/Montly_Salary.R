### Monthly salary per inhabitant ###

# Necessary packages
library(readxl)
library(dplyr)

# Load data
data <- read_excel("Monthly_Salary.xlsx")

# Cleaning data by removing rows with missing values and rows with dots 
clean_data <- data %>%
  filter(!apply(., 1, function(row) any(grepl("\\.", row)))) %>%
  na.omit()

# Remove the last two rows from the data, using tidyverse
clean_data <- clean_data %>% 
  slice(1:(n() - 2)) %>%
  select(-'...2') %>% 
  rename(
    Mun = `12852: Kommunefordelt månedslønn, etter region, statistikkmål, statistikkvariabel, år og arbeidssted/bosted`,
    Monthly_salary = '...3'
  ) %>% 
  separate(Mun, into = c("Municipality_Code", "Municipality_Name"), sep = " ", remove = FALSE) %>% 
  select(-c("Municipality_Name", "Mun")) %>%
  mutate(Monthly_salary = as.numeric(Monthly_salary))

summary(clean_data)