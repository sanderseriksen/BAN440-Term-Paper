### Merging demography with the final data ###

# Loading necessary libraries
library(tidyverse)
library(readxl)
library(writexl)

# Loading the final data
final_data <- read_excel("final_data.xlsx")

# Transforming to normal characters
final_data$Municipality_Name <- iconv(final_data$Municipality_Name, from = "UTF-8", to = "ASCII//TRANSLIT")
final_data$Municipality_Name <- trimws(final_data$Municipality_Name)

# Standardize store names to improve matching
final_data <- final_data %>%
  mutate(Municipality_Name = str_trim(str_to_lower(Municipality_Name)))  # Trim spaces and convert to lowercase

# Loading the kommune data
kommune_data <- read_excel("kommune_data_final.xlsx")

# Standardize the kommune data
kommune_data <- kommune_data %>%
  mutate(Mun_name = iconv(Mun_name, from = "UTF-8", to = "ASCII//TRANSLIT"),
         Mun_name = str_trim(str_to_lower(Mun_name)))  # Trim spaces and convert to lowercase

# Joining the final data with the kommune data
final_data <- final_data %>%
  left_join(kommune_data, by = c("Municipality_Name" = "Mun_name"))