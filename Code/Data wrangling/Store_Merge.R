### Vinmonopolet merge ###

# Importing libraries
library(tidyverse)
library(readxl)
library(stringr)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Read the "Kommune_matched" file from the defined file path
kommune_data <- read_excel("Kommune_matched.xlsx", sheet = "Store")

# Number of unique observations for each value in the "Kommune" column
kommune_data %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  arrange(desc(n))