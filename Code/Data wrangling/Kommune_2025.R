### Municipality area and population ###

# Importing libraries
library(tidyverse)
library(readxl)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Read data
kommune_data <- read_excel("Kommune_data.xlsx", skip = 3) %>% 
  rename("Municipality" = "...1",
         "Population" = "2025...2",
         "Area" = "2025...3") %>% 
  separate(Municipality, into = c("Mun_num", "Mun_name"), sep = " ") %>% 
  filter(Population != 0,
         Area != 0) %>% 
  mutate(Population = as.numeric(Population),
         Area = as.numeric(Area))

