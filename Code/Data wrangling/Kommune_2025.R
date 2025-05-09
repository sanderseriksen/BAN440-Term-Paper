### Municipality area and population ###

# Importing libraries
library(tidyverse)
library(readxl)
library(writexl)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Read data for total population and area of each municipality
kommune_data <- read_excel("Kommune_data.xlsx", skip = 3) %>%
  rename("Municipality" = "...1",
         "Population" = "2025...2",
         "Area" = "2025...3") %>%
  separate(Municipality, into = c("Mun_num", "Mun_name"), sep = " ", extra = "merge", fill = "right") %>%
  filter(Population != 0,
         Area != 0) %>%
  mutate(Population = as.numeric(Population),
         Area = as.numeric(Area))

# Read data for demographic data
demographic_data <- read_excel("Kommune_demo.xlsx", skip = 4) %>%
  rename("Municipality" = "...1",
         "0-17" = "0-17 ??r",
         "18+" = "18 ??r eller eldre") %>%
  filter(if_all(everything(), ~ !is.na(.) & . != 0)) %>% # Remove rows with NA or 0 in any column
  separate(Municipality, into = c("Mun_num", "Mun_name"), sep = " ", extra = "merge", fill = "right") %>%
  separate(Mun_num, into = c("K", "Mun_num"), sep = "-") %>%
  select(-"K",
         -"Mun_name")

# Merge the two datasets
kommune_data_final <- kommune_data %>%
  left_join(demographic_data, by = c("Mun_num"))

# Write data to Excel
write_xlsx(kommune_data_final, "Kommune_data_final.xlsx")