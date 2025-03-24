library(readxl)
library(dplyr)

data <- read_excel("Monthly_Salary.xlsx")

# Fjerner rader som inneholder "." i noen av kolonnene
clean_data <- data %>%
  filter(!apply(., 1, function(row) any(grepl("\\.", row)))) %>% 
  na.omit()

# Sjekker resultatet
print(clean_data)
