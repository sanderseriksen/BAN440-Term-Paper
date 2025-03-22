### Bresnahan & Reiss test document ###

# relevant libraries
library(tidyverse)
library(readxl)

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

# Table of the number of stores per market
table(Vinmonopolet_market$Number_of_stores)

# Calculate rho, the raw correlation between Population and number of stores
rho <- cor(Vinmonopolet_market$Population, Vinmonopolet_market$Number_of_stores)



