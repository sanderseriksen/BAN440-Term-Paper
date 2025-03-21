### Bresnahan & Reiss test document ###

# relevant libraries
library(tidyverse)
library(readxl)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data
Vinmonopolet_raw <- read_excel("final_data_mun.xlsx")

# This data has a couple of unnecessart columns, so we will remove them
# Additionally, we will clean the data where necessary

# Cleaning data
Vinmonopolet <- Vinmonopolet_raw %>% 
  select(-c(Store_ID, Store_Status, Postal_Code, GPS_Coordinates, Poststed,
            PostnummerKategoriKode, PostnummerKategori, Region_Code, Mun_name)) %>%
  mutate(
    Region_Name = case_when(
      Region_Name == "AUST-AGDER" ~ "Agder",
      Region_Name == "VEST-AGDER" ~ "Agder",
      Region_Name == "AKERSHUS" ~ "Akershus",
      Region_Name == "OPPLAND" ~ "Innlandet",
      Region_Name == "BUSKERUD" ~ "Buskerud",
      Region_Name == "VESTFOLD" ~ "Vestfold",
      Region_Name == "FINNMARK" ~ "Finnmark",
      Region_Name == "HEDMARK" ~ "Innlandet",
      Region_Name == "M??RE OG ROMSDAL" ~ "M??re of Romsdal",
      Region_Name == "NORDLAND" ~ "Nordland",
      Region_Name == "OSLO" ~ "Oslo",
      Region_Name == "ROGALAND" ~ "Rogaland",
      Region_Name == "TELEMARK" ~ "Telemark",
      Region_Name == "TROMS" ~ "Troms",
      Region_Name == "S??R-TR??NDELAG" ~ "Tr??ndelag",
      Region_Name == "NORD-TR??NDELAG" ~ "Tr??ndelag",
      Region_Name == "SOGN OG FJORDANE" ~ "Vestland",
      Region_Name == "HORDALAND" ~ "Vestland",
      Region_Name == "??STFOLD" ~ "??stfold"
    )
  )

