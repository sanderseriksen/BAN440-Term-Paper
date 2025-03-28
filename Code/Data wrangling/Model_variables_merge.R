### Independent variables merge ###

# relevant libraries
library(tidyverse)
library(readxl)
library(fastDummies)
library(knitr)
library(writexl)

# Set locale to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data
Vinmonopolet <- read_excel("final_data_mun_dist.xlsx") %>% 
  select(-c(Store_ID, Store_Status, Postal_Code, Poststed,
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
      Region_Name == "MØRE OG ROMSDAL" ~ "Møre og Romsdal",
      Region_Name == "NORDLAND" ~ "Nordland",
      Region_Name == "OSLO" ~ "Oslo",
      Region_Name == "ROGALAND" ~ "Rogaland",
      Region_Name == "TELEMARK" ~ "Telemark",
      Region_Name == "TROMS" ~ "Troms",
      Region_Name == "SØR-TRØNDELAG" ~ "Trøndelag",
      Region_Name == "NORD-TRØNDELAG" ~ "Trøndelag",
      Region_Name == "SOGN OG FJORDANE" ~ "Vestland",
      Region_Name == "HORDALAND" ~ "Vestland",
      Region_Name == "ØSTFOLD" ~ "Østfold",
      is.na(Region_Name) & str_starts(Municipality_Code, "03") ~ "Oslo",
      is.na(Region_Name) & str_starts(Municipality_Code, "11") ~ "Rogaland",
      is.na(Region_Name) & str_starts(Municipality_Code, "15") ~ "Møre og Romsdal",
      is.na(Region_Name) & str_starts(Municipality_Code, "18") ~ "Nordland",
      is.na(Region_Name) & str_starts(Municipality_Code, "31") ~ "Østfold",
      is.na(Region_Name) & str_starts(Municipality_Code, "32") ~ "Akershus",
      is.na(Region_Name) & str_starts(Municipality_Code, "33") ~ "Buskerud",
      is.na(Region_Name) & str_starts(Municipality_Code, "34") ~ "Innlandet",
      is.na(Region_Name) & str_starts(Municipality_Code, "39") ~ "Vestfold",
      is.na(Region_Name) & str_starts(Municipality_Code, "40") ~ "Telemark",
      is.na(Region_Name) & str_starts(Municipality_Code, "42") ~ "Agder",
      is.na(Region_Name) & str_starts(Municipality_Code, "46") ~ "Vestland",
      is.na(Region_Name) & str_starts(Municipality_Code, "50") ~ "Trøndelag",
      is.na(Region_Name) & str_starts(Municipality_Code, "55") ~ "Troms",
      is.na(Region_Name) & str_starts(Municipality_Code, "56") ~ "Finnmark",
      TRUE ~ Region_Name  # Keep existing Region_Name if no conditions are met
    )
  ) %>% 
  select(-Mun_name)

# Aggregating per municipality data
Vinmonopolet_market <- Vinmonopolet %>%
  group_by(Municipality_Code) %>%
  summarise(
    Mun_name = first(Municipality_Name),
    Region_Name = first(Region_Name),
    Population = first(Population),
    Area = first(Area),
    Number_of_stores = sum(`2024` > 0),  # Count non-zero sales
    Sales = sum(`2024`),
    Lat = first(Latitude),
    Lon = first(Longitude),
    Dist_nearest = first(dist_nearest_store),
  )


# Scaling the variables that have nt been scaled yet
Vinmonopolet_market <- Vinmonopolet_market %>%
  mutate(Population = Population / 1000,
         Sales = Sales / 1000)

# Now we have loaded and wrangled the main data set, but we can use some
# new variables for our analysis

## Merge 1: Grensehandel ######################################################

# Load the weights datas
weights <- read_excel("Grensehandel_weights.xlsx", skip = 3) %>% 
  slice(1) %>% 
  select(-'...1') %>%
  mutate(
    mean_weight = (as.numeric(`2024K1`) + as.numeric(`2024K2`) + as.numeric(`2024K3`) + as.numeric(`2024K4`)) / 4
  )

weight_grensehandel <- weights$mean_weight / 100

# Load the regional data
regional <- read_excel("Grensehandel_regions.xlsx")

total_grensehandel <- sum(regional$"2024")

# Calculate grensehandel per region
regional <- regional %>% 
  rename(
    Region = `Fylker`,
    Total_sale = `2024`
  ) %>%
  mutate(
    Grensehandel = Total_sale * weight_grensehandel
  )

# Split the "Vestlandet" region row into three new rows: "Rogaland", "Vestland" and "MC8re og Romsdal"
regional <- regional %>%
  rbind(
    regional %>% filter(Region == "Vestlandet") %>% mutate(Region = "Rogaland"),
    regional %>% filter(Region == "Vestlandet") %>% mutate(Region = "Vestland"),
    regional %>% filter(Region == "Vestlandet") %>% mutate(Region = "Møre og Romsdal")
  ) %>%
  filter(Region != "Vestlandet")

# Divide the grensehandel value by three for "Rogaland", "Vestland" and "MC8re og Romsdal"
regional <- regional %>%
  mutate(
    Grensehandel = case_when(
      Region == "Rogaland" ~ Grensehandel * 0.35,
      Region == "Vestland" ~ Grensehandel * 0.46,
      Region == "Møre og Romsdal" ~ Grensehandel * 0.19,
      TRUE ~ Grensehandel  # Keep the original value for other regions
    )
  )

# Split the "Nord-Norge" region row into three new rows: "Nordland", "Troms" and "Finnmark"
# And divide the grensehandel value by three
regional <- regional %>%
  mutate(
    Grensehandel = ifelse(Region == "Nord-Norge", Grensehandel / 3, Grensehandel)
  ) %>%
  rbind(
    regional %>% filter(Region == "Nord-Norge") %>% mutate(Region = "Nordland"),
    regional %>% filter(Region == "Nord-Norge") %>% mutate(Region = "Troms"),
    regional %>% filter(Region == "Nord-Norge") %>% mutate(Region = "Finnmark")
  ) %>%
  filter(Region != "Nord-Norge")

# Divide the grensehandel value by three for "Nordland", "Troms" and "Finnmark"
regional <- regional %>%
  mutate(
    Grensehandel = case_when(
      Region == "Nordland" ~ Grensehandel * 0.5,
      Region == "Troms" ~ Grensehandel * 0.35,
      Region == "Finnmark" ~ Grensehandel * 0.15,
      TRUE ~ Grensehandel  # Keep the original value for other regions
    )
  )

# Split the "Agder, Telemark, Buskerud og Vestfold" column into four new columns: "Agder", "Telemark", "Buskerud" and "Vestfold"
# And divide the grensehandel value by four
regional <- regional %>%
  mutate(
    Grensehandel = ifelse(Region == "Agder, Telemark, Buskerud og Vestfold", Grensehandel / 4, Grensehandel)
  ) %>%
  rbind(
    regional %>% filter(Region == "Agder, Telemark, Buskerud og Vestfold") %>% mutate(Region = "Agder"),
    regional %>% filter(Region == "Agder, Telemark, Buskerud og Vestfold") %>% mutate(Region = "Telemark"),
    regional %>% filter(Region == "Agder, Telemark, Buskerud og Vestfold") %>% mutate(Region = "Buskerud"),
    regional %>% filter(Region == "Agder, Telemark, Buskerud og Vestfold") %>% mutate(Region = "Vestfold")
  ) %>%
  filter(Region != "Agder, Telemark, Buskerud og Vestfold")

# Divide the grensehandel value by four for "Agder", "Telemark", "Buskerud" and "Vestfold"
regional <- regional %>%
  mutate(
    Grensehandel = case_when(
      Region == "Agder" ~ Grensehandel * 0.31,
      Region == "Telemark" ~ Grensehandel * 0.17,
      Region == "Buskerud" ~ Grensehandel * 0.26,
      Region == "Vestfold" ~ Grensehandel * 0.26,
      TRUE ~ Grensehandel  # Keep the original value for other regions
    )
  )

# Removing the "total_sale" column from the regional data set
regional <- regional %>% select(-Total_sale)

# Merge the regional data with the main data set on Region_Name in the Vinmonopolet_market data set and Region in the regional data set
Vinmonopolet_market <- left_join(Vinmonopolet_market, regional, by = c("Region_Name" = "Region"))

# Add a new column "Region_pop" where "Population" is summarized for each region
Vinmonopolet_market <- Vinmonopolet_market %>%
  group_by(Region_Name) %>%
  mutate(Region_pop = sum(Population)) %>%
  ungroup()

Vinmonopolet_market <- Vinmonopolet_market %>%
  mutate(Kommune_share = Population / Region_pop,
         Grensehandel_mun = Grensehandel * Kommune_share) %>% 
  select(-c("Region_pop", "Kommune_share", "Grensehandel")) %>% 
  rename(Grensehandel = Grensehandel_mun)




## Merge 2: Tourism ############################################################

# Reading tourism data
Tourism <- read_excel("Tourism.xlsx", skip = 4) %>%
  rename(
    Mun = '...1',
    H = 'Hotell og liknande overnattingsbedrifter',
    C = 'Campingplassar, hyttegrender og vandrarheim',
  ) %>% 
  select(-'...2') %>%
  mutate_at(vars(H, C), ~as.numeric(str_replace_all(., ":", "0"))) %>% 
  mutate(n_stays = H + C) %>%
  separate(Mun, into = c("Municipality_Code", "Municipality_Name"), sep = " ", remove = FALSE) %>% 
  select(-c("Mun", "H", "C", "Municipality_Name")) %>% 
  filter(!is.na(Municipality_Code))

# Merging the data
Vinmonopolet_market <- left_join(Vinmonopolet_market, Tourism, by = "Municipality_Code") %>% 
  mutate(
    n_stays = ifelse(is.na(n_stays), 0, n_stays),
    n_stays = n_stays / 1000
  )

# There is a great deal of missing data, so we do not know the relevance of 
# this data yet


## Merge 3: Income #############################################################

# Average monthly salary per inhabitant in the municipality

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
  mutate(Monthly_salary = as.numeric(Monthly_salary),
         Monthly_salary = Monthly_salary / 1000)

# Merge with the main data set
Vinmonopolet_market <- left_join(Vinmonopolet_market, clean_data, by = "Municipality_Code")





## Merge 4: Concentration ######################################################

# Load data
concentration <- read_excel("Concentration.xlsx", skip = 5) %>% 
  slice(1:357) %>% 
  select('...1',
         'Spredtbygd strøk...3') %>%
  rename(Mun = '...1',
         Spread = 'Spredtbygd strøk...3') %>% 
  separate(Mun, into = c("Municipality_Code", "Municipality_Name"), sep = " ", remove = FALSE) %>% 
  select(-c("Municipality_Name", "Mun")) %>% 
  mutate(Spread = as.numeric(Spread),
         Spread = Spread / 1000)

# Remove the first two characters of each cell in the "Municpality_Code" column
concentration$Municipality_Code <- substr(concentration$Municipality_Code, 3, nchar(concentration$Municipality_Code))

# Merge with the main data set
Vinmonopolet_market <- left_join(Vinmonopolet_market, concentration, by = "Municipality_Code")



# Write to Excel
write_xlsx(Vinmonopolet_market, "demand_data.xlsx")

