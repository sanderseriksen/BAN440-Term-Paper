# Last inn n??dvendige pakker
library(ggplot2)
library(sf)
library(tidyverse)
library(readxl)

# Last inn butikkdata
df <- read_excel("final_data.xlsx")

# Del opp GPS-koordinater i breddegrad og lengdegrad
df <- df %>%
  separate(GPS_Coordinates, into = c("Latitude", "Longitude"), sep = ";", convert = TRUE)

# Konverter til numeriske verdier
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

# Les inn kommunegrenser fra en lokal fil (GeoJSON eller SHP)
kommunegrenser <- st_read("Norgeskart_kommuner.geojson")

# Endre projeksjon til ETRS89 UTM zone 33 (EPSG:25833) for et bedre 2D-kart
kommunegrenser <- st_transform(kommunegrenser, crs = 25833)

# Konverter butikkdata til sf-objekt og transformer koordinater
butikker_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)  # Fra WGS84
butikker_sf <- st_transform(butikker_sf, crs = 25833)  # Konverter til UTM 33N

# Plott kart med kommunegrenser og butikker
ggplot() +
  geom_sf(data = kommunegrenser, fill = NA, color = "black", size = 0.3) +  # Kommunegrenser
  geom_sf(data = butikker_sf, color = "red", size = 2) +  # Butikker
  labs(title = "Shops in Norway") +
  theme_minimal()
