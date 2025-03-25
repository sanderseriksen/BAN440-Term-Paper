library(readxl)
library(dplyr)
library(tidyr)
library(geosphere)

# Load the full dataset
data <- read_excel("final_data_mun.xlsx")

# Split GPS_Coordinates
data <- data %>%
  separate(GPS_Coordinates, into = c("store_lat", "store_lon"), sep = ";", convert = TRUE) %>%
  mutate(
    store_lat = as.numeric(store_lat),
    store_lon = as.numeric(store_lon)
  )

data <- data %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )

# Only rows that are valid Vinmonopolet stores
store_data <- data %>%
  filter(!is.na(store_lat), !is.na(store_lon))

store_locations <- store_data %>%
  select(store_lon, store_lat) %>%
  distinct() %>%
  as.matrix()
# Now apply the distance function over all municipality centers
min_distance_to_store <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) {
    return(NA)
  }
  muni_coord <- matrix(c(lon, lat), nrow = 1)
  dists <- distHaversine(muni_coord, store_locations)
  return(min(dists)/1000)  # km
}

# Apply to municipality coordinates
data$dist_nearest_store <- mapply(
  min_distance_to_store,
  data$Longitude,
  data$Latitude
)
str(data$Longitude)
str(data$Latitude)

