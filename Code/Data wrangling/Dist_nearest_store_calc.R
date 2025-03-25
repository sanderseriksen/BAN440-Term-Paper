# -----------------------------
# Setup: Load Packages
# -----------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(geosphere)

# -----------------------------
# 1. Load and Prepare Data
# -----------------------------
# Load Vinmonopolet + municipality dataset
data <- read_excel("final_data_mun.xlsx")

# Load pre-cleaned municipality admin center coordinates
admin_centers_final <- readRDS("admin_centers_final.rds")

# Ensure join columns match in type
data <- data %>%
  mutate(Municipality_Code = as.character(Municipality_Code))

admin_centers_final <- admin_centers_final %>%
  mutate(kommunenummer = as.character(kommunenummer))

# -----------------------------
# 2. Merge Coordinates
# -----------------------------
# Merge admin center lat/lon into dataset by municipality
data <- left_join(data, admin_centers_final, by = c("Municipality_Code" = "kommunenummer"))

# Overwrite old coordinates with admin center coordinates
data <- data %>%
  mutate(
    Latitude = as.numeric(lat),
    Longitude = as.numeric(lon)
  )

# -----------------------------
# 3. Parse Store GPS Coordinates
# -----------------------------
# Split store GPS into separate numeric lat/lon
data <- data %>%
  separate(GPS_Coordinates, into = c("store_lat", "store_lon"), sep = ";", convert = TRUE) %>%
  mutate(
    store_lat = as.numeric(store_lat),
    store_lon = as.numeric(store_lon)
  )

# -----------------------------
# 4. Build Store Location Matrix
# -----------------------------
# Extract distinct (lon, lat) of all Vinmonopolet stores
store_locations <- data %>%
  filter(!is.na(store_lon), !is.na(store_lat)) %>%
  select(store_lon, store_lat) %>%
  distinct() %>%
  as.matrix()

# -----------------------------
# 5. Define Distance Function
# -----------------------------
# Compute distance (km) from a point to nearest Vinmonopolet store
min_distance_to_store <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) return(NA)
  muni_coord <- matrix(c(lon, lat), nrow = 1)
  dists <- distHaversine(muni_coord, store_locations)
  return(min(dists) / 1000)  # convert meters to km
}

# -----------------------------
# 6. Calculate Distance for All Municipalities
# -----------------------------
# Use updated Longitude/Latitude columns
data$dist_nearest_store <- mapply(
  min_distance_to_store,
  data$Longitude,
  data$Latitude
)

# -----------------------------
# 7. Optional: Drop Redundant Columns
# -----------------------------
data <- data %>%
  select(
    -lat, -lon, -multikurve, -kommunenavn
  )

# -----------------------------
# 8. Final Checks (Optional)
# -----------------------------
str(data$dist_nearest_store)
summary(data$dist_nearest_store)

# -----------------------------
# 9. Does VInmonopolets 30 km threshold 97% goal work based on our data
# -----------------------------
# 1. Total population (all municipalities)
total_pop <- sum(data$Population, na.rm = TRUE)

# 2. Population in municipalities with distance > 30 km
pop_far_away <- data %>%
  filter(dist_nearest_store > 30) %>%
  summarise(total = sum(Population, na.rm = TRUE)) %>%
  pull(total)

# 3. Share of population far away
share_far_away <- pop_far_away / total_pop

# 4. Share WITH access (within 30 km)
share_within_30km <- 1 - share_far_away

# 5. Print results
cat(sprintf("Share of population within 30 km of a Vinmonopolet: %.2f%%\n", share_within_30km * 100))
cat(sprintf("Target (Vinmonopolet): 97%%\n"))

underserved <- data %>%
  filter(dist_nearest_store > 30) %>%
  select(,Mun_name, Population, dist_nearest_store) %>%
  arrange(desc(dist_nearest_store))

print(underserved, n = 50)

# -----------------------------
# 10. Export the final data to an Excel file
# -----------------------------

library(writexl)
write_xlsx(data, "final_data_mun_dist.xlsx")
# -----------------------------