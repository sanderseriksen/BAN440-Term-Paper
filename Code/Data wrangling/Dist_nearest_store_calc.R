
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

# Load required packages
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(tidyr)       # For splitting columns
library(geosphere)   # For calculating geographical distances using lat/lon

# ---------------------------
# STEP 1: Load dataset
# ---------------------------

# Read merged dataset with both Vinmonopolet store info and municipality info
data <- read_excel("final_data_mun.xlsx")

# ---------------------------
# STEP 2: Parse store coordinates
# ---------------------------

# GPS_Coordinates column contains both latitude and longitude as a string separated by ";"
# We split this into two separate numeric columns: store_lat and store_lon

data <- data %>%
  separate(GPS_Coordinates, into = c("store_lat", "store_lon"), sep = ";", convert = TRUE) %>%
  mutate(
    store_lat = as.numeric(store_lat),   # ensure store latitude is numeric
    store_lon = as.numeric(store_lon)    # ensure store longitude is numeric
  )


# -----------------------------
# 4. Build Store Location Matrix
# -----------------------------
# Extract distinct (lon, lat) of all Vinmonopolet stores
store_locations <- data %>%
  filter(!is.na(store_lon), !is.na(store_lat)) %>%

# ---------------------------
# STEP 3: Ensure municipality center coordinates are numeric
# ---------------------------

# These are already separate in the dataset, but stored as characters — we convert them
data <- data %>%
  mutate(
    Longitude = as.numeric(Longitude),  # longitude of the municipality center
    Latitude = as.numeric(Latitude)     # latitude of the municipality center
  )

# ---------------------------
# STEP 4: Extract store coordinates for distance calculation
# ---------------------------

# We only want to use valid store locations for calculating distances
# (some rows in the dataset are just municipality data with no store info)
store_data <- data %>%
  filter(!is.na(store_lat), !is.na(store_lon))

# Extract a unique matrix of all Vinmonopolet store locations
# Format required by geosphere is matrix of (longitude, latitude)
store_locations <- store_data %>%

  select(store_lon, store_lat) %>%
  distinct() %>%
  as.matrix()

# ---------------------------
# STEP 5: Define function to calculate distance to nearest store
# ---------------------------

# For a given municipality center (lon, lat), compute distance to nearest store
# Uses Haversine formula (accounts for Earth's curvature)
min_distance_to_store <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) {
    return(NA)  # return NA if municipality coordinates are missing
  }
  muni_coord <- matrix(c(lon, lat), nrow = 1)  # convert to matrix format for geosphere
  dists <- distHaversine(muni_coord, store_locations)  # distances in meters
  return(min(dists) / 1000)  # convert to kilometers
}

# ---------------------------
# STEP 6: Apply distance function to each municipality
# ---------------------------

# For each row (i.e., each municipality center), calculate distance to closest Vinmonopolet store
# Note: This includes all rows (even ones without a store)

data$dist_nearest_store <- mapply(
  min_distance_to_store,
  data$Longitude,
  data$Latitude
)


# ---------------------------
# STEP 7: Quick check (optional)
# ---------------------------

# Check that coordinates are numeric
str(data$Longitude)
str(data$Latitude)


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