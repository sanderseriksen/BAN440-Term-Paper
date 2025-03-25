# -------------------------------
# Load Required Packages
# -------------------------------
library(sf)       # For reading and handling geospatial data
library(dplyr)    # For data manipulation

# -------------------------------
# Step 1: Load GML File from SSR
# -------------------------------
# This dataset contains geolocated place names in Norway, including municipalities

gml_data_sted <- st_read("Basisdata_0000_Norge_3035_Stedsnavn_GML.gml")

# -------------------------------
# Step 2: Explore Available Place Types
# -------------------------------
# Check what place types are available (e.g., kommune, tettsted, by, etc.)
unique(gml_data_sted$navneobjekttype)

# Count how many features exist for each relevant type
table(gml_data_sted$navneobjekttype[
  gml_data_sted$navneobjekttype %in% c(
    "rådhus", "kommune", "tettsted", "poststed", "by", "statistiskTettsted", "administrativBydel"
  )
])

# -------------------------------
# Step 3: Filter to "kommune" Points Only
# -------------------------------
# This assumes one location per municipality (357 municipalities in Norway)
admin_centers <- gml_data_sted %>%
  filter(navneobjekttype == "kommune")

# -------------------------------
# Step 4: Prepare Admin Center Table
# -------------------------------
# - Convert geometry to POINT
# - Transform CRS to WGS84 (lat/lon)
# - Extract coordinates
# - Flatten kommunenummer/kommunenavn for use

admin_centers_clean <- admin_centers %>%
  st_cast("POINT") %>%                       # Ensure geometry is POINT
  st_transform(4326) %>%                     # Convert from projected CRS to lat/lon
  mutate(
    lon = st_coordinates(.)[, 1],           # Extract longitude
    lat = st_coordinates(.)[, 2],
    kommunenummer = as.character(unlist(`kommune|Kommune|kommunenummer`)),  # Flatten from list-column
    kommunenavn = as.character(unlist(kommunenavn))
  )

# -------------------------------
# Step 5: Final Admin Center Table
# -------------------------------
# Keep only columns needed for merging into main dataset

admin_centers_final <- admin_centers_clean %>%
  select(kommunenummer, kommunenavn, lon, lat)

# Preview result
head(admin_centers_final)


