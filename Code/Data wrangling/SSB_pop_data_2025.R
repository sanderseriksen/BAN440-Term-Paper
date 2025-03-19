# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# Define API URLs
ssb_url_14216 <- "https://data.ssb.no/api/v0/en/table/14216"
ssb_url_05212 <- "https://data.ssb.no/api/v0/en/table/05212"

# Function to fetch data from SSB API
fetch_ssb_data <- function(url, dataset_id) {
  query <- '{
    "query": [
      {"code": "Region", "selection": {"filter": "all", "values": ["*"]}},
      {"code": "Tid", "selection": {"filter": "item", "values": ["2024"]}}
    ],
    "response": {"format": "json-stat2"}
  }'
  
  response <- POST(url, body = query, encode = "json", content_type("application/json"))
  
  if (status_code(response) == 200) {
    print(paste("Successfully retrieved data for dataset:", dataset_id))
    return(fromJSON(content(response, as = "text")))
  } else {
    print(paste("Error fetching dataset:", dataset_id, "Status:", status_code(response)))
    return(NULL)
  }
}

# Fetch data
data_14216 <- fetch_ssb_data(ssb_url_14216, "14216")
data_05212 <- fetch_ssb_data(ssb_url_05212, "05212")

# Function to extract and clean data
extract_ssb_data <- function(ssb_dataset, dataset_name) {
  dim_info <- ssb_dataset$dimension
  values <- ssb_dataset$value  
  
  # Extract dimensions
  regions <- dim_info$Region$category$label
  contents <- dim_info$ContentsCode$category$label
  year <- dim_info$Tid$category$label
  
  regions_df <- tibble(Region_Code = names(regions), Municipality_Name = unname(regions))
  contents_df <- tibble(ContentsCode = names(contents), Contents_Label = unname(contents))
  year_df <- tibble(Year = as.integer(names(year)))
  
  # Expand data to match value length
  expanded_df <- tibble(
    Region_Code = rep(regions_df$Region_Code, each = length(contents_df$ContentsCode)),
    ContentsCode = rep(contents_df$ContentsCode, times = length(regions_df$Region_Code)),
    Year = rep(year_df$Year, times = length(contents_df$ContentsCode) * length(regions_df$Region_Code)),
    Value = values
  )
  
  # Merge descriptive names
  final_df <- expanded_df %>%
    left_join(regions_df, by = "Region_Code") %>%
    left_join(contents_df, by = "ContentsCode")
  
  return(final_df)
}

# Extract datasets
df_14216 <- extract_ssb_data(data_14216, "Population Density")
df_05212 <- extract_ssb_data(data_05212, "Total Population")

# Transform df_14216 to a wider format
df_14216_fixed <- df_14216 %>%
  pivot_wider(names_from = Contents_Label, values_from = Value)

# Transform df_05212 to a wider format
df_05212_cleaned <- df_05212 %>%
  pivot_wider(names_from = Contents_Label, values_from = Value)

# View cleaned datasets
head(df_14216_fixed)
head(df_05212_cleaned)

# Fix df_14216: Pivot so that each Municipality has one row with separate columns for each variable
df_14216_fixed <- df_14216 %>%
  select(-ContentsCode) %>%  # Remove redundant ContentsCode column
  pivot_wider(names_from = Contents_Label, values_from = Value) %>%  # Reshape
  rename(
    Urban_Area_km2 = `Area of urban settlements (km²)`,  
    Urban_Population = `Number of residents`
  )  # Rename columns for clarity

# View fixed dataset
head(df_14216_fixed)

