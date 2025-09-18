library(httr)
library(jsonlite)
library(dplyr)
library(mapgl)


url <- "https://api.purpleair.com/v1/sensors"
response <- GET(
  url,
  add_headers("X-API-Key" = "47BA01B7-6F08-11F0-AF66-42010A800028"),
  query = list(
    fields = "sensor_index,name,latitude,longitude,location_type"
  )
)

sensors_data <- fromJSON(content(response, as = "text"))$data
sensor_columns <- fromJSON(content(response, as = "text"))$fields
sensors_df <- as.data.frame(sensors_data)
names(sensors_df) <- sensor_columns

sensors_df <- sensors_df %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    location_type = as.integer(location_type)  # optional: for filtering outdoor only
  )

# Filter to Hawai‘i (approximate bounding box)
hawaii_sensors <- sensors_df %>%
  filter(latitude >= 18 & latitude <= 23,
         longitude <= -154 & longitude >= -162)
head(hawaii_sensors)



library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)

# Your API key
api_key <- "47BA01B7-6F08-11F0-AF66-42010A800028"

# Vector of sensor indexes (e.g. from your previous hawaii_sensors dataframe)
hawaii_sensor_ids <- hawaii_sensors$sensor_index

# Function to pull latest PM2.5 + metadata from each sensor
get_latest_pm25_cf1 <- function(sensor_index) {
  url <- paste0("https://api.purpleair.com/v1/sensors/", sensor_index)
  
  response <- GET(
    url,
    add_headers("X-API-Key" = api_key),
    query = list(fields = "pm2.5_cf_1,humidity,name,latitude,longitude,confidence,confidence_auto,confidence_manual")
  )
  
  if (response$status_code != 200) {
    message("Sensor ", sensor_index, " failed with status ", response$status_code)
    return(NULL)
  }
  
  content <- fromJSON(content(response, as = "text"))
  
  tibble(
    sensor_index = sensor_index,
    name = content$sensor$name,
    pm25_cf_1 = content$sensor$`pm2.5_cf_1`,
    humidity = content$sensor$humidity,
    latitude = content$sensor$latitude,
    longitude = content$sensor$longitude,
    timestamp = as.POSIXct("2020-09-15", tz = "UTC")
  )
}

# Run over all sensors and collect results
pm_data <- map_dfr(hawaii_sensor_ids, get_latest_pm25_cf1)

# Preview
print(pm_data)





# Save or append to log
log_file <- "purpleair_pm25cf1_log.csv"
if (file.exists(log_file)) {
  write_csv(pm_data, log_file, append = TRUE)
} else {
  write_csv(pm_data, log_file)
  

}



## Mapping
library(mapgl)
library(sf)
library(viridisLite)
library(tidyverse)
library(here)


pm_data <- read_csv(here("purpleair_pm25cf1_log.csv"))

# Convert to sf object
pm_sf <- pm_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Create color palette for PM2.5 (e.g., up to 50 µg/m³)
pm_palette <- interpolate_palette(
  data = pm_data,
  column = "pm25_cf_1",
  method = "equal",
  n = 6,
  palette = magma
)

# Build the map
# Step 2: Build the interactive map
mapboxgl(bounds = pm_sf) |>
  add_circle_layer(
    id = "hawaii_pm25",
    source = pm_sf,
    circle_color = pm_palette$expression,
    circle_radius = 6,
    circle_opacity = 0.8,
    popup = c("name", "pm25_cf_1", "timestamp"),
    tooltip = "pm25_cf_1",
    hover_options = list(
      circle_color = "black",
      circle_radius = 8
    )
  ) |>
  add_legend(
    legend_title = "PM2.5 (µg/m³)",
    values = pm_palette$values,
    colors = pm_palette$colors,
    type = "continuous"
  )




#### hourly data

get_pm25_data_hourly <- function(sensor_index, api_key) {
  url <- "https://api.purpleair.com/v1/sensors/timeseries"
  
  start_time <- as.numeric(as.POSIXct("2025-07-01 00:00:00", tz = "UTC"))
  end_time <- as.numeric(as.POSIXct("2025-07-07 23:59:59", tz = "UTC"))
  
  response <- httr::GET(
    url,
    httr::add_headers("X-API-Key" = api_key),
    query = list(
      sensor_index = sensor_index,
      start_timestamp = start_time,
      end_timestamp = end_time,
      average = 60,
      fields = "pm2.5_cf_1"
    )
  )
  
  res <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  
  if (!"data" %in% names(res)) {
    message("No data returned for sensor ", sensor_index)
    return(NULL)
  }
  
  df <- as.data.frame(res$data)
  colnames(df) <- res$fields
  
  # Use the returned timestamps if available
  if (!is.null(res$timestamps)) {
    df$timestamp <- as.POSIXct(res$timestamps, origin = "1970-01-01", tz = "UTC")
  }
  
  df$sensor_index <- sensor_index
  return(df)
}

pm_data <- get_pm25_data_hourly("279394", api_key = "47BA01B7-6F08-11F0-AF66-42010A800028")
