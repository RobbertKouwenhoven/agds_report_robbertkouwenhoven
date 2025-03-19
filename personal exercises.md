# Exercise 1 - circle

sphere_comp <- function(radius) {
  circumference <- 2*pi*radius
  area <- pi*radius^2
  return(list(circumference = circumference, area = area))
}

print(sphere_comp(5)[1])

# Exercise 2 - vector

x <- seq(from = 0, to = pi, length.out = 5)
print(x)
?seq

# Exercise 3 - Gauss sum
y <- seq(from=1, to=100, by=1)
cum_sum_y <- cumsum(y)
print(cum_sum_y[-1])
print(50*101)

# Exercise 4 - magic trick
magic_trick <- function(x1) {
  x = x1 +1
  x = x*2
  x = x+4
  x = x/2
  return(x-x1)
}

print(magic_trick(8))

# Exercise 5 - vectors
print(datasets::rivers)
?datasets::rivers
class(datasets::rivers)
length(datasets::rivers)
mean(datasets::rivers)      # Mean
median(datasets::rivers)    # Median
min(datasets::rivers)       # Minimum
max(datasets::rivers)       # Maximum
quantile(datasets::rivers, 0.33)  # 33%-quantile

#Exercise 6 - Data frames
print(datasets::quakes)
?datasets::quakes
dim(datasets::quakes) # obtain dimensions
magnitudes <- datasets::quakes$mag #extract the column containing magnitudes
print(magnitudes)
max_magnitude <- max(magnitudes)
print(max_magnitude)
# Find the row index of the maximum magnitude
max_index <- which.max(magnitudes)
# Extract latitude and longitude of that event
epicenter <- datasets::quakes[max_index, c("lat", "long")]
print(epicenter)

#---- Chapter 3 - programming primers
# Exercise 1 - Gauss variations
vector1 <- seq(from=1, to=100, by=1)
cum_sum <- 0
for (idx in seq(length(vector1))){
  cum_sum <- cum_sum + vector1[idx]
}
cum_sum

counter = 1
cum_sum2 <- 0
while (counter <= 100){
  cum_sum2 <- cum_sum2 + vector1[counter]
  counter = counter + 1
}
print(cum_sum2)

multiples <- vector1[vector1 %% 3 == 0 & vector1 %% 7 == 0]
sum_result <- sum(multiples)
cat("The sum of multiples of 3 and 7 within 1-100 is:", sum_result)

# Exercise 2 - nested loops
mymat <- matrix(c(6, 7, 3, NA, 15, 6, 7, 
              NA, 9, 12, 6, 11, NA, 3, 
              9, 4, 7, 3, 21, NA, 6, 
              rep(NA, 7)),
            nrow = 4, byrow = TRUE)
myvec <- c(8, 4, 12, 9, 15, 6)

idx <- 1
idy <- 1
for (idx in seq(nrow(mymat))){
  for (idy in seq(ncol(mymat))){
    if(is.na(mymat[idx,idy])){
      mymat[idx,idy]<-max(myvec)
    }
  }
  myvec <- myvec[!myvec==max(myvec)]
}

print(mymat)

# Exercise 3 - interpolation
# Step 1: Define three vectors
vec1 <- rep(6, 25)   
vec2 <- rep(NA, 41)    
vec3 <- rep(-20, 34)

interpol_vec <- c(vec1,vec2,vec3)
print(interpol_vec)
interp_vec <- approx(seq_along(interpol_vec), interpol_vec, method = "linear", xout = seq_along(interpol_vec))
?approx

print(interp_vec)
# Plot the results
plot(interp_vec, type = "o", col = "blue", pch = 16, main = "Interpolated Values", xlab = "Index", ylab = "Value")

# --- Chapter 4 - Data Wrangling
# Exercise 1 - Star Wars
library(dplyr)
library(stringr)

starwars |>
  filter(homeworld %in% c("Ryloth", "Naboo"), skin_color == "pale") |>
  nrow()

starwars %>% # %>% is also pipe operator, just like |>
  arrange(desc(height)) %>%  # Sort by height in descending order
  slice(1:30) %>%            # Select the tallest 30 characters
  arrange(desc(birth_year)) %>%  # Sort by birth year (oldest first)
  pull(name) %>%             # Extract the name
  first()                    # Get the first (oldest) character

starwars %>%
  filter(grepl("Return of the Jedi", films)) %>%  # Select characters from the film
  arrange(height) %>%  # Sort by height (ascending)
  slice(1) %>%  # Select the smallest character
  select(name, starships)  # Extract name and starships

# Exercise 2 - Aggregating
half_hourly_fluxes <- readr::read_csv("./FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")
half_hourly_fluxes

library(lubridate)

# Convert TIMESTAMP_START to datetime format
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(Datetime = ymd_hm(as.character(TIMESTAMP_START))) %>%
  mutate(Date = as_date(Datetime))  # Extract only the date for daily aggregation

# Check if it worked
glimpse(half_hourly_fluxes)

# Compute daily VPD and 1st + 3rd quantile
daily_vpd <- half_hourly_fluxes %>%
  group_by(Date) %>%
  summarise(
    mean_VPD = mean(VPD_F, na.rm = TRUE),
    q25_VPD = quantile(VPD_F, probs = 0.25, na.rm = TRUE),
    q75_VPD = quantile(VPD_F, probs = 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

# Check result
print(daily_vpd, n = 10)

# Compute quantile thresholds
lower_threshold <- quantile(daily_vpd$mean_VPD, probs = 0.10, na.rm = TRUE)
upper_threshold <- quantile(daily_vpd$mean_VPD, probs = 0.90, na.rm = TRUE)

# Filter days in the lower or upper 10%
filtered_vpd <- daily_vpd %>%
  filter(mean_VPD <= lower_threshold | mean_VPD >= upper_threshold)

# Check result
print(filtered_vpd, n = 10)

summary_stats <- filtered_vpd %>%
  summarise(
    mean_q25 = mean(q25_VPD, na.rm = TRUE),
    mean_q75 = mean(q75_VPD, na.rm = TRUE)
  )

print(summary_stats)

# Exercise 3 - Patterns in data quality
# Convert TIMESTAMP_START to Datetime and extract hour
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(Datetime = ymd_hm(as.character(TIMESTAMP_START)),
         Hour = hour(Datetime))  # Extracts hour (0-23)

nee_quality_per_hour <- half_hourly_fluxes %>%
  group_by(Hour) %>%
  summarise(
    total = n(),
    measured = sum(NEE_VUT_REF_QC == 0, na.rm = TRUE) / total,
    good_qc = sum(NEE_VUT_REF_QC == 1, na.rm = TRUE) / total,
    medium_qc = sum(NEE_VUT_REF_QC == 2, na.rm = TRUE) / total,
    poor_qc = sum(NEE_VUT_REF_QC == 3, na.rm = TRUE) / total
  ) %>%
  ungroup()

# View results
print(nee_quality_per_hour, n = 24)
# --> measurements are best during the day, gap filling more used at dusk/dawn/night

daily_gpp_all <- half_hourly_fluxes %>%
  group_by(Date = as_date(Datetime)) %>%
  summarise(daily_GPP = mean(GPP_NT_VUT_REF, na.rm = TRUE)) %>%
  ungroup()

daily_gpp_measured <- half_hourly_fluxes %>%
  filter(NEE_VUT_REF_QC == 0) %>%  # Keep only measured data
  group_by(Date = as_date(Datetime)) %>%
  summarise(daily_GPP = mean(GPP_NT_VUT_REF, na.rm = TRUE)) %>%
  ungroup()

mean_gpp_all <- mean(daily_gpp_all$daily_GPP, na.rm = TRUE)
mean_gpp_measured <- mean(daily_gpp_measured$daily_GPP, na.rm = TRUE)

print(paste("Mean GPP (all data):", mean_gpp_all))
print(paste("Mean GPP (measured only):", mean_gpp_measured))
# --> Since gap filling mostly during nights (less GPP), decrease in mean

# -- Chapter 5 - data visualization
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)

half_hourly_fluxes <- readr::read_csv("./FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")
half_hourly_fluxes

# Exercise 1 - spurious data
# Identify spurious values (values appearing more than once)
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(spurious = GPP_NT_VUT_REF %in% GPP_NT_VUT_REF[duplicated(GPP_NT_VUT_REF)])

# Convert TIMESTAMP_START to a proper datetime format
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Filter for first year only (2004)
half_hourly_fluxes_2004 <- half_hourly_fluxes %>%
  filter(year(TIMESTAMP_START) == 2004)

# Visualize half-hourly GPP for 2004
ggplot(half_hourly_fluxes_2004, aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF, color = spurious)) +
  geom_line() +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Half-Hourly GPP in 2004", y = "GPP_NT_VUT_REF", color = "Spurious") +
  theme_minimal()

# Aggregate to daily means
daily_fluxes <- half_hourly_fluxes %>%
  mutate(date = as_date(TIMESTAMP_START)) %>%
  group_by(date) %>%
  summarise(
    mean_GPP = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    prop_spurious = mean(spurious, na.rm = TRUE)  # Proportion of spurious values
  )

# Visualize daily GPP with spurious proportion
ggplot(daily_fluxes, aes(x = date, y = mean_GPP, color = prop_spurious)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Daily Mean GPP", y = "Mean GPP_NT_VUT_REF", color = "Prop. Spurious") +
  theme_minimal()

# Exercise 2 - Identifying outliers
# Compute Q1, Q3, and IQR
gpp_stats <- boxplot.stats(half_hourly_fluxes$GPP_NT_VUT_REF)
outlier_values <- gpp_stats$out  # These are the actual outlier values

# Find the row indices of outliers
outlier_indices <- which(half_hourly_fluxes$GPP_NT_VUT_REF %in% outlier_values)

# Create a new column to indicate outliers
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(outlier_raw = ifelse(row_number() %in% outlier_indices, "Outlier", "Normal"))

# Plot GPP_NT_VUT_REF vs SW_IN_F with outliers in red
ggplot(half_hourly_fluxes, aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = outlier_raw)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
  labs(title = "Outliers in GPP_NT_VUT_REF", color = "Outlier Status") +
  theme_minimal()

# step 2 - define outliers based on the residual with a linear regression
# Fit a linear model: GPP_NT_VUT_REF ~ SW_IN_F
model <- lm(GPP_NT_VUT_REF ~ SW_IN_F, data = half_hourly_fluxes)

# Compute residuals
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(residuals = residuals(model))

# Compute the IQR-based threshold for residuals
residual_outlier_indices <- as.integer(names(boxplot.stats(half_hourly_fluxes$residuals, coef = 5)$out))

# Mark outliers based on residuals
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(outlier_residual = ifelse(row_number() %in% residual_outlier_indices, "Outlier", "Normal"))

# Plot GPP_NT_VUT_REF vs SW_IN_F, highlighting residual-based outliers
ggplot(half_hourly_fluxes, aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = outlier_residual)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
  labs(title = "Outliers in GPP_NT_VUT_REF Based on Residuals", color = "Outlier Status") +
  theme_minimal()

# Exercise 3 - visualizing diurnal and seasonal cycles of GPP
# Ensure TIMESTAMP_START is parsed as datetime
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(
    hour = hour(TIMESTAMP_START),  # Extract hour of day
    doy = yday(TIMESTAMP_START)  # Extract day of year
  )

# Compute mean GPP for each (hour, day-of-year) combination
diurnal_seasonal_GPP <- half_hourly_fluxes %>%
  group_by(doy, hour) %>%
  summarise(mean_GPP = mean(GPP_NT_VUT_REF, na.rm = TRUE), .groups = "drop")
diurnal_seasonal_GPP

# Remove rows where mean_GPP is NA or Inf before plotting
diurnal_seasonal_GPP_clean <- diurnal_seasonal_GPP %>%
  filter(!is.na(mean_GPP), is.finite(mean_GPP))

ggplot(diurnal_seasonal_GPP_clean, aes(x = hour, y = doy, fill = mean_GPP)) +
  geom_tile() +  # Heatmap representation
  scale_fill_viridis_c(option = "plasma") +  # Colorblind-friendly colormap
  scale_y_continuous(
    breaks = seq(15, 365, by = 30),  # Start at day 15 to better align with months
    labels = month.abb
  ) +  
  labs(
    title = "Diurnal and Seasonal Cycle of GPP",
    x = "Hour of Day",
    y = "Day of Year",
    fill = "Mean GPP (µmol CO₂ m⁻² s⁻¹)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

#Exercise 4 - trend in carbon dioxide concentrations
co2_timeseries <- readr::read_csv("./co2_mm_mlo.csv", skip=40)
co2_timeseries

# Create a time object by combining the year and month
co2_timeseries$Date <- ymd(paste(co2_timeseries$year, co2_timeseries$month, "15", sep = "-"))

# Visualize the monthly CO2 time series
ggplot(co2_timeseries, aes(x = Date, y = average)) +
  geom_line() +
  labs(
    title = "Monthly CO2 Time Series",
    x = "Date",
    y = "CO2 Concentration (ppm)"
  ) +
  theme_minimal()  

co2_timeseries
compute_running_mean <- function(data, window_size = 12) {
  rollapply(data, width = window_size, FUN = mean, align = "center", fill = NA)
}

# Define a function to compute a running mean
compute_running_mean <- function(data, window_before = 5, window_after = 6) {
  # Create an empty vector to store the results
  running_mean <- numeric(length = nrow(data))
  # Loop through each row to calculate the running mean
  for (i in 1:nrow(data)) {
    start_index <- max(1, i - window_before)
    end_index <- min(nrow(data), i + window_after)
    running_mean[i] <- mean(data$average[start_index:end_index], na.rm = TRUE)
  }
  return(running_mean)
}

# Compute the 12-month running mean with default window size
co2_timeseries$running_mean <- compute_running_mean(co2_timeseries)

# Check the first few rows to ensure the running mean is calculated
print(co2_timeseries)
  
# Plot the monthly CO2 and the 12-month running mean
ggplot(co2_timeseries, aes(x = Date)) +
  geom_line(aes(y = average), color = "blue", alpha = 0.6, size = 1) +  # Monthly data
  geom_line(aes(y = running_mean), color = "red", size = 1.2) +  # Running mean
  labs(
    title = "Monthly CO2 Time Series with 12-Month Running Mean",
    x = "Date",
    y = "CO2 Concentration (ppm)",
    caption = "Blue: Monthly CO2, Red: 12-Month Running Mean"
  ) +
  theme_minimal()

# Chapter 6 - data variety
library(readr)
library(jsonlite)
library(dplyr)
library(raster)
library("terra")
library(hwsdr)
library(terra)

# exercise 1 - files and file formats
url1 <- "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_1.csv"
url2 <- "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_2.csv"
url3 <- "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_3.csv"

data1 <- read_csv(url1)
data2 <- read_csv(url2)
data3 <- read_csv(url3)

# Combine the data into one data frame
combined_data <- bind_rows(data1, data2, data3)

# Save the combined data as a temporary CSV file
temp_csv <- tempfile(fileext = ".csv")
write_csv(combined_data, temp_csv)

# Read the temporary CSV file back in
read_combined_data <- read_csv(temp_csv)
json_file <- "combined_data.json" # Save the data as a JSON file
write_json(read_combined_data, json_file)

# Print the path of the saved JSON file
cat("JSON file saved as:", json_file, "\n")
# Read the JSON file
json_data <- fromJSON("combined_data.json")

# Inspect the JSON data
print(json_data)  

# Exercise 2 - binary files
# This is a netCDF file, which can be opened easiest with the ncdf4 library
# This file contains temperature 2 meters above the surface.
library(ncdf4)

nc_file <- nc_open("./demo_data.nc")
print(nc_file)

# Close the file after inspecting
nc_close(nc_file)

# Read the NetCDF file as a Raster object
raster_data <- raster("./demo_data.nc")

# Write the raster data to a GeoTIFF file
writeRaster(raster_data, "demo_data.tif", format = "GTiff") 

# load the geoTIFF file
raster_data_tif <- raster("./demo_data.tif")
# Inspect the data
print(raster_data_tif)

# Plot the data
plot(raster_data_tif)  
# yes! from the plot I can recognize the alps and the beginning of Italy's boot!

# Exercise 3 - GET
# Set API URL endpoint for the total sand content
url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1247/T_SAND.nc4"

# Formulate the query to pass to httr for Switzerland's extent
query <- list(
  "var" = "T_SAND",
  "south" = 45.8,  # South latitude for Switzerland
  "west" = 5.9,    # West longitude for Switzerland
  "east" = 10.5,   # East longitude for Switzerland
  "north" = 47.8,  # North latitude for Switzerland
  "disableProjSubset" = "on",
  "horizStride" = 1,
  "accept" = "netcdf4"
)

# Download data using the API endpoint and query data
status <- httr::GET(
  url = url,
  query = query,
  httr::write_disk(
    path = file.path(tempdir(), "T_SAND_Switzerland.nc"),
    overwrite = TRUE
  )
)

sand <- terra::rast(file.path(tempdir(), "T_SAND_Switzerland.nc"))
terra::plot(sand) 

# And now let's do the same thing for silt:
url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1247/T_SILT.nc4"

# Formulate the query for topsoil silt content within Switzerland's extent
query <- list(
  "var" = "T_SILT",
  "south" = 45.8,  # South latitude for Switzerland
  "west" = 5.9,    # West longitude for Switzerland
  "east" = 10.5,   # East longitude for Switzerland
  "north" = 47.8,  # North latitude for Switzerland
  "disableProjSubset" = "on",
  "horizStride" = 1,
  "accept" = "netcdf4"
)

# Download data using the API endpoint and query data
status <- httr::GET(
  url = url,
  query = query,
  httr::write_disk(
    path = file.path(tempdir(), "T_SILT_Switzerland.nc"),
    overwrite = TRUE
  )
)

# Load and visualize the data
silt <- terra::rast(file.path(tempdir(), "T_SILT_Switzerland.nc"))
terra::plot(silt)

## Download the same data using {hwsdr} library
ls("package:hwsdr")

hwsd_meta_data

?ws_download

# Download the database (this will save it to the default temp directory)
hwsd_path <- ws_download(ws_path = tempdir(), verbose = TRUE)

# Print the path to confirm the file location
print(hwsd_path)

# List files in the temp directory
list.files(tempdir())

#debugging
file.info(file.path(tempdir(), "T_SAND_Switzerland.nc"))
sand_raster <- try(rast(file.path(tempdir(), "T_SAND_Switzerland.nc")), silent = TRUE)

if (inherits(sand_raster, "try-error")) {
  print("Error: Unable to open T_SAND_Switzerland.nc either.")
} else {
  plot(sand_raster)
}

# MODIS
library(MODISTools)

# Get the list of available MODIS data products
products <- mt_products()

# Count the number of available products
num_products <- nrow(products)
print(paste("Number of available MODIS data products:", num_products))

# Print the first few rows to see what's available
head(products)

# Canton of Bern land cover
# Define the MODIS product for land cover classification
product <- "MCD12Q1"  # MODIS Land Cover Type

# Central coordinate for the canton of Bern
lat_central <- 46.95
lon_central <- 7.45

# Download MODIS land cover data for the canton of Bern
bern_landcover <- mt_subset(
  product = product,
  lat = lat_central,  # Single central latitude
  lon = lon_central,  # Single central longitude
  band = "LC_Type1",  # Land cover classification
  site_name = "Bern",
  km_lr = 75,  # Covers ~150 km horizontally
  km_ab = 100,  # Covers ~200 km vertically
  internal = TRUE
)

# View first rows of data
head(bern_landcover)

