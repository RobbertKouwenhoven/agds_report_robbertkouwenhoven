library(targets)
library(MODISTools)
library(dplyr)
library(ggplot2)

# Set target options
tar_option_set(packages = c("MODISTools", "dplyr", "ggplot2"))

# Define functions
fetch_modis_data <- function(lat, lon, year = 2020) {
  mt_subset(
    product = "MCD12Q1",
    band = "LC_Type1",
    lat = lat,
    lon = lon,
    start = year,
    end = year,
    site_name = paste0("Point_", lat, "_", lon),
    km_lr = 0,
    km_ab = 0
  )
}

# Define targets
list(
  tar_target(
    grid_points,
    expand.grid(lat = seq(50.5, 51.5, length.out = 5),
                lon = seq(3.5, 5.5, length.out = 5))
  ),
  
  tar_target(
    modis_data,
    bind_rows(lapply(1:nrow(grid_points), function(i) {
      fetch_modis_data(grid_points$lat[i], grid_points$lon[i])
    }))
  ),
  
  tar_target(
    plot_landcover,
    {
      modis_data <- modis_data %>%
        mutate(latitude = as.numeric(latitude), 
               longitude = as.numeric(longitude), 
               value = as.factor(value))
      
      ggplot(modis_data, aes(x = longitude, y = latitude, fill = value)) +
        geom_tile() +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = "MODIS Land Cover Classification (2020)", fill = "Land Cover Type")
    }
  )
)

library(targets)
tar_make()  # Execute the pipeline
