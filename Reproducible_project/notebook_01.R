library(MODISTools)
library(terra)
library(ggplot2)
library(sf)
library(rmarkdown)
library(ggplot2)

products <- mt_products()
print(products)

# Specify the MODIS land cover product (MCD12Q1 is a common choice)
product <- "MCD12Q1"

# Define the approximate central point of Belgium
lat_center <- 50.5
lon_center <- 4.5

# Download MODIS land cover data
belgium_landcover <- mt_subset(
  product = "MCD12Q1",
  band = "LC_Type1",  # Land cover classification
  lat = lat_center,  
  lon = lon_center,
  start = "2012-01-01",
  end = "2012-12-31",
  site_name = "Belgium",
  km_lr = 100,  # 100 km left-right
  km_ab = 100   # 100 km above-below
)

str(belgium_landcover)

count_land_cover <- function(landcover_data) {
  # Convert to a data frame if needed
  lc_df <- as.data.frame(landcover_data)
  
  # Count occurrences of each land cover class
  landcover_counts <- table(lc_df$value)
  
  # Convert to a tidy data frame
  landcover_df <- as.data.frame(landcover_counts)
  colnames(landcover_df) <- c("Land_Cover_Type", "Count")
  
  return(landcover_df)
}

# Apply the function
lc_counts <- count_land_cover(belgium_landcover)
print(lc_counts)

# Convert MODIS data to a raster format for plotting
landcover_raster <- rast(belgium_landcover)

# Plot using terra
plot(landcover_raster, main = "MODIS Land Cover Map of Belgium")

# Optional: Use ggplot2 for a more customized visualization
ggplot(data = as.data.frame(landcover_raster, xy = TRUE)) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(name = "Land Cover Type") +
  labs(title = "MODIS Land Cover Map of Belgium",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

rmarkdown::render("notebook_01.Rmd")

