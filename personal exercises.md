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

