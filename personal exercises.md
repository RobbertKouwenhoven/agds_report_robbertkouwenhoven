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

