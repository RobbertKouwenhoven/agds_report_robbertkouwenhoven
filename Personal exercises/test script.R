# Calculate the sum of 1, 2, and 3
1 + 2 + 3
var_1 <- 5
var_1
class(var_1)
var_2 <- as.integer(5)
class(var_2)
var_3 <- as.character(var_2)

x <- c(1, 2, 3, 4, 5) # C stands for concatenate
x[2] # First index is number 1! (not 0 as in python!!)
x[1:3]

x <- c(1, 2, 3)
y <- c(4, 5, 6)
x + y
z <- c(x, y, "seven", "eight")
z
x > 2

seq(from = 0, to = 10, by = 2)
x[x > 1]
which(y > 4)
which.min(y)
y[-2]
