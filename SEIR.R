
set.seed(0)
n<- rep(1:200,5); sample(n, 1000, replace = T)

n


# 1. Create a sequence of unique numbers (e.g., from 1 to 200).
unique_values <- 1:200

# 2. Repeat each unique number 5 times. This results in a vector of length 1000.
repeated_values <- rep(unique_values, each = 5)

# 3. Randomly shuffle the order of the 1000 numbers.
random_numbers <- sample(repeated_values, size = 1000, replace = FALSE)
