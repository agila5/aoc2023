# First part --------------------------------------------------------------
input <- readLines("data/input06.txt")
times <- regmatches(input[1], gregexpr("\\d+", input[1]))[[1]] |> as.integer()
distances <- regmatches(input[2], gregexpr("\\d+", input[2]))[[1]] |> as.integer()

count <- integer(length = length(times))
for (i in seq_along(count)) {
  time <- times[i]
  distance <- distances[i]
  for (t in 0:time) {
    if (t * (time - t) > distance) {
      count[i] <- count[i] + 1L
    }
  }
}
prod(count)

# Second part -------------------------------------------------------------
rm(list = ls())
library(Rcpp)
input <- readLines("data/input06.txt")
max_time <- regmatches(input[1], gregexpr("\\d+", input[1]))[[1]] |> paste(collapse = "") |> as.integer()
distance <- regmatches(input[2], gregexpr("\\d+", input[2]))[[1]] |> paste(collapse = "") |> as.numeric()

cppFunction(
  code = '
  int aoc(size_t max_time, double distance) {
    int count = 0; 
    for (size_t t = 0; t <= max_time; t++) {
      if (t * (max_time - t) >= distance) {
        count++; 
      }
    }
    return count; 
  }', 
  verbose = TRUE
)
aoc(max_time, distance)
