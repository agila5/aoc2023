# First part --------------------------------------------------------------
games <- readLines("data/input02.txt")

sum_valid_ids <- 0
for (game in games) {
  split <- strsplit(game, ":")[[1]]
  id <- readr::parse_number(split[1])
  skip_id <- FALSE
  estrazioni <- strsplit(split[2], ";")[[1]]
  
  for (estrazione in estrazioni) {
    sets <- strsplit(estrazione, ",")[[1]]
    blue <- 0
    red <- 0
    green <- 0
    
    for (set in sets) {
      col <- regmatches(set, regexpr("(red|green|blue)", set))
      num <- readr::parse_number(set)
      
      if (col == "blue") {
        blue <- c(blue, num)
      } else if (col == "red") {
        red <- c(red, num)
      } else if (col == "green") {
        green <- c(green, num)
      } else {
        stop("Error")
      }
    }
    
    if (any(blue > 14) | any(green > 13) | any(red > 12)) {
      skip_id <- TRUE
    } 
  }
  
  
  if (skip_id) next
  sum_valid_ids <- sum_valid_ids + id
}

print(sum_valid_ids)

# Second part -------------------------------------------------------------
rm(list = ls())
games <- readLines("data/input02.txt")

tot_power <- 0
for (game in games) {
  split <- strsplit(game, ":")[[1]]
  id <- readr::parse_number(split[1])
  estrazioni <- strsplit(split[2], ";")[[1]]
  
  blue <- 0
  red <- 0
  green <- 0
  
  for (estrazione in estrazioni) {
    sets <- strsplit(estrazione, ",")[[1]]
    for (set in sets) {
      col <- regmatches(set, regexpr("(red|green|blue)", set))
      num <- readr::parse_number(set)
      
      if (col == "blue") {
        blue <- c(blue, num)
      } else if (col == "red") {
        red <- c(red, num)
      } else if (col == "green") {
        green <- c(green, num)
      } else {
        stop("Error")
      }
    }
  }

  tot_power <- tot_power + max(blue) * max(red) * max(green)
}

print(tot_power)
