# First part --------------------------------------------------------------
input <- readLines("data/input05.txt")
input_split <- split(input, cumsum(input == ""))

match_input_map <- function(input, maps) {
  # Tolgo spazio iniziale e nome mappa
  maps <- maps[-c(1, 2)]
  
  # Divido i tre pezzi in ogni parte di mappa + to integer
  maps <- strsplit(maps, " ") |> lapply(as.numeric)
  
  # Costruisco le sequenze 
  maps <- lapply(
    maps, 
    function(x) {
      seq <- x[1]:(x[1] + x[3] - 1)
      attr(seq, "min_names") <- x[2]
      attr(seq, "max_names") <- (x[2] + x[3] - 1)
      seq
    }
  )
  
  # Match
  out <- NA
  for (map in maps) {
    min_names <- attr(map, "min_names")
    max_names <- attr(map, "max_names")
    if (!dplyr::between(as.numeric(input), left = min_names, right = max_names)) {
      next
    }
    dist <- as.numeric(input) - min_names + 1
    out <- map[dist]
  }
  
  if (is.na(out)) {
    out <- input
  }
  
  as.character(out)
}

# Process seeds
seeds <- input_split[[1]]
seeds <- regmatches(seeds, gregexpr("\\d+", seeds))[[1]]

min_location <- Inf
for (seed in seeds) {
  # Processing
  soil <- match_input_map(seed, input_split[[2]])
  fertilizer <- match_input_map(soil, input_split[[3]])
  water <- match_input_map(fertilizer, input_split[[4]])
  light <- match_input_map(water, input_split[[5]])
  temperature <- match_input_map(light, input_split[[6]])
  humidity <- match_input_map(temperature, input_split[[7]])
  min_location <- min(min_location, as.numeric(match_input_map(humidity, input_split[[8]])))
}
min_location

# Second part -------------------------------------------------------------
rm(list = ls())
input <- readLines("data/input05.txt")
input_split <- split(input, cumsum(input == ""))
match_input_map_v2 <- function(input, maps) {
  
  
  # Tolgo spazio iniziale e nome mappa
  maps <- maps[-c(1, 2)]
  
  # Divido i tre pezzi in ogni parte di mappa + to integer
  maps <- strsplit(maps, " ") |> lapply(as.numeric)
  
  # Costruisco le sequenze 
  maps <- lapply(
    maps, 
    function(x) {
      seq <- x[1]:(x[1] + x[3] - 1)
      attr(seq, "min_names") <- x[2] 
      attr(seq, "max_names") <- x[2] + x[3] - 1
      seq
    }
  )
  
  # Match
  out <- rep(NA_real_, length(input))
  for (map in maps) {
    gc(verbose = FALSE)
    min_names <- attr(map, "min_names")
    max_names <- attr(map, "max_names")
    is_between <- input[1] >= min_names & input[length(input)] <= max_names
    if (!any(is_between)) {
      next
    }
    is_between <- input >= min_names & input <= max_names
    out[is_between] <- map[input[is_between] - min_names + 1]
  }
  rm(min_names, max_names, is_between)
  gc(verbose = FALSE)
  
  if (anyNA(out)) {
    out[is.na(out)] <- input[is.na(out)]
  }
  
  out
}

seeds <- input_split[[1]]
seeds <- regmatches(seeds, gregexpr("\\d+", seeds))[[1]]

min_location <- Inf
for (i in seq(1, length(seeds) - 1, by = 2)) {
  min_range <- as.numeric(seeds[i])
  length <- as.numeric(seeds[i + 1L])
  seed <- min_range:(min_range + length - 1L)
  soil <- match_input_map_v2(seed, input_split[[2]]); rm(seed); gc(verbose = FALSE)
  fertilizer <- match_input_map_v2(soil, input_split[[3]]); rm(soil); gc()
  water <- match_input_map_v2(fertilizer, input_split[[4]]); rm(fertilizer); gc()
  print(paste0(i, " - light"))
  light <- match_input_map_v2(water, input_split[[5]]); rm(water); gc()
  temperature <- match_input_map_v2(light, input_split[[6]]); rm(light); gc()
  humidity <- match_input_map_v2(temperature, input_split[[7]]); rm(temperature); gc()
  min_location <- min(min_location, min(match_input_map_v2(humidity, input_split[[8]])))
  print(paste0(i, " - end"))
}

print(min_location)
