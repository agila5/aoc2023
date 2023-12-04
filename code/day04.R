# First part --------------------------------------------------------------
cards <- readLines("data/input04.txt")
parse_nums <- function(x) {
  x <- strsplit(x, " ")[[1]]
  x[x != ""]
}

sum_points <- 0L
for (card in cards) {
  card <- strsplit(card, ":|\\|")[[1]] |> trimws()
  winning_numbers <- parse_nums(card[2])
  numbers_I_have <- parse_nums(card[3])
  n_match <- sum(numbers_I_have %in% winning_numbers)
  if (n_match) {
    sum_points <- sum_points + 2 ^ (n_match - 1L)
  }
}
print(sum_points)

# Second part -------------------------------------------------------------
cards <- readLines("data/input04.txt")
multiplicities <- rep(1, length(cards))

for (card in cards) {
  card <- strsplit(card, ":|\\|")[[1]] |> trimws()
  id <- regmatches(card[1], regexpr("\\d+", card[1])) |> as.numeric()
  winning_numbers <- parse_nums(card[2])
  numbers_I_have <- parse_nums(card[3])
  n_match <- sum(numbers_I_have %in% winning_numbers)
  
  if (n_match) {
    multiplicities[(id + 1):(id+n_match)] <- 
      multiplicities[(id + 1):(id+n_match)] + rep(multiplicities[id], n_match)
  }
}

sum(multiplicities)
