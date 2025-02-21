# First part --------------------------------------------------------------
input <- c(
  "32T3K 765",
  "T55J5 684",
  "KK677 28",
  "KTJJT 220",
  "QQQJA 483"
)
input <- strsplit(input, " ", fixed = TRUE) |> do.call(rbind, args = _)
cards <- input[, 1] |> strsplit(split = "")
bids <- input[, 2] |> as.numeric()

# rank_cards <- function(x) {
x <- cards
  x_table <- lapply(x, table)
  x_table_lengths <- lengths(x_table)
  
  switch(
    
  )
# }
ù

sort
