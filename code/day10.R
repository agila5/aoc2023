# First part --------------------------------------------------------------
library(igraph)
input <- readLines("data/input10_small.txt")
input <- do.call(rbind, lapply(input, \(x) strsplit(x, "")[[1]]))

# Fix S
which(input == "S", arr.ind = TRUE)
input[1:3, 1:3]
input[2, 2] <- "F"
?graph_from_adj_list

input_adj_matrix <- matrix(0, nrow = nrow(input), ncol = ncol(input))
for (i in seq_len(nrow(input))) {
  for (j in seq_len(ncol(input))) {
    if (input[i, j] == ".") {
      next
    } else if (input[i, j] == "|") {
      input_adj_matrix[c(i - 1, i + 1), j] <- 1
    }
  }
}
