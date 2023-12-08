# First part --------------------------------------------------------------
input <- readLines("data/input08.txt")
instructions <- strsplit(input[1], "", fixed = TRUE)[[1]]
nodes <- input[-c(1, 2)]
nodes <- do.call(rbind, regmatches(nodes, gregexpr("\\w+", nodes)))

library(Rcpp)
sourceCpp(file = "code/day08.cpp", verbose = TRUE)
idx_node <- which(nodes[, 1] == "AAA")
aoc(nodes, instructions, idx_node - 1L)

# Second part -------------------------------------------------------------
idx_starting_nodes <- which(substr(nodes[, 1], 3, 3) == "A")

options(digits = 22)
vapply(
  X = idx_starting_nodes - 1L, 
  FUN = aoc, 
  FUN.VALUE = numeric(1), 
  instructions = instructions, 
  node_names = nodes, 
  version = 2L
) |> 
  Reduce(pracma::Lcm, x = _)
