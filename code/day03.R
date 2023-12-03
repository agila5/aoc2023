# First part --------------------------------------------------------------
input <- readLines("data/input03.txt")
input_mat <- as.matrix(do.call(rbind, lapply(input, \(x) strsplit(x, "")[[1]])))

sum_parts <- 0
for (i in seq_along(input)) {
  pos_nums <- gregexpr("\\d+", input[[i]])[[1]]
  
  if (any(pos_nums < 0)) next
  
  for (j in seq_along(pos_nums)) {
    seq_j <- seq(pos_nums[j], length.out = attr(pos_nums, "match.length")[j])
    seq_y <- seq.int(
      from = max(1, min(seq_j) - 1L), 
      to = min(ncol(input_mat), max(seq_j) + 1L)
    )
    seq_x <- seq.int(
      from = max(1, i- 1), 
      to = min(nrow(input_mat), i + 1)
    )
    boundary_data <- input_mat[seq_x, seq_y]
    if (any(grepl("[^\\.\\d]", as.vector(boundary_data), perl = TRUE))) {
      part_num <- as.numeric(paste0(input_mat[i, seq_j], collapse = ""))
      sum_parts <- sum_parts + part_num
    }
  }
}

print(sum_parts)

# Second part -------------------------------------------------------------
rm(list = ls())
input <- readLines("data/input03.txt")
input_mat <- as.matrix(do.call(rbind, lapply(input, \(x) strsplit(x, "")[[1]])))
unique_id_nums <- 
  matrix((t(input_mat) %in% as.character(0:9)) * 
  (1 + cumsum(!(t(input_mat) %in% as.character(0:9)))), nrow = nrow(input_mat), ncol = ncol(input_mat), byrow = TRUE)
unique_id_nums[unique_id_nums == 0] <- NA

sum_gear_ratio <- 0

for (i in seq_along(input)) {
  pos_gear <- gregexpr("\\*", input[[i]])[[1]]
  
  if (any(pos_gear < 0)) next
  
  for (j in seq_along(pos_gear)) {
    seq_y <- seq.int(
      from = max(1, pos_gear[[j]] - 1L), 
      to = min(ncol(input_mat), pos_gear[[j]] + 1L)
    )
    seq_x <- seq.int(
      from = max(1, i- 1), 
      to = min(nrow(input_mat), i + 1)
    )
    boundary_data <- unique_id_nums[seq_x, seq_y]
    id_nums_in_boundary <- boundary_data |> as.vector() |> na.omit() |> unique()
    if (length(id_nums_in_boundary) == 2L) {
      num1 <- input_mat[which(unique_id_nums == id_nums_in_boundary[[1]])] |> paste0(collapse = "") |> as.numeric()
      num2 <- input_mat[which(unique_id_nums == id_nums_in_boundary[[2]])] |> paste0(collapse = "") |> as.numeric()
      sum_gear_ratio <- sum_gear_ratio + num1 * num2
    }
  }
}

print(sum_gear_ratio)
