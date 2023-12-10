# First part --------------------------------------------------------------
input <- readLines("data/input09.txt")
get_history <- function(x) {
  x <- as.numeric(strsplit(x, " ")[[1]])
  new_x <- x
  list_x <- list()
  i <- 1L
  list_x[[1]] <- x
  
  repeat {
    i <- i + 1L
    new_x <- diff(new_x)
    list_x[[i]] <- new_x
    target <- rep(0, length(new_x))
    if (isTRUE(all.equal(new_x, target))) {
      break
    }
  }
  
  for (i in rev(seq_along(list_x))[-1]) {
    list_x[[i]] <- c(list_x[[i]], tail(list_x[[i + 1]], 1) + tail(list_x[[i]], 1))
  }
  
  # Get history value
  tail(list_x[[1]], 1)
}

vapply(input, get_history, numeric(1), USE.NAMES = FALSE) |> sum()

# Second part -------------------------------------------------------------
input <- readLines("data/input09.txt")
get_backward_history <- function(x) {
  x <- as.numeric(strsplit(x, " ")[[1]])
  new_x <- x
  list_x <- list()
  i <- 1L
  list_x[[1]] <- x
  
  repeat {
    i <- i + 1L
    new_x <- diff(new_x)
    list_x[[i]] <- new_x
    target <- rep(0, length(new_x))
    if (isTRUE(all.equal(new_x, target))) {
      break
    }
  }
  
  for (i in rev(seq_along(list_x))[-1]) {
    list_x[[i]] <- c(
      head(list_x[[i]], 1) - head(list_x[[i + 1]], 1), 
      list_x[[i]] 
    )
  }
  
  # Get history value
  head(list_x[[1]], 1)
}

vapply(input, get_backward_history, numeric(1), USE.NAMES = FALSE) |> sum()
