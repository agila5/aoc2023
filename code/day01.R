# First part --------------------------------------------------------------
input <- readLines("data/input01.txt")

vapply(
  X = input, 
  FUN = function(x) {
    # Extract all digits
    digits <- regmatches(
      x = x, 
      m = gregexpr("[0-9]", x)
    )
    digits <- digits[[1]]
    as.numeric(paste0(digits[1], digits[length(digits)]))
  },  
  FUN.VALUE = numeric(1)
) |> 
  sum()


# Second part -------------------------------------------------------------
vapply(
  X = input, 
  FUN = function(x) {
    # Divide digits and non-digits
    splitted <- regmatches(
      x = x, 
      m = gregexpr(
        pattern = "([0-9]|\\D+)",
        text = x
      )
    )
    splitted <- splitted[[1]]
    digits <- lapply(
      X = splitted, 
      FUN = function(x) {
        if (grepl("\\d+", x)) {
          return(x)
        }
        matching_text <- c(
          "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
          "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9
        )
        # The following is required (instead of something like a grep()) since
        # there might be overlapping patterns (e.g. "oneight"). gregexpr is
        # required since there are duplicated matches and I need all of them. I
        # run matching using out[unlist(greg...)] since I need to preserve the
        # order according to which I observed the data.
        out <- list()
        for (t in names(matching_text)) {
          if (grepl(t, x)) {
            out[unlist(gregexpr(t, x))] <- matching_text[t]
          }
        }
        unlist(out)
      }
    )
    digits <- unlist(digits)
    digits <- digits[digits != ""]
    as.numeric(paste0(digits[1], digits[length(digits)]))
  }, 
  FUN.VALUE = numeric(1)
) |> 
  sum()