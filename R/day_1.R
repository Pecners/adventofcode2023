d <- read.delim("data/day_1/puzzle.txt", header = FALSE)


# Part 1
library(tidyverse)

# Steps
# 1. Extract first and last digit from each line
# 2. Combine digits to form two-digit number
#     - single digit is doubled, e.g., 1 => 11
# 3. Answer is the sum of these two-digit numbers

d |> 
  mutate(
    
    # remove all letters from each line, leaving only digits
    x = str_remove_all(V1, "[:alpha:]"),
    
    # extract the first digit
    y = str_extract(x, "^\\d"),
    
    # extract the last digit
    z = str_extract(x, "\\d$"),
    
    # combine y and z to make two-digit number
    yz = paste0(y, z)) |> 
  
  # extract this to a vector
  pull(yz) |> 
  
  # convert to numberic
  as.numeric() |> 
  
  # sum up entire vector
  sum()

# Part 2
# added twist: also handle written numbers 1:9
make_num <- function(x) {
  map_chr(x, function(s) {
    switch (s, 
            "one" = "1",
            "two" = "2",
            "three" = "3",
            "four" = "4",
            "five" = "5",
            "six" = "6",
            "seven" = "7",
            "eight" = "8",
            "nine" = "9",
            s)
  })
}

# get first numeral
first <- d$V1 |> 
  str_extract("\\d|one|two|three|four|five|six|seven|eight|nine") |> 
  make_num()

# get second numeral
last <- d$V1 |> 
  str_extract(".*(\\d|one|two|three|four|five|six|seven|eight|nine)") |> 
  str_extract("(\\d|one|two|three|four|five|six|seven|eight|nine)$") |> 
  make_num()

# combine
both <- paste0(first, last) |> 
  as.numeric()

# sum
sum(both)


# First attempt at part 2 I got wrong, not sure why, yet
# 
# mat <- d$V1 |> 
#   str_extract("[:digit:]|one|two|three|four|five|six|seven|eight|nine",
#               simplify = TRUE)
# 
# recoded <- mat |> 
#   map_chr(function(s) {
#     switch (s, 
#             "one" = "1",
#             "two" = "2",
#             "three" = "3",
#             "four" = "4",
#             "five" = "5",
#             "six" = "6",
#             "seven" = "7",
#             "eight" = "8",
#             "nine" = "9",
#             s)
#   }) |> 
#   matrix(nrow = 1000)
# 
# f <- map_dbl(1:nrow(recoded), function(r) {
#   this <- paste0(recoded[r,], collapse = "") 
#   x <- str_extract(this, "^.")
#   y <- str_extract(this, ".$")
#   paste0(x, y) |> 
#     as.numeric()
# }) 
# 
# sum(f)
