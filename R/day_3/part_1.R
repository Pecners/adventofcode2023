d <- read.delim("data/day_3/puzzle.txt", header = FALSE)

# Part 1 ----------
library(tidyverse)

# Step 1 - prep data
#   - convert data to matrix
splt <- strsplit(d$V1, split = "") |> 
  unlist() |> 
  as.character()

# get vector of symbols to detect
symbs <- unique(splt)
these_symbs <- symbs[which(!str_detect(symbs, "\\d|\\."))]

# create matrix of original data
mat <- matrix(splt, nrow = nrow(d), ncol = str_length(d[1,1]), 
              byrow = TRUE)

# set up dummie matrices
is_symb <- is_both <- matrix(data = FALSE, 
                  nrow(mat), 
                  ncol(mat), byrow = TRUE)

# Step 2 - identify true areas
#   - for every symbol, 3x3 grid centered on cell is TRUE
#   - any number that has digits overlap with TRUE cells should be included
walk(1:nrow(mat), function(i) {
  walk(1:ncol(mat), function(j) {
    if (i == 1) {
      i_range <- 1:2
    } else if (i == nrow(mat)) {
      i_range <- nrow(mat):(nrow(mat)-1)
    } else {
      i_range <- (i-1):(i+1)
    }
    
    if (j == 1) {
      j_range <- 1:2
    } else if (j == nrow(mat)) {
      j_range <- nrow(mat):(nrow(mat)-1)
    } else {
      j_range <- (j-1):(j+1)
    }
    
    if (mat[i,j] %in% these_symbs) {
      is_symb[i_range, j_range] <<- TRUE
    }
    
  })
})

# Step 3 - identify cells live cells with digits
is_both <- str_detect(mat, "\\d") & is_symb

# Step 4 - pull whole numbers connected to Step 3 cells
these_numbers <- map_df(1:nrow(d), function(i) {
  # limit to one row
  tr <- d$V1[[i]]
  this_row <- str_locate_all(tr, "\\d")[[1]] |> 
    as_tibble()
  true_nums <- is_both[i,]
  
  # Extract multi-digit numbers from row
  g <- 1
  this_ind <- map_df(1:nrow(this_row), function(i) {
    if (i == 1) {
      delta <- 1
    } else {
      delta <- this_row[[i,2]] - this_row[[(i-1),2]]
    }
    if (delta > 1) {
      g <<- g + 1
    }
    
    this_row[i,] |> 
      mutate(delta = delta,
             group = g)
  }) |> 
    group_by(group) |> 
    summarise(group_start = min(start),
              group_end = max(end),
              # this will be > 0 if it should be included
              has_true = sum(true_nums[group_start:group_end])) 
  
  # Return a df that shows the row and row-number indexes, 
  # makes debugging easier than just returning numbers
  map_df(1:nrow(this_ind), function(j) {
    if (this_ind[[j, "has_true"]] > 0) {
      x <- str_sub(tr, start = this_ind[[j,"group_start"]],
                   end = this_ind[[j,"group_end"]]) |> 
        as.numeric()
      tibble(row = i, n_count = j, num = x)
    }
  })
  
})

# Step 5 - sum it up
sum(these_numbers$num)

