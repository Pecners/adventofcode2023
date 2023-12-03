d <- read.delim("data/day_3/puzzle.txt", header = FALSE)

# Part 2 ----------
library(tidyverse)

# Step 1 - prep data
splt <- strsplit(d$V1, split = "") |> 
  unlist() |> 
  as.character()

# create matrix of original data
mat <- matrix(splt, nrow = nrow(d), ncol = str_length(d[1,1]), 
              byrow = TRUE)

# set up dummie matrices
is_symb <- is_both <- matrix(data = FALSE, 
                             nrow(mat), 
                             ncol(mat), byrow = TRUE)

# Step 2 - identify true areas
#   - for every *, 3x3 grid centered on cell is TRUE
#   - label with an index for the center of the grid "i,j"
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
    
    if (mat[i,j] == "*") {
      m <- mat[i_range, j_range] 
      # m <- str_detect(m, "\\d") |> 
      #   matrix(nrow = 3, ncol = 3)
      
      num_count <- map_dbl(1:3, function(r) {
        these <- m[r,] |> 
          paste0(collapse = "") |> 
          str_extract_all("\\d*")
        nc <- which(!is.na(as.numeric(these[[1]]))) |> length()
        return(nc)
      }) |> 
        sum()
      
      if (num_count == 2) {
        is_symb[i_range, j_range] <<- paste(i, j, sep = ",")
      }
    }
  })
})

# Step 3 - identify cells live cells with digits
is_both <- str_detect(mat, "\\d") & (is_symb != "FALSE")

# Step 4 - pull whole numbers connected to Step 3 cells
these_numbers <- map_df(1:nrow(d), function(i) {
  # limit to one row
  tr <- d$V1[[i]]
  this_row <- str_locate_all(tr, "\\d")[[1]] |> 
    as_tibble()
  true_nums <- is_both[i,]
  
  # Extract multi-digit numbers from row
  g <- 1
  this_ind <- map_df(1:nrow(this_row), function(j) {
    if (j == 1) {
      delta <- 1
    } else {
      delta <- this_row[[j,2]] - this_row[[(j-1),2]]
    }
    if (delta > 1) {
      g <<- g + 1
    }
    this_row[j,] |> 
      mutate(delta = delta,
             group = g)
  }) |> 
    group_by(group) |> 
    summarise(group_start = min(start),
              group_end = max(end),
              has_true = sum(true_nums[group_start:group_end]))

  # Return a df that shows the row and symbol indexes, 
  # makes debugging easier than just returning numbers
  map_df(1:nrow(this_ind), function(j) {
    if (as.numeric(this_ind[[j, "has_true"]]) > 0) {
      s <- this_ind[[j,"group_start"]]
      e <- this_ind[[j,"group_end"]]
      x <- str_sub(tr, start = s, end = e) |> 
        as.numeric()
      
      ti <- is_symb[i,c(s:e)] 
      fixed <- str_replace_all(ti, "FALSE", min(ti)) |> 
        unique()

      tibble(row = i, s_ind = fixed, n_count = j, num = x)
    }
  })
  
})

# Step 5 - compute product of symbol groups, then sum
these_numbers |> 
  group_by(s_ind) |> 
  summarise(total = prod(num)) |> 
  ungroup() |> 
  summarise(total = sum(total))

