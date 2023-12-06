d <- read.delim("data/day_5/puzzle.txt", header = FALSE)
# sample data
# d <- read.delim("data/day_5/sample.txt", header = FALSE)

# Part 1 ----------
library(tidyverse)

seeds <- strsplit(d[1,], split = " ") |> 
  unlist() |> 
  setdiff("seeds:")

# each line has a source range, which are seed numbers,
# and destination ranges, which are locations
# any seed not included has the same destination as its number (e.g., 10=10)
less_one <- d |> 
  filter(row_number() != 1) 
group_start <- less_one |> 
  mutate(group_start = str_detect(V1, ":")) |> 
  pull(group_start)
group_inds <- which(group_start)

# CHANGE -- FOR EACH SEED, WALK THROUGH TO LOCATION

get_locations <- function(seeds) {
  map_dbl(seeds, function(s) {
    dest <- s
    for (i in 1:length(group_inds)) {
      if (group_inds[i] == group_inds[length(group_inds)]) {
        this <- less_one[c(group_inds[i]:nrow(less_one)),]
      } else {
        this <- less_one[c(group_inds[i]:(group_inds[i+1]-1)),]
      }
      
      ss <- as.numeric(dest)
      
      lab <- this[1]
      vals <- this[2:length(this)]
      
      these_maps <- map_df(vals, function(j) {
        that <- strsplit(j, split = " ") |> 
          unlist() |> 
          as.numeric()
        r <- that[3]
        
        tibble(
          lab = lab,
          dest_min = that[1],
          dest_max = that[1] + r - 1,
          source_min = that[2],
          source_max = that[2] + r - 1
        )
      })
      
      w_seed <- these_maps |> 
        mutate(is_seed = ifelse(ss >= source_min & ss <= source_max,
                                TRUE, FALSE),
               ind = row_number(),
               seed = ss) |> 
        filter(is_seed)
      
      if (nrow(w_seed) > 0) {
        diff <- ss - w_seed$source_min
        location <- w_seed$dest_min + diff
      } else {
        # cat(crayon::cyan(glue::glue("Seed {ss}")), "\n")
        location <- ss
      }
      
      dest <- location
    }
    return(dest)
  }) 
}

get_locations(seeds) |> 
  min()

