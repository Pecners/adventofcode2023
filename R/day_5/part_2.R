# d <- read.delim("data/day_5/puzzle.txt", header = FALSE)
# sample data
d <- read.delim("data/day_5/sample.txt", header = FALSE)


# Part 2 ----------
library(tidyverse)

seeds <- strsplit(d[1,], split = " ") |> 
  unlist() |> 
  setdiff("seeds:")

start_inds <- seq(from = 1, to = length(seeds), by = 2)
range_inds <- seq(from = 2, to = length(seeds), by = 2)

seed_ranges <- map_df(1:length(start_inds), function(i) {
  this_s <- seeds[start_inds[i]] |> 
    as.numeric()
  this_r <- seeds[range_inds[i]] |> 
    as.numeric() - 1
  tibble(s_min = this_s,
         s_max = this_s + this_r)
})

less_one <- d |> 
  filter(row_number() != 1) 
group_start <- less_one |> 
  mutate(group_start = str_detect(V1, ":")) |> 
  pull(group_start)
group_inds <- which(group_start)

this <- less_one[c(rev(group_inds)[1]:nrow(less_one)),]

vals <- this[2:length(this)] 

locs <- map_df(vals, function(j) {
  that <- strsplit(j, split = " ") |> 
    unlist() |> 
    as.numeric()
  r <- that[3]
  
  tibble(
    dest_min = that[1],
    dest_max = that[1] + r - 1,
    source_min = that[2],
    source_max = that[2] + r - 1
  )
}) |> 
  mutate(ind = row_number()) |> 
  arrange(dest_min) |> 
  pull(ind)
# Work backwards from closest locations

min_loc_ranges <- map_df(locs, function(l) {
    s_min <- NULL
    s_max <- NULL
    # for (i in length(group_inds):1) {
      for (i in 7:1) {
      if (group_inds[i] == group_inds[length(group_inds)]) {
        this <- less_one[c(group_inds[i]:nrow(less_one)),]
      } else {
        this <- less_one[c(group_inds[i]:(group_inds[i+1]-1)),]
      }
      
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
      
      if (group_inds[i] == group_inds[length(group_inds)]) {
        w_seed <- these_maps[l,]
        s_min <- w_seed$source_min
        s_max <- w_seed$source_max
      } else {
        w_seed <- these_maps |> 
          filter(
            (s_min >= dest_min & s_min <= dest_max) |
              (s_max >= dest_min & s_max <= dest_max)
          ) 
        
        if (nrow(w_seed > 0)) {
          s_min <- w_seed$dest_min
          s_max <- w_seed$dest_max
        } 
      }
      cat(s_min, s_max, "\n")

    }
    tibble(
      ind = which(l == locs),
      s_min,
      s_max
    )
})

map_df(1:nrow(min_loc_ranges), function(i) {
  t <- min_loc_ranges[i,]

})