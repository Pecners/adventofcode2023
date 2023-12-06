d <- read.delim("data/day_5/puzzle.txt", header = FALSE)
# sample data
# d <- read.delim("data/day_5/sample.txt", header = FALSE)

# Part 1 ----------
library(tidyverse)

# vector of seeds
seeds <- strsplit(d[1,], split = " ") |> 
  unlist() |> 
  setdiff("seeds:")

# remove first row with seed data
less_one <- d |> 
  filter(row_number() != 1) 

# get vector that represents starting row of groups
group_start <- less_one |> 
  mutate(group_start = str_detect(V1, ":")) |> 
  pull(group_start)
group_inds <- which(group_start)

# FOR EACH SEED, WALK THROUGH TO LOCATION

get_locations <- function(seeds) {
  # for each seed, walk through the process to find the location
  map_dbl(seeds, function(s) {
    # each iteration produces the input to the next iteration
    dest <- s
    # this loop walks through each group for this seed
    # for (i in 1:length(group_inds)) {
    walk(1:length(group_inds), function(i) {
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
      cat(which(seeds == s), "\n")
      dest <<- location
    })
    dest
    
    
  }) 
}

start_time <- Sys.time()
cat(glue("Start Time: {start_time}"), "\n")
get_locations(seeds) |> 
  min()
end_time <- Sys.time()
cat(glue("Total time: {end_time - start_time}"))

# Part 2 ----------
library(doParallel)
library(parallel)
library(glue)

start_inds <- seq(from = 1, to = length(seeds), by = 2)
range_inds <- seq(from = 2, to = length(seeds), by = 2)

get_min <- function(i) {
  these_seeds <- seq(from = seeds[start_inds[i]] |> 
                       as.numeric(),
                     length.out = seeds[range_inds[i]] |> 
                       as.numeric(),
                     by = 1)
  this_min <- get_locations(these_seeds) |> 
    min()
  
  saveRDS(this_min, glue("data/day_5/part_2/{i}_min.rda"))
}

{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  registerDoParallel(10)
  
  foreach(i = 1:length(start_inds)) %dopar% get_min(i)
  
  ff <- list.files("data/day_5/part_2")
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}


map_dbl(ff, function(f) {
  read_rds(glue("data/day_5/part_2/{f}"))
}) |> 
  min()
