library(spam)
dims <- 20
world <- matrix(sample(c(0,1), size = dims * dims, replace = TRUE), 
  nrow = dims, ncol = dims)
sum(world) / (dims * dims)
image(world)
# so we need to figure out how to go through each of the cells and figure out
# if its unit is "satisfied" where satisfied is defined as having neighbors 
# that are at least half of like color


# we need to take in a row/column index
# this will allow us to generate the neighborhood and its sum
how_many_1s <- sum(world) / (dims * dims)
position_adjustments <- c(0, 1, -1)
positions <- gtools::permutations(3, 2, v = position_adjustments, 
  repeats.allowed = TRUE)
positions <- positions[-5, ]
# need to do somethign about going over 20/20!!
# sum_of_neighbor_values <- function(mat = neighbors)
# {
#   vals <- c()
#   for(i in 1:8)
#   {
#     row_idx <- mat[i, 1]
#     col_idx <- mat[i, 2]
#     if(row_idx %in% seq(1, 20, 1) & col_idx %in% seq(1, 20, 1))
#     {
#       vals[i] <- world[row_idx, col_idx]
#     }
#   }
#   sum(vals, na.rm = TRUE)
# }
sum_of_neighbor_values <- function(mat = neighbors, dimens = dims)
{
  vals <- sapply(1:8, function(i)
   {
    row_idx <- mat[i, 1]
    col_idx <- mat[i, 2]
    if(row_idx %in% seq(1, dimens, 1) & col_idx %in% seq(1, dimens, 1))
    {
      world[row_idx, col_idx]
    }
   }
  )
  sum(unlist(vals), na.rm = TRUE)
}
# write a function that deals with positions on the edges
# position_on_edges <- function()
# {
#   if(selected_unit == 1 & sum_of_neighbors < 3)
#   {
#     world_matrix[row_pos, col_pos] <- 0
#   }
#   if(selected_unit == 0 & sum_of_neighbors >= 3)
#   {
#     world_matrix[row_pos, col_pos] <- 1
#   }
#   #cat("it works!")
# }

segregation_function <- function(world, dims)
{
  world_matrix <- world
  row_pos <- sample(seq(1, dims, 1), 1, replace = TRUE)
  col_pos <- sample(seq(1, dims, 1), 1, replace = TRUE)
  selected_unit <- world_matrix[row_pos, col_pos]
  neighbors <- cbind(row_pos + positions[, 1], col_pos + positions[, 2])
  sum_of_neighbors <- sum_of_neighbor_values(mat = neighbors, dimens = dims)
  if(row_pos %in% c(1, dims) | col_pos %in% c(1, dims))
  {
    if(row_pos == col_pos | row_pos == 1 & col_pos == dims | 
      row_pos == dims & col_pos == 1)
    {
      if(selected_unit == 1 & sum_of_neighbors < 2)
      {
        world_matrix[row_pos, col_pos] <- 0
      }
      if(selected_unit == 0 & sum_of_neighbors >= 2)
      {
        world_matrix[row_pos, col_pos] <- 1
      }
    }
    if(selected_unit == 1 & sum_of_neighbors < 3)
    {
      world_matrix[row_pos, col_pos] <- 0
    }
    if(selected_unit == 0 & sum_of_neighbors >= 3)
    {
      world_matrix[row_pos, col_pos] <- 1
    }
    #position_on_edges()
  }
  else{
    if(selected_unit == 1 & sum_of_neighbors < 4)
    {
      world_matrix[row_pos, col_pos] <- 0
    }
    if(selected_unit == 0 & sum_of_neighbors >= 4)
    {
      world_matrix[row_pos, col_pos] <- 1
    }
  }
  world <<- world_matrix
  print(sum(world) / (dims * dims))
}
dims <- 100
world <- matrix(sample(c(0,1), size = dims * dims, replace = TRUE, 
  prob = c(0.5, 0.5)), nrow = dims, ncol = dims)
image(world)
props <- replicate(10000, segregation_function(world = world, dims = dims))
plot(props)
image(world)

####################################
# Below here is broken/in development
####################################
# write a wrapper that allows you to enter dimensions, cutoff, and probs of 1s
# make the dimension be at least 3 x 3
# need to make it so the function knows to update the edge positions
# have it print the first matrix, last matrix, proportion of 1's
master_segregation_function <- function(dims = 20, cutoff = 4, 
  probs = c(0.5, 0.5), seed = sample(.Machine$integer.max, 1))
{
  world <- matrix(sample(c(0,1), size = dims * dims, replace = TRUE, 
    prob = c(0.5, 0.5)), nrow = dims, ncol = dims)
  props <- replicate(1000, segregation_function(world = world))
  props
  #par(mfrow = c(1, 1)) 
  #image(first_world)
  #image(world)
  #plot(props)
}
master_segregation_function()

