library(spam)
####################################
# Start
####################################

position_adjustments <- c(0, 1, -1)
positions <- gtools::permutations(3, 2, v = position_adjustments, 
  repeats.allowed = TRUE)
positions <- positions[-5, ]

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

find_and_check <- function(i, nas = na_coords, rowz = row_num,
  colz = col_num, dimensio = dims, matz = mat)
{
  unit_to_move <- matz[rowz, colz]
  distance_for_one <- dist(rbind(cbind(rowz, colz), nas[i, ]))
  ok <- c(distance = distance_for_one, nas[i, ])
  neighbors <- cbind(nas[i, 1] + positions[, 1], nas[i, 2] + positions[, 2])
  sum_of_neighbors <- sum_of_neighbor_values(mat = neighbors, dimens = dimensio)
  if(nas[i, 1] %in% c(1, dimensio) | nas[i, 2] %in% c(1, dimensio))
  {
    if((nas[i, 1] == nas[i, 2]) | (nas[i, 1] == 1 & 
       nas[i, 2] == dimensio) | (nas[i, 1] == dimensio & 
       nas[i, 2] == 1))
    {
      if(unit_to_move == 1 & sum_of_neighbors >= 2) 
      {
        return(ok)
      }
      if(unit_to_move == 0 & sum_of_neighbors < 2)
      {
        return(ok)
      }
      else{
        NULL
      }
    }
    else{
      if(unit_to_move == 1 & sum_of_neighbors >= 3)
      {
        return(ok)
      } 
      if(unit_to_move == 0 & sum_of_neighbors < 3)
      {
        return(ok)
      }
      else{
        NULL
      }
    }
  }
  else{
    if((unit_to_move == 1 & sum_of_neighbors >= 4) | 
       (unit_to_move == 0 & sum_of_neighbors < 4))
    {
      return(ok)
    }
    else{
      NULL
    }
  }
}

happy_spaces_switching <- function(row_num = row_pos, col_num = col_pos, 
  mat = world_matrix)
{
  selected_unit <- mat[row_num, col_num]
  na_coords <- which(is.na(mat), arr.ind = TRUE)
  distances <- do.call(rbind, sapply(1:nrow(na_coords), find_and_check, 
    nas = na_coords, rowz = row_num, colz = col_num, dimensio = dims,
    matz = mat))
  id_of_closest_happy_na <- which.min(distances[, 1])
  switch_row <- distances[id_of_closest_happy_na, 2]
  switch_col <- distances[id_of_closest_happy_na, 3]
  mat[switch_row, switch_col] <- selected_unit
  mat[row_num, col_num] <- NA
  mat
}

segregation_function <- function(world, dims)
{
  world_matrix <- world
  one_coords <- which(world_matrix == 1, arr.ind = TRUE)
  zero_coords <- which(world_matrix == 0, arr.ind = TRUE)
  non_na_coords <- rbind(one_coords, zero_coords)
  idx <- sample(seq(1:nrow(non_na_coords)), 1)
  row_pos <- non_na_coords[idx, 1]
  col_pos <- non_na_coords[idx, 2]
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
        world_matrix <- 
          happy_spaces_switching(row_num = row_pos, col_num = col_pos,
            mat = world_matrix)
      }
      if(selected_unit == 0 & sum_of_neighbors >= 2)
      {
        world_matrix <- 
          happy_spaces_switching(row_num = row_pos, col_num = col_pos,
            mat = world_matrix)
      }
    }
    else{
      if(selected_unit == 1 & sum_of_neighbors < 3)
      {
        world_matrix <- 
          happy_spaces_switching(row_num = row_pos, col_num = col_pos,
            mat = world_matrix)
      }
      if(selected_unit == 0 & sum_of_neighbors >= 3)
      {
        world_matrix <- 
          happy_spaces_switching(row_num = row_pos, col_num = col_pos,
            mat = world_matrix)
      }
    }
  }
  else{
    if(selected_unit == 1 & sum_of_neighbors < 4)
    {
      world_matrix <- 
        happy_spaces_switching(row_num = row_pos, col_num = col_pos, 
          mat = world_matrix)
    }
    if(selected_unit == 0 & sum_of_neighbors >= 4)
    {
      world_matrix <- 
        happy_spaces_switching(row_num = row_pos, col_num = col_pos,
          mat = world_matrix)
    }
  }
  world <<- world_matrix
}


dims <- 30
world <- matrix(sample(c(0,1, NA), size = dims * dims, replace = TRUE, 
  prob = c(0.3, 0.3, 0.3)), nrow = dims, ncol = dims)
image(world)
n_sims <- 1000
props <- system.time(replicate(n_sims, segregation_function(world = world, dims = dims)))
image(world)
props # system time: 7.123, pretty fast


####################################
# Stop!
####################################

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

# old below here

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

# segregation_function <- function(world, dims)
# {
#   world_matrix <- world
#   row_pos <- sample(seq(1, dims, 1), 1, replace = TRUE)
#   col_pos <- sample(seq(1, dims, 1), 1, replace = TRUE)
#   selected_unit <- world_matrix[row_pos, col_pos]
#   neighbors <- cbind(row_pos + positions[, 1], col_pos + positions[, 2])
#   sum_of_neighbors <- sum_of_neighbor_values(mat = neighbors, dimens = dims)
#   if(row_pos %in% c(1, dims) | col_pos %in% c(1, dims))
#   {
#     if(row_pos == col_pos | row_pos == 1 & col_pos == dims | 
#       row_pos == dims & col_pos == 1)
#     {
#       if(selected_unit == 1 & sum_of_neighbors < 2)
#       {
#         world_matrix[row_pos, col_pos] <- 0
#       }
#       if(selected_unit == 0 & sum_of_neighbors >= 2)
#       {
#         world_matrix[row_pos, col_pos] <- 1
#       }
#     }
#     if(selected_unit == 1 & sum_of_neighbors < 3)
#     {
#       world_matrix[row_pos, col_pos] <- 0
#     }
#     if(selected_unit == 0 & sum_of_neighbors >= 3)
#     {
#       world_matrix[row_pos, col_pos] <- 1
#     }
#     #position_on_edges()
#   }
#   else{
#     if(selected_unit == 1 & sum_of_neighbors < 4)
#     {
#       world_matrix[row_pos, col_pos] <- 0
#     }
#     if(selected_unit == 0 & sum_of_neighbors >= 4)
#     {
#       world_matrix[row_pos, col_pos] <- 1
#     }
#   }
#   world <<- world_matrix
#   print(sum(world) / (dims * dims))
# }