# First trials
# Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Ingoing values for the stage ----
w <- 10
h <- 50
t <- 0

stage <- matrix(0, h, w)

# Defining pieces ----
pieces <- list(c(1,0,1,0,1,1,0,0), c(0,1,0,1,1,1,0,0), c(0,1,1,1,1,0,0,0), c(1,0,1,1,0,1,0,0), c(1,0,1,0,1,0,1,0), c(1,1,1,1,0,0,0,0), c(1,0,1,1,1,0,0,0))
pieces <- lapply(pieces, function(x) matrix(x, 4, 2, byrow = T))
pieces <- lapply(pieces, function(x) x[rowSums(x) != 0,])
pieces <- lapply(pieces, function(x) x[,colSums(x) != 0])
pieces[[5]] <- matrix(1, 4, 1)
names(pieces) <- c("L", "P", "Z", "S", "I", "O", "E")

current_piece <- sample(1:7, 1)

# Plotting stage ----
stage_2_df <- function(x){
  x <- as.data.frame(x)
  x$row <- dim(x)[1]:1
  pivot_longer(x, -row, values_to = "value", names_prefix = "V", names_to = "col") %>% 
    mutate(col = as.numeric(col), row = as.numeric(row))
}

stage_df <- stage_2_df(stage)

ggplot(stage_df, aes(col, row, color = as.character(value))) + geom_point() + coord_equal() + theme(legend.position = "none")

plot_stage <- function(stage){
  g <- ggplot(stage_2_df(stage), aes(col, row, color = as.character(value))) + 
    geom_point() + 
    coord_equal() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("white", "black", "red"))
  print(g)
}

# A round
current_piece <- 1

placement <- 3
transpose <- F

drop_piece <- function(stage, current_piece, placement, transpose){
  current_piece <- pieces[[current_piece]]
  if (transpose) current_piece <- t(current_piece)
  if (placement + dim(current_piece)[2] > w) placement <- w - dim(current_piece)[2] + 1
  
  covered_columns <- placement:(placement + dim(current_piece)[2] - 1)
  
  covered_stage <- stage[, covered_columns]
  first_hit <- min(apply(covered_stage, 2, function(x) max(which(x == 0))))
  
  covered_rows <- first_hit:(first_hit - dim(current_piece)[1] + 1)
  
  stage[covered_rows, covered_columns] <- stage[covered_rows, covered_columns] + current_piece
  
  stage
}

drop_piece(stage, 1, 1, F) %>% 
  drop_piece(1, 1, F) %>% 
  drop_piece(5, 8, T) %>% 
  drop_piece(1, 1, F) %>% 
  drop_piece(1, 1, F) %>% 
  plot_stage()

for(i in 1:10){
  stage <- drop_piece(stage, sample(1:7,1), sample(1:10, 1), sample(0:1, 1))
  plot_stage(stage) %>% print()
  Sys.sleep(1)
}
