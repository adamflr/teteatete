# First trials
# Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Ingoing values for the stage ----
w <- 10
h <- 25
t <- 0

stage <- matrix(0, h, w)

# Defining pieces ----
pieces <- list(c(1,0,1,0,1,1,0,0), c(0,1,0,1,1,1,0,0), c(0,1,1,1,1,0,0,0), 
               c(1,0,1,1,0,1,0,0), c(1,0,1,0,1,0,1,0), c(1,1,1,1,0,0,0,0), 
               c(1,0,1,1,1,0,0,0))
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

plot_stage <- function(stage){
  g <- ggplot(stage_2_df(stage) %>% dplyr::filter(value != 0), 
              aes(col, row, color = as.character(value))) + 
    geom_point(size = 4, shape = 15) + 
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = c(0.5, w + 0.5)) +
    coord_equal() +
    xlim(0, w + 1) +
    ylim(0, h + 5) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank())
  g
}

plot_stage(stage)

# A round
current_piece <- 1

placement <- 3
transpose <- F

drop_piece <- function(stage, current_piece, placement, transpose){
  current_piece_no <- current_piece
  current_piece <- pieces[[current_piece]]
  if (transpose) current_piece <- as.matrix(t(current_piece))
  
  if (placement + dim(current_piece)[2] > w) placement <- w - dim(current_piece)[2] + 1
  
  covered_columns <- placement:(placement + dim(current_piece)[2] - 1)
  
  covered_stage <- stage[, covered_columns]
  
  top_row_per_col <- apply(covered_stage, 2, function(x) ifelse(any(x != 0), min(which(x != 0)), h + 1))
  lost_row_at_piece_bottom <- dim(current_piece)[1] - apply(current_piece, 2, function(x) max(which(x != 0)))
  first_hit <- min(top_row_per_col + lost_row_at_piece_bottom)
  
  covered_rows <- (first_hit - dim(current_piece)[1]):(first_hit - 1)
  
  if(min(covered_rows) <= 0){
    errorCondition("Hitting the roof")
  }
  
  stage[covered_rows, covered_columns] <- stage[covered_rows, covered_columns] + current_piece * current_piece_no
  stage
}

drop_piece(stage, 1, 2, F)

drop_piece(stage, 1, 1, F) -> stage

drop_piece(stage, 1, 1, F) %>% 
  drop_piece(1, 1, F) %>% 
  drop_piece(5, 13, T) %>% 
  plot_stage(stage)

stage <- matrix(0, h, w)

for(i in 1:20){
  stage <- drop_piece(stage, sample(1:7,1), sample(1:10, 1), sample(0:1, 1))
  plot_stage(stage) %>% print()
}

