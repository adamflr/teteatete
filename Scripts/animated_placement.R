# Animated placement
library(gganimate)

# First step
current_piece_no <- 4
current_piece <- pieces[[current_piece_no]]

current_piece_longer <- stage_2_df(current_piece) %>% filter(value != 0)

g1 <- plot_stage(stage) + 
  geom_point(aes(col, row + h, color = as.character(current_piece_no)), data = current_piece_longer, inherit.aes = F, size = 4, shape = 15)

# Second step
placement <- 5
current_piece_longer %>% 
  mutate(col = col - 1 + placement) -> current_piece_longer

g2 <- plot_stage(stage) + 
  geom_point(aes(col, row + h, color = as.character(current_piece_no)), data = current_piece_longer, inherit.aes = F, size = 4, shape = 15)

# Third step
g3 <- drop_piece(stage, 4, placement, transpose) %>% plot_stage()

# One, two, three
dat <- g1$data
dat$id <- 1:dim(dat)[1]
dat$t <- 1

g1$layers[[4]]$data
g3$data


rbind(dat, 
      stage_2_df(current_piece) %>% filter(value != 0) %>% mutate(id = paste0("n", 1:length(value)), t = 1),
      stage_2_df(current_piece) %>% filter(value != 0) %>% mutate(id = paste0("n", 1:length(value)), col = col - 1 + placement, t = 2),
      g3$data %>% mutate(id)
      )
      

