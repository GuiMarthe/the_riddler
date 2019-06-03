library(tidyverse)
library(Matrix)


simulate_game <- function(figures_size = 54 , grid_size = 4) {
  figures <- 1:figures_size
  grid1 <- sample(figures, size = grid_size * grid_size, replace = F) 
  grid2 <- sample(figures, size = grid_size * grid_size, replace = F) 
  points1 = 0
  points2 = 0
  figures <- sample(figures)
  for (card in figures) {
  		if (card %in% grid1) points1 <- points1 + 1L
  		if (card %in% grid2) points2 <- points2 + 1L
  #		print('points1')
  #		print(points1)
  #		print('points2')
  #		print(points2)
  #		print('figs')
  #		print(figures)
  		game_over <- (points1 >= grid_size*grid_size) | (points2 >= grid_size*grid_size) 
  #		print('cond')
 # 		print(cond)
#		  print('end round -----')
		  if(game_over) break
  }
  c(points1 ,points2)
#  print('end game -----')
}


repeat(simulate_game(54, 4))

simulations <- 
tibble(figures_size = seq(5, 1000, by = 100)) %>%  
	crossing(grid_size = seq(2, 13, by = 1)) %>% 
	filter(figures_size > grid_size*grid_size) %>% 
	crossing(trial = 1:1500) %>% 
	mutate(scores = map2(.x = figures_size, .y = grid_size, 
														 .f = ~ simulate_game(.x, .y)))


trials <- simulations %>% 
	mutate(losing_score = map_dbl(scores, min)) %>% 
	mutate(winning_score = map_dbl(scores, max)) %>% 
	select(-scores)


trials %>% 
	group_by(figures_size, grid_size) %>% 
	summarise(
		mean_losing_score = mean(losing_score), 
		mean_winning_score = mean(winning_score), 
						prop_zero = mean(losing_score == 0)
						) -> trials_sum


trials_sum %>% 
	ggplot(aes(figures_size, prop_zero)) +
	geom_point() + 
	geom_line(aes(group = grid_size, color = as.character(grid_size)))

trials_sum %>% arrange(grid_size ) %>% view



