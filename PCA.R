# PCA of PL data

library(tidyverse)
library(wordspace)
library(ggfortify)

load("goalkeepers.rda")
load("outfielders.rda")

outfielders <- outfielders %>% 
  filter(!grepl("GK", Pos))


defenders <- outfielders %>% 
  filter(grepl("DF", Pos))

midfielders <- outfielders %>% 
  filter(grepl("MF", Pos))

forwards <- outfielders %>% 
  filter(grepl("FW", Pos))


pca <- outfielders %>%
  select(-c(1:8)) %>% 
  sapply(as.numeric) %>% 
  as_tibble() %>% 
  sapply(replace_na, 0) %>% 
  as_tibble() %>% 
  prcomp(center = TRUE, scale. = TRUE)

autoplot(pca, data = outfielders, colour = 'Pos')


get_similarity <- function(position_data, player_name) {
  pca <- position_data %>%
    select(-c(1:8)) %>% 
    sapply(as.numeric) %>% 
    as_tibble() %>% 
    sapply(replace_na, 0) %>% 
    as_tibble() %>% 
    prcomp(center = TRUE, scale. = TRUE)
  
  similarity <- dist.matrix(pca$x)
  
  scores <- tibble(player = position_data$Player,
                   team = position_data$Squad,
                   age = position_data$Age,
                   likeness = similarity[grep(player_name, position_data$Player),]) %>%
    arrange(likeness)
  
  return(scores)
}

