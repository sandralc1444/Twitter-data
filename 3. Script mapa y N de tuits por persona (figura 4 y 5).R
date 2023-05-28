############################Distribución geográfica###########################
###Excel: datatwitter_map.xlsx

library(tidyverse)
library(maps)
library(ggplot2)
library(ggthemes)

# Cargar datos y mapa
map_world <- map_data("world")
datatwitter_map <- read_excel("datatwitter_map.xlsx")
datatwitter_map <- na.omit(datatwitter_map)

# Cambiar etiquetas de los temas

datatwitter_map$topic <- gsub("Fish", "Pesca recreativa", datatwitter_map$topic)
datatwitter_map$topic <- gsub("Hunt", "Caza recreativa", datatwitter_map$topic)

# Crear el mapa fijo
p <- ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group),
               fill = "grey80",
               color = "white") +
  geom_point(data = datatwitter_map,
             aes(x = coord2, y = coord1, color = as.factor(topic)),
             alpha = 1,
             size = 2) +
  scale_color_manual(values = c("coral3", "cadetblue"), name = " ") +
  ggtitle("Tuits totales en el mundo") +
  theme_bw()

# Guardar el mapa fijo como imagen
ggsave("C:/Users/annas/OneDrive/Documents/CURS DATA SCIENCE UB 2023/Treball Data Science/Maps/TuistTotales_mapa.png", plot = p, width = 8, height = 6)


######################Número de tuits por persona##########################
#######Excel: Twitter_data - cleaned-up - unnamed users.xlsx

library(tidyverse)
library(readxl)
setwd("C:/Users/annas/OneDrive/Documents/CURS DATA SCIENCE UB 2023/Treball Data Science/Tweets for users")
datatwitter_unnamed <- read_excel("Twitter_data - cleaned-up - unnamed users.xlsx")

View(datatwitter_unnamed)
library(dplyr)

tweets_for_person <- datatwitter_unnamed %>%
  group_by(user) %>%
  summarise(num_tweets = n())

View(tweets_for_person)
tweets_for_person <- tweets_for_person %>% arrange(desc(num_tweets)) #ordenar decreciente
library(ggplot2)
ggplot(tweets_for_person, aes(x = user, y = num_tweets, group = 1))+ 
  geom_line() +
  scale_x_discrete(limits = tweets_for_person$user)


