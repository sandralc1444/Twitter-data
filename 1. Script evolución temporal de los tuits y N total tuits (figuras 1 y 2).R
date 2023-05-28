#####################Estudio de la evolución temporal de los tuits#################
Excel con las columnas: search_term y year: data.xlsx

install.packages("readxl")
library(readxl)
dateprova <- read_excel("data.xlsx", sheet = 1)

str_count(dateprova)
str(dateprova) #str para la tabla en variables independientes (para trabajar con ellas de forma independiente)
table(dateprova$search_term) #Frecuencias por cada search_term

d1 <- data.frame(table(dateprova$search_term)) #lo ordenamos para verlo de forma mejor en un data.frame


##########FRECUENCIAS DE NUESTRAS DOS CATEGORÍAS: RECREATIONAL FISHING Y RECREATIONAL HUNTING##################

#Vector para unir todas las palabras de fish en una nueva columna

library(dplyr)

dateprova <- dateprova %>%
  mutate(nueva_categoria = ifelse(search_term %in% c("recreational_fish", "recreational_fishers", "recreational_fisheries", "recreational_fishery", "recreational_fishing", "recreational_fisher"), "recreational fishing", search_term))

#Vector para unir todas las palabras de hunting en una nueva columna
dateprova <- dateprova %>%
  mutate(nueva_categoria2 = ifelse(search_term %in% c("recreational_hunt", "recreational_hunter", "recreational_hunting", "recreational_hunters"), "recreational hunting", search_term))


str(dateprova) #str para la tabla en variables independientes (para trabajar con ellas de forma independiente)
table(dateprova$nueva_categoria)
str(dateprova) #str para la tabla en variables independientes (para trabajar con ellas de forma independiente)
table(dateprova$nueva_categoria2)


######Para unir en una nueva columna "recreational fishing" y "recreational hunting"

dateprova$topic <-ifelse(dateprova$nueva_categoria == "recreational fishing", "recreational fishing",
                         ifelse(dateprova$nueva_categoria2 == "recreational hunting", "recreational hunting", NA))



a <- subset(dateprova, select = c("year", "topic" ))


##########EVOLUCIÓN TUITS CON EL TIEMPO DE LAS DOS CATEGORÍAS#########

###Arreglar datos para la gráfica de evolución temporal

install.packages("stringr")
library(stringr)

data.frame(a$topic, a$year, fish=str_count(a$topic,"fish"), hunt=str_count(a$topic,"hunt")) #Para que cuente con 0 y 1 los datos que empiezan por fish y por hunt, en vd no cal hacer lo anterior, se podría haber hecho directamente así, pero así tenemos un data.frame ordenado y no tenemos que cambiar los nombres otra vez

dy<-data.frame(a$topic, a$year, fish=str_count(a$topic,"fish"), hunt=str_count(a$topic,"hunt")) #le damos un nombre al nuevo dataframe

aggregate(dy$fish, by=list(Category=dy$a.year), FUN=sum) #aggregate=sumar para cada categoría (año)
aggregate(dy$hunt, by=list(Category=dy$a.year), FUN=sum)

tr<-aggregate(dy$fish, by=list(Category=dy$a.year), FUN=sum) #Para que sume los tuits de recreational fishing por año
trh<-aggregate(dy$hunt, by=list(Category=dy$a.year), FUN=sum) #Para que sume los tuits de recreational hunting por año

str(tr)
str(trh)

gra <- data.frame(year=tr$Category, fishing=tr$x, hunting=trh$x) #tabla resumen (data frame dónde tenemos la columna de año y una columna para fishing y otra para hunting)


##########Gráfica evolución temporal del número de tuits de cada categoría en el tiempo

install.packages("ggplot2")
library(ggplot2)


#Para usar ggplot necesitamos datos numéricos



ggplot(gra, aes(as.numeric(year))) +
  geom_line(aes(y = as.numeric(fishing), colour = "fishing")) +
  geom_line(aes(y = as.numeric(hunting), colour = "hunting")) +
  scale_x_continuous(limits = c(2007, 2022), breaks = seq(2002, 2022, by = 1)) + #Para que el eje de la X vaya desde el 2012 hasta el 2022, de año en año
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 500))+
  labs(x="Años",y="Número de tuits", title= "Evolución temporal y variación de los tuits desde 2007 hasta 2022: un estudio longitudinal de la actividad en las redes sociales", colour = "Temática") +
  scale_color_manual(labels = c("Pesca recrativa", "caza recreativa"), values = c("cadetblue", "coral3")) +
  theme_classic() + #para ponerle un fondo blanco y sin rayas
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))

###########Gráfica de barras con el número total de tuits para las dos temáticas########

total_fishing <- sum(gra$fishing)
total_hunting <- sum(gra$hunting) #Para sumar en la nueva variable "total_fishing" y "total_hunting" toda la columna del data.frame gra de fishing y hunting

total <- c(total_fishing,total_hunting) #Creamos el vector con las dos sumas anteriores

max_value <- max(total) #valor máximo del vector, para ajustar el eje de la y

par(mar = c(1,1,1,1)) #ajusta los márgenes inferior, izquierdo, superior y derecho respectivamente

barplot(total, names.arg = c("Pesca recreativa", "Caza recreativa"), xlab = "Temática", ylab = "Número de tuits", col = c("cadetblue", "coral3"),
        main = "Número total de tuits para cada temática",
        ylim = c(0, max_value+100),
        axes = TRUE)
