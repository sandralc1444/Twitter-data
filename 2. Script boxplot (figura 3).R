####################Gráfica de número total de tuits por mes###################
Excel que contenga las columnas: search_term, month y year (excel: boxplot_data.xlsx)

###En el script 1 teníamos una columna de años y otra de categoría (fishing y hunting), ahora necesitamos la columna de categoría con la de los meses y años.

dateprova <- read_excel("boxplot_data.xlsx", sheet = 1)

b <- subset(dateprova, select = c("search_term", "month", "year" )) #Seleccionar columnas que interesan


#Para cambiar los números por meses de la columna "month"
b$month <-ifelse(b$month == "01", "January",
                 ifelse(b$month == "02", "February",
                        ifelse(b$month == "03", "March",
                               ifelse(b$month == "04", "April",
                                      ifelse(b$month == "05", "May",
                                             ifelse(b$month == "06", "June",
                                                    ifelse(b$month == "07", "July",
                                                           ifelse(b$month == "08", "August",
                                                                  ifelse(b$month == "09", "September",
                                                                         ifelse(b$month == "10", "October",
                                                                                ifelse(b$month == "11", "November",
                                                                                       ifelse(b$month == "12", "December", NA))))))))))))

#Unir las temáticas en "Recreational Fishing" y "Recreational hunting"

#Vector para unir todas las palabras de fish en una nueva columna

library(dplyr)

b <- b %>%
  mutate(nueva_categoria = ifelse(search_term %in% c("recreational fish", "recreational fisher", "recreational fisheries", "recreational fishery", "recreational fishing", "recreational fishers"), "recreational fishing", search_term))

#Vector para unir todas las palabras de hunting en una nueva columna
b <- b %>%
  mutate(nueva_categoria2 = ifelse(search_term %in% c("recreational hunt", "recreational hunter", "recreational hunting", "recreational hunters"), "recreational hunting", search_term))


#Para comprobar si nos ha hecho bien los agrupamientos
str(b) #str para la tabla en variables independientes (para trabajar con ellas de forma independiente)
table(b$nueva_categoria)
str(b) #str para la tabla en variables independientes (para trabajar con ellas de forma independiente)
table(b$nueva_categoria2)

head(b) #para ver los primeros valores del data frame

######Para unir en una nueva columna "recreational fishing" y "recreational hunting"

b$topic <-ifelse(b$nueva_categoria == "recreational fishing", "recreational fishing",
                 ifelse(b$nueva_categoria2 == "recreational hunting", "recreational hunting", NA))



table(b$topic) #Para ver cuántos datos tenemos de cada temática y comprovar que se nos ha hecho bien la unión


b <- subset(b, select = c("topic", "month", "year" ))


library(stringr)

data.frame(b$topic,b$month,b$year,fish=str_count(b$topic,"fish"), hunt=str_count(b$topic,"hunt")) #Para que cuente con 0 y 1 los datos que empiezan por fish y por hunt

dy2<-data.frame(b$topic, b$month, b$year, fish=str_count(b$topic,"fish"), hunt=str_count(b$topic,"hunt")) #le damos un nombre al nuevo dataframe


fish<-aggregate(dy2$fish, by=list(Category=dy2$b.month, dy2$b.year), FUN=sum)
hunt<-aggregate(dy2$hunt, by=list(Category=dy2$b.month, dy2$b.year), FUN=sum)

sum(dy2$fish)
sum(dy2$hunt)

fish<-data.frame(category="fish", month= fish$Category, year= fish$Group.2, freq= fish$x)#Reagrupar los datos anteriores: añadimos la columna fish y cambiamos el nombre del resto de columnas
hunt<-data.frame(category="hunt", month= hunt$Category, year= hunt$Group.2, freq= hunt$x)

testprova<-rbind(fish, hunt) #Para fusionar las dos matrices fish y hunt





##############################Diagrama de cajas############################

ggplot(data = testprova, aes(x = month, y = freq, fill = category)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50)) +
  scale_x_discrete(labels=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")) +
  labs(x="Meses",y="Número de tuits", title= "Descubriendo tendencias temporales: un análisis de distribución de la actividad mensual de tweets de 2007 a 2022", color = "Tematica") +
  scale_fill_manual(values = c("cadetblue", "coral3"),name = "Temática", labels = c("Pesca recreativa", "Caza recreativa")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))




