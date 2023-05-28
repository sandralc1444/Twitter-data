######GRÁFICA DE IMPACTO DE LOS TUITS: me gusta, respuestas y retuits#######
Excel: new3.xlsx, que contiene las columnas de me gusta, respuestas y retuits

newdata <- read_excel("new3.xlsx", sheet = 1)

data <- cbind(dy2, newdata) #cbind es para unir dos data.frame el dy2 y el newdata


total <- tapply(data$likes_count, data$b.topic, sum) #suma para la temática de caza por un lado y de pesca por el otro del total de likes
total1 <- tapply(data$replies_count, data$b.topic, sum)
total2 <- tapply(data$retweets_count, data$b.topic, sum)


total3 <- cbind(tapply(data$likes_count, data$b.topic, sum), #Creamos un data frame con los vectores anteriores, dónde para cada columna (retweet, likes, replies) nos suma el total para cada temática
                tapply(data$replies_count, data$b.topic, sum),
                tapply(data$retweets_count, data$b.topic, sum))

colnames(total3)<-c("likes","replies","retweets") #Cambiar el nombre de las columnas

rownames(total3) <- c("Recreational fishing", "Recreational hunting") #Cambiar el nombre de las filas


####Crear barplot

install.packages("reshape2")
library(reshape2)

df.long<-melt(total3) #Recategoriza los datos, nos transpone la matriz y nos hace copias para fishing y hunting.

ggplot(df.long,aes(Var2,value,fill=Var1))+
  geom_bar(stat="identity",position="dodge")+ #dentro del ggplot llamo a una geometría de gráfico de barras
  scale_fill_manual(values = c("cadetblue", "coral3"),name = "Temática", labels = c("Pesca recreativa", "Caza recreativa")) +
  xlab("Interacciones sociales") +
  ylab("Número de interacciones")+
  ggtitle("Medida de impacto de los tuits a través del número de social engagements")+
  scale_y_continuous(limits = c(0, 120000), breaks = seq(0, 120000, by = 10000))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16)) +
  theme_classic()



