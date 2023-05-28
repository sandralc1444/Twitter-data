###########################PARTE 2: ANÁLISI TUITS#########################
## Excel: a.topic.xlsx (sacado de dy, generado al documento 1) con las categorías y el excel base con la columna de tuits. Los unimos con cbind

library(readxl)
tuits <- read_excel("a.topic.xlsx")

#Dividir cada tweet en palabras (tokens)

text<-as.character(text$tweet)
hamlet <- tibble(line=1:length(text), text=text)
counts <- hamlet %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n > 75)
hamlet
counts
summaryHU<-data.frame(counts)
write.csv(summaryHU, "summaryHU.csv")

####Analizar manualmente los tokens para cada categoría manualmente mediante Excel##

#######Cargar el Excel creado con los tokens clave: tokens.xlsx

####################GRÁFICAS TOKENS#######################
##########GRÁFICO DE BARRAS ESPECIES
#######################Pesca Recreativa###########################
library(readxl)
clases_pesca <- read_excel("tokens.xlsx", sheet = 1)

library(ggplot2)
library(dplyr)

clases_pesca %>% ggplot(aes(Frecuencia, Especie, fill="#b24000")) + geom_col() + labs(y=NULL) #####Versión básica

clases_pesca %>%
  ggplot(aes(Frecuencia, Especie)) +
  geom_col(fill = "cadetblue") +
  labs(title = "Frecuencia de especies agrupadas por clases para pesca recreativa",
       x = "Número de tokens",
       y = "Clase") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(),
        legend.position = "none") +
  scale_fill_manual(values = "cadetblue") +
  scale_x_continuous(breaks = seq(0, max(12000), by = 1000),
                     limits = c(0, max(12500)))


############################Caza Recreativa###########################

especies_caza <- read_excel("tokens.xlsx", sheet = 2)

especies_caza %>%
  ggplot(aes(Frecuencia, species)) +
  geom_col(fill = "coral3") +
  labs(title = "Frecuencia de individuos agrupados por especies para caza recreativa",
       x = "Número de tokens",
       y = "Especies") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(),
        legend.position = "none") +
  scale_fill_manual(values = "coral3") +
  scale_x_continuous(breaks = seq(0, max(1000), by = 150),
                     limits = c(0, max(1000)))


###########Combinar dos ggplot
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

# Unir las dos gráficas en dos columnas
grid.arrange(plot1, plot2, ncol = 2) ###tenemos que ponerle nombre a nuestros dos plots: plot1 <- ggplot….


##############################CATEGORÍAS###############################
###############################Conservación###############################
install.packages("reshape2")
library(reshape2)
cons <- read_excel("tokens.xlsx", sheet = 3)

data<-melt(cons)

ggplot(data, aes(x=tokens, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_cartesian(ylim = c(0, 1600)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 160)) +
  labs(title = "Frecuencia de palabras clave en relación a la conservación", x = "Tokens", y="Número de tokens", fill = "Temática") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(values=c("cadetblue","coral3"), labels=c("Pesca recreativa", "Caza recreativa"))


###############################Gestión#################################
mang <- read_excel("tokens.xlsx", sheet = 4)

data2 <-melt(mang)

ggplot(data2, aes(x=tokens, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_cartesian(ylim = c(0, 4500)) +
  scale_y_continuous(breaks = seq(0, 4500, by = 500)) +
  labs(title = "Frecuencia de palabras clave en relación a la gestión", x = "Tokens", y="Número de tokens", fill = "Temática") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(values=c("cadetblue","coral3"), labels=c("Pesca recreativa", "Caza recreativa"))


##########WORDCLOUD

install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(readxl)
library(tm)
library(wordcloud)
library(RColorBrewer)

Sys.setenv(TZ = "Europe/Barcelona")

#cargar datos fichero
data_wordcloud <- read_excel("C:/Users/annas/OneDrive/Documents/CURS DATA SCIENCE UB 2023/Treball Data Science/Wordcloud/Data_Wordcloud.xlsx")


freq_total <- c(data_wordcloud$frecuencia_F, data_wordcloud$frecuencia_H)
Tokens<-c(data_wordcloud$tokens)

par(mar = rep(0, 4))
wordcloud(words = Tokens, freq = freq_total, random.order = FALSE, colors = brewer.pal(8, "Dark2"))



