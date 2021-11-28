library(ggplot2)
library(readxl)
DATA = read_xlsx("~/faculty/estadistica2/TPFinalEstadistica2/DATOSPROGRAMAS.xlsx")
View(DATA)

puntos_partidas <- append(DATA$PUNTOS_PRIMERA_PARTIDA, DATA$PUNTOS_SEGUNDA_PARTIDA)
puntos_dinero_rapido_partidas <- append(DATA$PUNTOS_PRIMER_DINERO_RAPIDO, DATA$PUNTOS_SEGUNDO_DINERO_RAPIDO)
dinero_partidas <- append(DATA$DINERO_PRIMERA_PARTIDA, DATA$DINERO_SEGUNDA_PARTIDA)
dinero_por_dia <- DATA$DINERO_PRIMERA_PARTIDA + DATA$DINERO_SEGUNDA_PARTIDA


x <- sort(puntos_dinero_rapido_partidas)

chisq.test(x, rnorm(length(x),mean(x),sd(x)))

ks.test(x,"pnorm",mean(x),sd(x))
qqnorm(x)
shapiro.test(x)
library(nortest)
pearson.test(x)
sf.test(x)
ad.test(x)

#density puntos dinero rapido por partidas
x <- puntos_dinero_rapido_partidas
density_puntos_dinero_rapido <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1)+
  labs(title = "Puntos dinero rapido por partida", x="Puntos")
density_puntos_dinero_rapido
histogram_puntos_dinero_rapido <- qplot(x, geom = "histogram", binwidth = 5, main = "Histograma de puntos en dinero rapido", xlab = "Puntos", fill = I("blue"), col = I("black"), alpha = I(.2), xlim = c(50, 250))
histogram_puntos_dinero_rapido

x <- puntos_dinero_rapido_partidas
density_puntos_dinero_rapido <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1)+
  labs(title = "Puntos dinero rapido por partida", x="Puntos")
density_puntos_dinero_rapido

#density de distribucion teorica con misma media y desvio
x <- rnorm(150, mean = mean(puntos_dinero_rapido_partidas), sd = sd(puntos_dinero_rapido_partidas))
density_puntos <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) +
  labs(title = "Puntos por partida", x="Puntos") 
density_puntos
histogram_puntos <- qplot(x, geom = "histogram", binwidth = 20, main = "Histograma de puntos en la partida", xlab = "Puntos", fill = I("blue"), col = I("black"), alpha = I(.2), xlim = c(100, 600))
histogram_puntos

#density dinero por partida
x <- dinero_partidas
density_dinero_partidas <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
  labs(title = "Dinero por partida", x="Dinero")
density_dinero_partidas
histogram_dinero_partidas <- qplot(x, geom = "histogram", binwidth = 1000, main = "Histograma de dinero ganado", xlab = "Puntos", fill = I("blue"), col = I("black"), alpha = I(.2), xlim = c(1000, 51000))
histogram_dinero_partidas

#density dinero por partida sin 50k
x = dinero_partidas[dinero_partidas != 50000] 
density_dinero_partidas_sin_50k <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
  labs(title = "Dinero por partida sin 50000", x="Dinero")
density_dinero_partidas_sin_50k
histogram_dinero_partidas_sin_50k <- qplot(x, geom = "histogram", binwidth = 500, main = "Histograma de dinero ganado sin 50k", xlab = "Puntos", fill = I("blue"), col = I("black"), alpha = I(.2), xlim = c(0, 10500))
histogram_dinero_partidas_sin_50k

#density dinero por dia 
x <- dinero_por_dia
density_dinero_por_dia <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
  labs(title = "Dinero por dia", x="Dinero")
density_dinero_por_dia
histogram_dinero_por_dia <- qplot(x, geom = "histogram", binwidth = 2000, main = "Histograma de dinero por dia", xlab = "Puntos", fill = I("blue"), col = I("black"), alpha = I(.2), xlim = c(0, 102000))
histogram_dinero_por_dia

#relacion puntos por partida y puntos en dinero rapido
modelo <- lm(puntos_dinero_rapido_partidas~puntos_partidas)
summary(modelo)
abline(plot(puntos_partidas,puntos_dinero_rapido_partidas, col="blue", xlab = "Puntos partida", ylab = "Puntos en dinero rapido"))
abline(modelo)

#Proporciones partidas con y sin 50k
partidas_con_50k <- dinero_partidas[dinero_partidas == 50000]
partidas_sin_50k <- dinero_partidas[dinero_partidas != 50000]
data <- data.frame(Grupos= c(" > 50k", " < 10k "),
  Porcentajes=c(length(partidas_con_50k)/length(dinero_partidas)*100, length(partidas_sin_50k)/length(dinero_partidas)*100)
)
ggplot(data, aes(x="", y=Porcentajes, fill=Grupos)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#Proporcion de plata ganada con 50k y sin
plata_con_50k <- dinero_partidas[dinero_partidas == 50000]
plata_sin_50k <- dinero_partidas[dinero_partidas != 50000]
data <- data.frame(Grupos= c("50k", " < 10k"),
                   Porcentajes=c(sum(plata_con_50k)/sum(dinero_partidas)*100, sum(plata_sin_50k)/sum(dinero_partidas)*100)
)
ggplot(data, aes(x="", y=Porcentajes, fill=Grupos)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


