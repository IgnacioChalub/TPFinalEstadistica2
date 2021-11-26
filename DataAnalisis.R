library(ggplot2)
library(readxl)
DATA = read_xlsx("~/faculty/estadistica2/TPFinalEstadistica2/DATOSPROGRAMAS.xlsx")
View(DATA)

puntos_partidas <- append(DATA$PUNTOS_PRIMERA_PARTIDA, DATA$PUNTOS_SEGUNDA_PARTIDA)
puntos_dinero_rapido_partidas <- append(DATA$PUNTOS_PRIMER_DINERO_RAPIDO, DATA$PUNTOS_SEGUNDO_DINERO_RAPIDO)
dinero_partidas <- append(DATA$DINERO_PRIMERA_PARTIDA, DATA$DINERO_SEGUNDA_PARTIDA)
dinero_por_dia <- DATA$DINERO_PRIMERA_PARTIDA + DATA$DINERO_SEGUNDA_PARTIDA

#density puntos dinero rapido por partidas
x <- puntos_dinero_rapido_partidas
density_puntos_dinero_rapido <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1)+
  labs(title = "Puntos dinero rapido por partida", x="Puntos")
density_puntos_dinero_rapido

#density puntos por partida
x <- puntos_partidas
density_puntos <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) +
  labs(title = "Puntos por partida", x="Puntos") 
density_puntos

#density dinero por partida
x <- dinero_partidas
density_dinero_partidas <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
  labs(title = "Dinero por partida", x="Dinero")
density_dinero_partidas

#density dinero por partida sin 50k
x = dinero_partidas[dinero_partidas != 50000] 
density_dinero_partidas_sin_50k <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
  labs(title = "Dinero por partida sin 50000", x="Dinero")
density_dinero_partidas_sin_50k

#density dinero por dia 
x <- dinero_por_dia
density_dinero_por_dia <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
  labs(title = "Dinero por dia", x="Dinero")
density_dinero_por_dia

#relacion puntos por partida y puntos en dinero rapido
modelo <- lm(puntos_dinero_rapido_partidas~puntos_partidas)
summary(modelo)
abline(plot(puntos_partidas,puntos_dinero_rapido_partidas, col="blue", xlab = "Puntos partida", ylab = "Puntos en dinero rapido"))
abline(modelo)

