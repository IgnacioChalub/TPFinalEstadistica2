library(ggplot2)
library(readxl)
DATA = read_xlsx("~/faculty/estadistica2/TPFinalEstadistica2/DATOSPROGRAMAS.xlsx")
View(DATA)

puntos_partidas = append(DATA$PUNTOS_PRIMERA_PARTIDA, DATA$PUNTOS_SEGUNDA_PARTIDA)
puntos_dinero_rapido_partidas = append(DATA$PUNTOS_PRIMER_DINERO_RAPIDO, DATA$PUNTOS_SEGUNDO_DINERO_RAPIDO)
dinero_partidas = append(DATA$DINERO_PRIMERA_PARTIDA, DATA$DINERO_SEGUNDA_PARTIDA)
dinero_por_dia = DATA$DINERO_PRIMERA_PARTIDA + DATA$DINERO_SEGUNDA_PARTIDA

#density puntos dinero rapido por partidas
x = puntos_dinero_rapido_partidas
density_puntos_dinero_rapido <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue")
density_puntos_dinero_rapido
density_puntos_dinero_rapido+ geom_vline(aes(xintercept=mean(x)),
              color="blue", linetype="dashed", size=1)

#density puntos por partida
x = puntos_partidas
density_puntos <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue")
density_puntos
density_puntos+ geom_vline(aes(xintercept=mean(x)),
                                         color="blue", linetype="dashed", size=1)

#density dinero por partida sin 50k
x = dinero_partidas[dinero_partidas != 50000] 
density_dinero_partidas_sin_50k <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue")
density_dinero_partidas_sin_50k
density_dinero_partidas_sin_50k+ geom_vline(aes(xintercept=mean(x)),
                           color="blue", linetype="dashed", size=1)

#density dinero por dia 
x = dinero_por_dia
density_dinero_partidas_sin_50k <- ggplot(data.frame(x), aes(x=x)) + 
  geom_density(color="darkblue", fill="lightblue")
density_dinero_partidas_sin_50k
density_dinero_partidas_sin_50k+ geom_vline(aes(xintercept=mean(x)),
                                            color="blue", linetype="dashed", size=1)

#relacion puntos por partida y puntos en dinero rapido
plot(puntos_partidas,puntos_dinero_rapido_partidas, col="blue", xlab = "Puntos partida", ylab = "Puntos en dinero rapido")
