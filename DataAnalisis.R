library(ggplot2)
library(readxl)
#tinytex::install_tinytex()
DATA = read_xlsx("~/faculty/estadistica2/TPFinalEstadistica2/Muestreo 100 Args.xlsx")
View(DATA)

puntaje_mas_popular <- na.omit(append(c(DATA$`Puntaje Mas PopularPuntaje Mas Popular1`), c(DATA$`Puntaje Mas PopularPuntaje Mas Popular`)))
puntaje_mas_popular <- na.omit(append(puntaje_mas_popular, c(DATA$`Puntaje Mas Popular2`)))
puntaje_mas_popular <- na.omit(append(puntaje_mas_popular, c(DATA$`Puntaje Mas Popular3`)))
puntaje_mas_popular <- na.omit(append(puntaje_mas_popular, c(DATA$`Puntaje Mas Popular4`)))                               
hist(puntaje_mas_popular, 
     main="Histograma de puntajes mas populares", 
     xlab="Puntajes", 
     ylab= "Frecuencia",
     border="darkblue", 
     col="lightblue")
mean(puntaje_mas_popular)
sd(puntaje_mas_popular)
puntaje_segundo_mas_popular <- na.omit(append(c(DATA$`Puntaje Segunda Mas Popular`), c(DATA$`Puntaje Segunda Mas Popular1`)))
puntaje_segundo_mas_popular <- na.omit(append(puntaje_segundo_mas_popular, c(DATA$`Puntaje Segunda Mas Popular2`)))
puntaje_segundo_mas_popular <- na.omit(append(puntaje_segundo_mas_popular, c(DATA$`Puntaje Segunda Mas Popular3`)))
puntaje_segundo_mas_popular <- na.omit(append(puntaje_segundo_mas_popular, c(DATA$`Puntaje Segunda Mas Popular4`)))                               
hist(puntaje_segundo_mas_popular, 
     main="Histograma de segundo puntajes mas populares", 
     xlab="Puntajes", 
     ylab= "Frecuencia",
     border="darkblue", 
     col="lightblue")
mean(puntaje_segundo_mas_popular)
sd(puntaje_segundo_mas_popular)
puntaje_malas <- na.omit(c(DATA$`Numero de Respuestas Malas 0-7`))
puntaje_buenas <- na.omit(c(DATA$`Numero de Mas Populares`))
puntaje_medias <- na.omit(c(DATA$`Numero de Segunda Mas Populares`))
sum(puntaje_malas)
sum(puntaje_buenas) + sum(puntaje_malas) + sum(puntaje_medias)

muestras <- c()
for(i in 1:40000){
    cantidad_de_puntos <- 0 
    for ( i in 1:5){
      mejor_puntaje <- trunc(rnorm(1,mean(puntaje_mas_popular),4.284323)) 
      segundo_puntaje <- trunc(rnorm(1,mean(puntaje_segundo_mas_popular),2.985134)) 
      tercer_puntaje <- trunc(rnorm(1, 14, 2)) 
      cuarto_puntaje <- trunc(rnorm(1,7, 2)) 
      for(i in 1:2){
        puntajes <- c(mejor_puntaje, segundo_puntaje, tercer_puntaje, cuarto_puntaje)
        n <- runif(n=1, min=0, max = 100) 
        if (n < mejor_puntaje){
          cantidad_de_puntos <- mejor_puntaje + cantidad_de_puntos
        }else if(  n > mejor_puntaje & n < mejor_puntaje + segundo_puntaje){
          cantidad_de_puntos <- segundo_puntaje + cantidad_de_puntos
        }else if( n > mejor_puntaje + segundo_puntaje & n < mejor_puntaje + segundo_puntaje + tercer_puntaje){
          cantidad_de_puntos <- tercer_puntaje + cantidad_de_puntos
        }else if( n > mejor_puntaje + segundo_puntaje + tercer_puntaje & n < mejor_puntaje + segundo_puntaje + tercer_puntaje + cuarto_puntaje){
          cantidad_de_puntos <- cuarto_puntaje + cantidad_de_puntos
        }else{
          cantidad_de_puntos <- cantidad_de_puntos
        }
      }
    }
    muestras <- append(muestras, cantidad_de_puntos)
  }

ganadores <- muestras[ muestras >= 200 ] 
probabilidad_de_ganar <- length(ganadores)/40000
diner_ganadores <- length(ganadores)/1000 * 50000
perdedores <- muestras[muestras < 200]
dinero_perdedores <- sum(perdedores)/1000 * 50
dinero_por_mes <- diner_ganadores + dinero_perdedores

puntos <- na.omit(c(DATA$Total))
proporcion_ganadas <- length(puntos[puntos >= 200])/length(puntos)

                            
                            
