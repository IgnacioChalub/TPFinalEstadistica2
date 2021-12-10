library(ggplot2)
library(readxl)
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
#
puntaje_1 <- na.omit(c(DATA$puntaje1))
mean(puntaje_1)
hist(puntaje_1)
puntaje_2 <- na.omit(c(DATA$puntaje2))
mean(puntaje_2)
hist(puntaje_2)
puntaje_3 <- na.omit(c(DATA$puntaje3))
mean(puntaje_3)
hist(puntaje_3)
puntaje_4 <- na.omit(c(DATA$puntaje4))
mean(puntaje_4)
hist(puntaje_4)
puntaje_5 <- na.omit(c(DATA$puntaje5))
mean(puntaje_5)
hist(puntaje_5)
puntaje_6 <- na.omit(c(DATA$puntaje6))
mean(puntaje_6)
hist(puntaje_6)

#puntaje_primera  --> media 27.75 y desvío 1.5
#puntaje_segunda  --> media 19.14 y desvío 1.5
#puntaje_tercera  --> media 12.97 y desvío 1.5
#puntaje_cuarta --> media 8.54 y desvío 1.5 
#puntaje_ quinta  --> media 4.47 y desvío 1.5 
#puntaje_sexta --> media 2.64 y desvío 1.5

muestras <- c()
for(i in 1:30000){
    cantidad_de_puntos <- 0 
    for ( i in 1:5){
      mejor_puntaje <- trunc(rnorm(1,mean(puntaje_mas_popular),4.284323)) 
      segundo_puntaje <- trunc(rnorm(1,mean(puntaje_segundo_mas_popular),2.985134)) 
      tercer_puntaje <- trunc(rnorm(1, 14, 2)) 
      cuarto_puntaje <- trunc(rnorm(1,7, 2)) 
      for(i in 1:2){
        puntajes <- c(mejor_puntaje, segundo_puntaje, tercer_puntaje, cuarto_puntaje, quinto_puntaje, sexto_puntaje)
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
perdedores <- muestras[muestras < 200]
length(ganadores)/1000*50000 + sum(perdedores)/1000 * 50



puntos_ <- na.omit(c(DATA$Total))
length(puntos_)
length(puntos_[puntos_ >= 200])

