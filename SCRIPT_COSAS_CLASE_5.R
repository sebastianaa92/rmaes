rm(list = ls ())
install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

datos = read.csv("/home/sebastian/Downloads/Saber.csv")

datos_limpios = na.omit(datos)


set.seed(123)
n=1000
muestra = datos_limpios[sample(1:nrow(datos_limpios),n),]
head(muestra)

summary(muestra$PUNT_MATEMATICAS)
table(muestra$ESTU_GENERO)

#HISTOGRAMA DE FRECUENCUIAS

histograma=ggplot(muestra, aes(x = PUNT_MATEMATICAS)) + 
  geom_histogram(binwidth = 10, fill = "#861627", color = "white") + 
  labs(title = "Histograma de Puntaje en Matemáticas", 
       x = "Puntaje en Matemáticas", 
       y = "Frecuencia") +
  stat_bin(binwidth = 10, geom = "text", aes(label = after_stat(count)), vjust = -0.5, color = "black") + 
  theme_minimal()+
  ylim(c(0,350))+
  geom_density(aes(y = ..count.. * 10), color = "blue", linewidth = 1) # La multiplicación ajusta la escala de la curva
histograma

#GRAFICO DEE CAJAS Y BIGOTES

boxplot = ggplot(muestra, aes(y = PUNT_MATEMATICAS))+
  geom_boxplot(fill = "lightgreen", color = "black")+
  labs(title = "Boxplot para el puntajde en matemáticas",
       y = "Puntaje")+
  theme_minimal()
boxplot

install.packages("ggplot2")


#QQPLOT permite comparar lo que teoricamente debería pasar en una distro normal, versus lo que realmente está pasando

qqplot <- ggplot(muestra, aes(sample = PUNT_MATEMATICAS)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "QQ Plot para el Puntaje en Matemáticas",
       x = "Teóricos",
       y = "Muestra") +
  theme_minimal()
qqplot  


grid.arrange(histograma,boxplot, qqplot, ncol = 3)


#¿EXISTE UNA DIFERENCIA SIGNIFICATIVA ENTRE EL PUNTAJE PROMEDIO DE MATEMÁTICAS DE HOMBRES Y MUJERES?

#PRIMER PASO: COMPARAR VARIANZAS
#DISTRO F de FISHER: Siempre va a aparecer para comparar varianzas, se parece a chi cuadrado

mujeres = muestra[which(muestra$ESTU_GENERO == "F"),]
mujeres

hombres = muestra[which(muestra$ESTU_GENERO == "M"),]
hombres

#VAMOS A CREAR LOS INTERVALOS DE CONFIANZA PARA LAS VARIANZAS

ic_var = var.test(mujeres$PUNT_MATEMATICAS,hombres$PUNT_MATEMATICAS, conf.level = 0.95)
ic_var$conf.int

#SEGUNDO PASO: COMPARAR MEDIAS

ic_medias = t.test(mujeres$PUNT_MATEMATICAS,
                   hombres$PUNT_MATEMATICAS,
                   var.equal = F,
                   conf.level = 0.95)
ic_medias$conf.int
#Se concluye que las medias son significativamente distintas ya que el 0 no está en el intervalo
#Como los 2 limites del intervalo son negativos, el pujntaje de matematicas en promedio es mayor
#ES IMPORTANTE USAR EL MISMO ORDEN DE COMPARACIÓN DE VARIANZAS 

#VAMOS AHORA A HACER UN INTERVALO QUE NOS COMPARE PROPORCIONES. SON LOS INTERVALOS QUE MÁS NOS REPORTAN RESULTADOS EN UNA ENCUESTAS
#O ESTUDIOS, NO COMO UNA PROPORCIÓN, SI NO COMO UN PORCENTAJE, SOLO QUE EL % NO ES UNA MEDIDA ESTADÍSTICA, LO QUE SI ES UNA
#PROPORCIÓN, LA CUAL NOS MUESTRA ELEMENTOS FAVORABLES O CUENTA CUANTOS ELEMEENTOS CUMPLEN UNA CONDICIÓN DADA

#¿EXISTE UNA DIFERENCIA SIGNIFICATIVA ENTRE LA PROPORCIÓN DE HOMBRES Y MUJERES CUYOS PUNTAJES EN MATEMÁTICAS SUPERAN LA MEDIA?

media = mean(muestra$PUNT_MATEMATICAS)
media

n1 = nrow(mujeres)
n2 = nrow(hombres)

x1 = length(which(mujeres$PUNT_MATEMATICAS > media))
x2 = length(which(hombres$PUNT_MATEMATICAS > media))

ic_prop = prop.test(c(x1,x2),c(n1,n2), conf.level = 0.95 )
ic_prop$conf.int

#LA CONCLUSIÓN ES QUE SI EXISTE UNA DIFERENCIA SIGNIFICATIVA, CUYO PUNTAJE ESTÁ POR ENCIMA DE LA MUESTRA
# Mujeres - Hombres < 0  -----------> Como el resultado fue negativo, se infiere que el valor de la izquierda, en este caso mujeres
#por lo que van a salir valores negativos.