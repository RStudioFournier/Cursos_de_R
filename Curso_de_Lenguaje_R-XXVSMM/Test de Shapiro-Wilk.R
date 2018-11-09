#Prueba de Bondad de Ajuste Shapiro-Wilk
#Autor:Arnoldo Daniel Miranda Fournier
#Fecha:12/02/2018
#Fecha de ultima modificacion:16/10/2018

#La prueba de Shapiro-Wilk se considera uno de los test mas potentes, mas utilizados
#y eficientes para el contraste de normalidad, sobre todo para muestras pequeñas (n<50). 
#En caso de tener un tamaño de muestra mayor a 5000 pueden usarse alguna de las muchas
#pruebas de normalidad que hay.

#H_0:Los datos provienen de una distribucion Normal
#vs
#H_1:Los datos no provienen de una distribucion Normal

set.seed(10)

x1 <- rnorm(100) # Creamos una variable normal con 100 valores
(x.test <- shapiro.test(x1))
#Como p-value >= 0.05 no hay suficiente evidencia para rechazar la hipotesis de que los datos provienen de una distribucion normal.

x2 <- runif(100) # Creamos una variable con distribución uniforme (no normal) con 100 valores
(x2.test <- shapiro.test(x2))
#Como p-value < 0.05 hay suficiente evidencia para rechazar la hipotesis de que los datos provienen de una distribucion normal.

x3 <- rchisq(100, 20) # Creamos una variable con distribución Ji-Cuadrada (no normal) con 100 valores
(x3.test <- shapiro.test(x3))
#Como p-value < 0.05 hay suficiente evidencia para rechazar la hipotesis de que los datos provienen de una distribucion normal.

#plot.shapiro.test:Dado un conjunto de datos dibuja el histograma asociado y la densidad de una normal con media y des.est estimada con esos datos
plot.shapiro.test <- function(x, 
                              main = "Histograma de frecuencias \ny distribución normal",
                              xlab = "Datos", ylab = "Densidad"){
  media <- round(mean(x), 2)
  de <- round(sd(x), 2)
  hist(x, freq = FALSE, main = main, xlab = xlab, ylab = ylab, col = "lightblue")
  curve(dnorm(x, media, de), min(x), max(x), add = TRUE, col = "red", lwd = 3)
}

plot.shapiro.test(x1, main = "Histograma de datos provenientes \n de una distribucion normal")
plot.shapiro.test(x2, main = "Histograma de datos provenientes \n de una distribucion uniforme")
plot.shapiro.test(x3, main = "Histograma de datos provenientes \n de una distribucion Ji-Cuadrada")
