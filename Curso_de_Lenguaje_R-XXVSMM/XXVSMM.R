#######################################################################################
#Ambiente de Trabajo####
getwd()
setwd("~/Dropbox/Codigos en R/UAM/Curso_de_Lenguaje_R-XXVSMM")
getwd()
#######################################################################################
#1.-Importacion y creacion de bases de datos####

dat1 <- data.frame(N = c("Lucia", "Ernesto", "Sofia", "Eduardo")
                   , Edades = c(19, 23, 34, 43))

dat2 <- data.frame(Estatura = c(1.73, 1.50, 1.45, 1.80), 
                   Peso = c(73, 80, 56, 84))

dat3 <- data.frame(dat1, dat2)

dat1; dat2

dat3

dat3[2, 4]

dat3[2, 4] <- 81.3

dat3$Estatura

#######################################################################################
#2.-Operaciones y funciones basicas de R####

sum(dat3$Estatura)

prod(dat3$Edades)

mean(dat3$Peso)

var(dat3$Estatura)

sd(dat3$Estatura)

median(dat3$Peso)

sort(dat3$Peso)

max(dat3$Edades)

min(dat3$Edades)

range(dat3$Edades)

summary(dat3)


data("iris")
head(iris)

iris$Sepal.Length

attach(iris)
Sepal.Length
detach(iris)

class(iris)
class(iris$Sepal.Length)
class(iris$Species)

#Variable Continua



summary(iris)

hist(iris$Sepal.Length)

hist(iris$Sepal.Length, main = "Histograma Iris \n Sepalo", xlab = expression(x^2), ylab = "Frecuencia", col = "blue")

if(1){
  Spe <- iris$Petal.Width ; dhSpe <- hist(Spe, plot = FALSE)$density
  hist(Spe, freq = FALSE, ylim = c(-max(dhSpe)/10, max(dhSpe)), col = "gray88", main = "", xlab = "", ylab = "")
  set.seed(1)
  points(Spe, runif(length(Spe), -max(dhSpe)/10, -max(dhSpe)/40), col = iris$Species,  cex = 1.5, pch = 20)
  title(main = "Histograma del ancho del Petalo", xlab = "Ancho del Petalo", ylab = "Frecuencia")
  legend("topright", levels(iris$Species), lty = c(-1, -1, -1), pch = c(20, 20, 20), lwd = c(1.5, 1.5, 1.5), merge = TRUE, col = c(1:3), cex = 1, pt.cex = 1.5, x.intersp = .7, y.intersp = 1)
}

if(1){
  Spe <- iris$Sepal.Width ; dhSpe <- hist(Spe, plot = FALSE)$density
  hist(Spe, freq = FALSE, ylim = c(-max(dhSpe)/10, max(dhSpe)), col = "gray88", main = "", xlab = "", ylab = "")
  set.seed(1)
  points(Spe, runif(length(Spe), -max(dhSpe)/10, -max(dhSpe)/40), col = iris$Species,  cex = 1.5, pch = 20)
  title(main = "Histograma del ancho del Sepalo", xlab = "Ancho del Sepalo", ylab = "Frecuencia")
  legend("topright", levels(iris$Species), lty = c(-1, -1, -1), pch = c(20, 20, 20), lwd = c(1.5, 1.5, 1.5), merge = TRUE, col = c(1:3), cex = 1, pt.cex = 1.5, x.intersp = .7, y.intersp = 1)
}

#Variable Categorica

table(iris$Species)

(lev <- levels(iris$Species))
levels(iris$Species) <- c("Tipo1", "Tipo2", "Tipo3")
levels(iris$Species) <- lev

Localidad <- gl(n = 3, k = 8, length = 150, labels = c("A", "B", "C"))
Localidad[3]

sample(1:150, 150, replace = TRUE)

Localidad <- Localidad[sample(1:150, 150, replace = TRUE)]

iris$Localidad <- Localidad

#Tabla de Frecuencias
(tabla <- as.data.frame(table(Localidad = iris$Localidad)))
options(digits = 2)
(tabla <- transform(tabla, Freq.Acum = cumsum(Freq), Freq.Rel = prop.table(Freq)))
(tabla <- transform(tabla, Freq.Rel.Acum = cumsum(Freq.Rel)))
(tabla <- transform(tabla, Freq.Porc = 100*Freq.Rel, Freq.Porc.Acum = 100*Freq.Rel.Acum))

#Tabla de Contingencia
table(Species = iris$Species, Localidad = iris$Localidad)

#Grafico de Barras
barplot(table(iris$Localidad))

barplot(table(iris$Localidad))
barplot(sort(table(iris$Localidad)), col = "Lightblue", main = "Grafica de Barras \n Iris", xlab = "Localidad", ylab = "Frecuencias Absolutas")

#Grafico de Pastel
pie(table(iris$Localidad))
pie(tabla$Freq.Porc, main=c("Grafico de Pastel"), labels = paste(tabla$Localidad,paste(round(tabla$Freq.Porc, digits = 2), "%", sep = "")))
#######################################################################################
#3.-Distribuciones probabilisticas####

#######################################################################################
#4.-Programacion con R####

#######################################################################################