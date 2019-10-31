getwd()
setwd("")
ls()


x <- 5

y <- c(0, 1)
y
x

z <- 1:50
z

iris

rm(y, z)

View(iris)

colnames(iris)

dim(iris)
?matrix
a <- matrix(1:9, 3, 3)

library(ggplot2)

(p <- ggplot(iris))

p <- p + 
  aes(x = Petal.Length, y = Petal.Width)

p <- p +
  geom_point()

p  <- p + aes(colour = Species)

plot(iris$Petal.Length, iris$Petal.Width)



attach(iris)
plot(Petal.Length, Petal.Width, pch = 20, col = Species)
detach(iris)
