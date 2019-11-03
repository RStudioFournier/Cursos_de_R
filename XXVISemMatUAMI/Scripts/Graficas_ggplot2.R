################################################################################
library(ggplot2)

data("iris")

p <- ggplot(iris)

p <- p + 
  aes(x = Petal.Length, y = Petal.Width, colour = Species)

p <- p +
  geom_point()

p
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, colour = Species),
    iris
  )

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, colour = Species),
    iris,
    show.legend = FALSE
  )

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, colour = Species),
    iris,
    shape = 21
  )

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  )

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
    color = "green"
  )

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"))

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"),
                    name = NULL)

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"),
                    name = "Esp. de Flor Iris")

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"),
                    name = "Esp. de Flor Iris",
                    labels = c("Especie 1", "Especie 2", "Especie 3"))

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"),
                    name = "Esp. de Flor Iris",
                    labels = c("Especie 1", "Especie 2", "Especie 3")) +
  ggtitle("                        Primer gráfico con ggplot2")

g
################################################################################
g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21,
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"),
                    name = "Esp. de Flor Iris",
                    labels = c("Especie 1", "Especie 2", "Especie 3"))+
  ggtitle("Primer gráfico con ggplot2",
          subtitle = "Mi nombre") +
  xlab("x") +
  ylab("y")

g
################################################################################
Titulo <- "Gráfica"
Subtitulo <- "B"
Titulo_x <- "C"
Titulo_y <- "D"
Titulo_Leyenda <- "E"
Leyenda_1 <- "F"
Leyenda_2 <- "G"
Leyenda_3 <- "H"

g <- ggplot() +
  geom_point(
    aes(x = Petal.Length, y = Petal.Width, fill = Species),
    iris,
    shape = 21
  ) +
  scale_fill_manual(values = c("yellow", "blue", "green"),
                    name = Titulo_Leyenda,
                    labels = c(Leyenda_1, Leyenda_2, Leyenda_3)) +
  ggtitle(Titulo,
          subtitle = Subtitulo) +
  xlab(Titulo_x) +
  ylab(Titulo_y)

g
################################################################################
dat <- data.frame(x = rnorm(1000))

g <- ggplot() +
  geom_histogram(aes(x = x, y = ..density..),
                 dat,
                 bins = nclass.Sturges(dat$x))

g
################################################################################