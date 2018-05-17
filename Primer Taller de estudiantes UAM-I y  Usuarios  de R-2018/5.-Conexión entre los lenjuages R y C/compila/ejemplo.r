# CARGANDO LIBRERÍAS Y FUNCIONES NECESARIAS
library(alr4)
library(doParallel)
library(foreach)
library(doRNG)
my_dir <- "C:/Users/Erwin/Dropbox/Cursos/UAM/TALLER_R/"
source(paste0(my_dir, "extras.r"))

# LOS DATOS A ANALIZAR
trabajo <- salarygov
names(trabajo) <- c("Tipo_trabajo", "mujeres_empleadas", "total_empleados", 
                    "dificultad", "salario_maximo")
plot(trabajo$dificultad , trabajo$salario_maximo, pch =19, 
     xlab = "Nivel de dificultad de la clase de trabajo", 
     ylab = "Salario máximo para la clase de trabajo")

plot(trabajo$dificultad , trabajo$salario_maximo, pch =19, 
     xlab = "Nivel de dificultad de la clase de trabajo", 
     ylab = "Salario máximo para la clase de trabajo")
identify(trabajo$dificultad , trabajo$salario_maximo, 
         labels = trabajo$Tipo_trabajo)


# DATOS DE ENTRENAMIENTO Y PREDICCIÓN
n <- nrow(trabajo)
k <- round(0.1*n, 0)
set.seed(1234)
id <- sample.int(n, k, replace = FALSE)
TRAIN <- trabajo[-id, ]
PRED <- trabajo[id, ]

plot(TRAIN$dificultad, TRAIN$salario_maximo, pch = 19, 
     xlab = "Dificultad", ylab = "Salario máximo", 
     axes = FALSE, xlim = c(0, 1200), ylim = c(1000, 9000))
points(PRED$dificultad, PRED$salario_maximo, col = "red", pch = 19)
axis(1, seq(0, 1200, by = 200))
axis(2, seq(1000, 9000, by = 1000))
box(lwd = 2)

###############################################################
# PREDICCIÓN ASUMIENDO QUE SE CUMPLEN LOS SUPUESTOS DE LA RLS #
###############################################################
m0 <- lm(salario_maximo ~ dificultad, data = TRAIN)
sm0 <- summary(m0)
sm0

prediccion <- predict.lm(m0, newdata = PRED, interval = "prediction", level = 0.95) 


plot(PRED$dificultad, PRED$salario_maximo, pch = 19, 
     xlab = "Dificultad", ylab = "Salario máximo", 
     axes = FALSE, xlim = c(0, 800), ylim = c(1000, 6000))
axis(1, seq(0, 1200, by = 200))
axis(2, seq(1000, 9000, by = 1000))
box(lwd = 2)
lines(PRED$dificultad, prediccion[,1], col = "red", lwd = 2)
lines(PRED$dificultad, prediccion[,2], col = "blue", lwd = 2)
lines(PRED$dificultad, prediccion[,3], col = "blue", lwd = 2)

#        Diagnósticos      #
y_hat <- fitted(m0)    # valores ajustados
e_hat <- residuals(m0) # residuales usuales

# Supuesto 1: varianza constante y linealidad
plot(y_hat, e_hat, pch = 19)
abline(h = 0, lty = 2, lwd = 2, col = "red")
abline(h = c(-1.5*sm0$sigma, 1.5*sm0$sigma), lwd = 2, col = "blue")

# Supuesto 2: normalidad
qqnorm(e_hat, pch = 19)
qqline(e_hat, col = "red", lwd = 2)

# Supuesto 3: independencia entre errores
idx <- 1:length(e_hat)
plot(idx, e_hat, pch = 20)
abline(h = 0, lty = 2)
abline(h = c(-1.5*sm0$sigma, 1.5*sm0$sigma), lwd = 2, col = "red")



#########################################
#  Predicción vía re-muestreo Bootstrap #
#########################################

# PROGRAMA EN R
m <- 100000
t1 <- system.time(res <- inferencia_boot_pred_R(m, TRAIN$dificultad, 
                                                   TRAIN$salario_maximo, 
                                                   PRED$dificultad))[3]
t1

# Comparando la predicción vía el modelo de RLS # 
#    - asumiendo que los supuestos ciertos -    #
par(mfrow = c(1, 2))
plot(PRED$dificultad, PRED$salario_maximo, pch = 19, 
     xlab = "Dificultad", ylab = "Salario máximo", 
     axes = FALSE, xlim = c(0, 800), ylim = c(1000, 6000))
axis(1, seq(0, 1200, by = 200))
axis(2, seq(1000, 9000, by = 1000))
box(lwd = 2)
lines(PRED$dificultad, prediccion[,1], col = "red", lwd = 2)
lines(PRED$dificultad, prediccion[,2], col = "blue", lwd = 2)
lines(PRED$dificultad, prediccion[,3], col = "blue", lwd = 2)
text(200, 5000, labels = "Predicción vía supuestos de RLS")

plot(PRED$dificultad, PRED$salario_maximo, pch = 19, 
     xlab = "Dificultad", ylab = "Salario máximo", 
     axes = FALSE, xlim = c(0, 800), ylim = c(1000, 6000))
axis(1, seq(0, 1200, by = 200))
axis(2, seq(1000, 9000, by = 1000))
box(lwd = 2)
lines(PRED$dificultad, res[,1], col = "red", lwd = 2)
lines(PRED$dificultad, res[,2], col = "blue", lwd = 2)
lines(PRED$dificultad, res[,3], col = "blue", lwd = 2)
text(200, 5000, labels = "Predicción vía Bootstrap")


# PROGRAMADO EN R CON PROCESAMIENTO EN PARALELO
detectCores()          # NÚMERO DE PROCESADORES QUE TIENE LA MAQUINA
c1 <- makeCluster(2)   # NÚMERO DE PROCESADORES QUE QUEREMOS USAR PARA EL PROCESO EN PARALELO
registerDoParallel(c1) # REGISTRANDO EL CLUSTER
getDoParWorkers()      # VERIFICAMOS
m <- 10000            # NUMERO DE REPLICAS BOOTSTRAP
t2 <- system.time(res2 <-inferencia_boot_pred_paralelo(m, TRAIN$dificultad, 
                                                          TRAIN$salario_maximo, 
                                                          PRED$dificultad))[3]
stopCluster(c1)
t2


plot(PRED$dificultad, PRED$salario_maximo, pch = 19, 
     xlab = "Dificultad", ylab = "Salario máximo", 
     axes = FALSE, xlim = c(0, 800), ylim = c(1000, 6000))
axis(1, seq(0, 1200, by = 200))
axis(2, seq(1000, 9000, by = 1000))
box(lwd = 2)
lines(PRED$dificultad, res2[,1], col = "red", lwd = 2)
lines(PRED$dificultad, res2[,2], col = "blue", lwd = 2)
lines(PRED$dificultad, res2[,3], col = "blue", lwd = 2)
text(200, 5000, labels = "Predicción vía Bootstrap")


# PROGRAMADO EN R Y C (el ciclo costoso en C)
dyn.load(paste(my_dir, 'pred.dll', sep=''))
m <- 1000000
t3 <- system.time(res3 <- inferencia_boot_pred_C(m, x = TRAIN$dificultad, 
                                                    y = TRAIN$salario_maximo, 
                                                    x_star = PRED$dificultad))[3]
t3


plot(PRED$dificultad, PRED$salario_maximo, pch = 19, 
     xlab = "Dificultad", ylab = "Salario máximo", 
     axes = FALSE, xlim = c(0, 800), ylim = c(1000, 6000))
axis(1, seq(0, 1200, by = 200))
axis(2, seq(1000, 9000, by = 1000))
box(lwd = 2)
lines(PRED$dificultad, res3[,1], col = "red", lwd = 2)
lines(PRED$dificultad, res3[,2], col = "blue", lwd = 2)
lines(PRED$dificultad, res3[,3], col = "blue", lwd = 2)
text(200, 5000, labels = "Predicción vía Bootstrap")