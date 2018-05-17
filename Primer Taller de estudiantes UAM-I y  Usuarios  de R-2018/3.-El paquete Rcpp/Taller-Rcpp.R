#Cargar la libreria Rcpp
library(Rcpp)

#Ejemplo sencillo
cppFunction('
            int sumar(int x, int y, int z){
            int sum=x+y+z;
            return sum;
            }
            ')

sumar   #mostrar la declaración/definición de la función creada

sumar(1,6,11)

#Comparacion entre R y C++
signoR<- function(x) {
  if(x > 0) {
    1
  } else if(x == 0) {
    0
  } else {
    -1
  }
}

cppFunction('
            int signoC(int x) {
              if (x > 0) {
                return 1;
              } else if (x == 0) {
                return 0;
              } else {
                return -1;
              }
            }
            ')

#Loops Vector -> Escalar
sumaR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

cppFunction('
            double sumaC(NumericVector x) {
              int n = x.size();
              double total = 0;
              for(int i = 0; i < n; ++i) {
                total += x[i];
              }
              return total;
            }
            ')

x<-runif(1e3)
library(microbenchmark)
microbenchmark(sum(x),
               sumaC(x),
               sumaR(x)
               )


#Distancia entre 1 valor y un vector de valores  Vector -> Vector
pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

cppFunction('
            NumericVector pdistC(double x, NumericVector ys) {
              int n = ys.size();
              NumericVector out(n);
            
              for(int i = 0; i < n; ++i) {
                out[i] = sqrt(pow(ys[i] - x, 2.0));
              }
              return out;
            }
            ')
x<-2
y<-runif(1e3)

microbenchmark(pdistR(x,y),
               pdistC(x,y))

# Matriz -> Vector
cppFunction('
            NumericVector rowSumC(NumericMatrix x) {
              int nrow = x.nrow(), ncol = x.ncol();
              NumericVector out(nrow);
            
              for (int i = 0; i < nrow; i++) {
                double total = 0;
                for (int j = 0; j < ncol; j++) {
                  total += x(i, j);
                }
                out[i] = total;
              }
            return out;
            }
            ')
set.seed(1)
x <- matrix(sample(100), 10)
rowSumC(x)

#uso de sourceCpp
sourceCpp("Taller-Rcpp.cpp")

#Error medio porcentual
modelo<-lm(mpg ~ wt, data = mtcars)
mpe(modelo)

#pasar funciones
callWithOne(function(x) x+6)
callWithOne(exp)

#STL (Standard Template Library)
#iteradores
#Guardar el valor de .end()

#Algoritmos

#Vectores
set.seed(9)
x<-runif(1e3)
int_conf<-findInterval2(x,c(0.025,0.975))
x[1:10]
int_conf[1:10]

# Run Length Encoding
lanzamoneda<-findInterval2(x,0.5)
rleC(lanzamoneda)

#mapa
x<-sample(1:20,1000,replace = T)
microbenchmark(table(x),tableC(x))

#Gibbs
gibbs_r <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

microbenchmark(
  gibbs_r(100, 10),
  gibbs_cpp(100, 10)
)

# Referencia
# Seamless R and C++ Integration with R, Dirk Edelbuettel, 2013, Springer
