#Funcion para extraer una muestra aleatoria de tamano n de una base de datos de tamano N.
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 20/04/2018
#Fecha de ultima modificacion: 20/04/2018
#Argumentos:
#Base.-Nombre de la base como se encuentra en el Environment.
#n.-El tamano de la muestra a extraer de la base de datos, deber ser un numero entre 0 y el tamano total de la base de datos.

Sampling_N <- function(Base, n){
  
  if(is.data.frame(Base)){
    
    if(0 <= n & n <= dim(Base)[1]){
      
      return(data.frame(Base[sample.int(dim(Base)[1], n), ], row.names = NULL))
      
    }else{
      
      cat("El valor ingresado para el parametro \"n\" no es correcto, por favor confirma el valor del mismo.")
      
      return(NA)
      
    }
    
  }else{
    
    cat("El objeto que ingresaste no es una base de datos, por favor confirma el objeto o la clase del mismo.")
    
    return(NA)
    
  }
  
}


#Ejemplo:
#data <- data.frame(Nombres = c("ACOSTA HERRERA ROSA ISELA", "AGUILAR MEDEL ELIZABETH", "BUTRON POPOCA BRUNO RICARDO", "GONZALEZ SANDOVAL CESAR ARTURO", "LOPEZ ROMERO FLOR JOHANA", "MORENO NIETO JUDITH MARIELA", "RAMIREZ VAZQUEZ ANA", "SERRANO HERNANDEZ ROCIO", "VAZQUEZ VILCHIS JORGE", "ZUÑIGA CAMBRAY VICTOR HUGO")
#                 , Calificacion = sample(seq(5, 10, by = .3), 10))
#data2 <- Sampling_N(data, 7)
#View(data2)
