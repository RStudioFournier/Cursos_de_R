#Funcion para ordenar una base de datos con respecto a una variable cuantitativa.
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 20/04/2018
#Fecha de ultima modificacion: 20/04/2018
#Argumentos:
#Base.-Nombre de la base como se encuentra en el Environment.
#Variable.-Nombre de la variable cuantitativa entre comillas por la cual se hara la ordenacion
#Decreasing.-Argumento logico que indica si el ordenamiento sera en orden decreciente por defecto es FALSO


Sort.D.F <- function(Base, Variable, Decreasing = FALSE){
  
  if(is.data.frame(Base)){
    
    if(sum(Variable == names(Base))){
      
      return(data.frame(Base[order(Base[,Variable], decreasing = Decreasing), ], row.names = NULL))
      
    }else{
      
      cat("El nombre de la variable ingresada no coincide con ninguna de la base ingresada, por favor confirma el nombre la misma.")
      
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
data2 <- Sort.D.F(data, "Calificacion")
#View(data2)
data3 <- Sort.D.F(data, "Calificacion", TRUE)
#View(data3)
