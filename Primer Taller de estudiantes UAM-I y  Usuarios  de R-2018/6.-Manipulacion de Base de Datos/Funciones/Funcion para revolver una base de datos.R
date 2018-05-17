#Funcion para revolver de manera aleatoria una base de datos.
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 20/04/2018
#Fecha de ultima modificacion: 20/04/2018
#Argumentos:
#Base.-Nombre de la base como se encuentra en el Environment.

Stir <- function(Base){
  
  if(is.data.frame(Base)){
    
    return(data.frame(Base[sample.int(dim(Base)[1]), ], row.names = NULL))
    
  }else{
    
    cat("El objeto que ingresaste no es una base de datos, por favor confirma el objeto o la clase del mismo.")
    
    return(NA)
    
  }
  
}


#Ejemplo:
 data <- data.frame(Nombres = c("ACOSTA HERRERA ROSA ISELA", "AGUILAR MEDEL ELIZABETH", "BUTRON POPOCA BRUNO RICARDO", "GONZALEZ SANDOVAL CESAR ARTURO", "LOPEZ ROMERO FLOR JOHANA", "MORENO NIETO JUDITH MARIELA", "RAMIREZ VAZQUEZ ANA", "SERRANO HERNANDEZ ROCIO", "VAZQUEZ VILCHIS JORGE", "ZUÑIGA CAMBRAY VICTOR HUGO")
                 , Calificacion = sample(seq(5, 10, by = .3), 10))
data2 <- Stir(data)
#View(data2)
