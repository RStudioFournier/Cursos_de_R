#Funcion para insertar una(s) columna(s) en la posicion deseada.
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 06/05/2018
#Fecha de ultima modificacion: 06/05/2018
#Argumentos:
#Base1.-Base a la que le quieres insertar una(s) columna(s).
#Base2.-Base que contiene la(s) columna(s) a insertar.
#Position.-Posicion en la que deseas ubicar la(s) columna(s) en la Base1, si no ingresas ninguna posicion se insertara
#en la ultima posicion.

C.Bind <- function(Base1, Base2, Position = NULL){
  
  if(is.null(Position)){
    
    return(cbind(Base1, Base2))
    
  }else{
    
    n <- dim(as.data.frame(Base1))[2] + dim(as.data.frame(Base2))[2]
    
    if(Position == 1){
      
      return(cbind(Base2, Base1))
      
    }else if(Position > 1 & Position < n){
      
      return(cbind(Base1,Base2)[ , c(1:(Position-1), n, (Position):(n-1))])
      
    }else if(Position == n){
      
      return(cbind(Base1, Base2))
      
    }else{
      
      cat("La posicion ingresada es incorrecta, por favor verificala.")
      
      return(NA)
      
    }
    
  }
  
}
