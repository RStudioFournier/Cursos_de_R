#Funcion para insertar una(s) fila(s) en la posicion deseada.
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 06/05/2018
#Fecha de ultima modificacion: 06/05/2018
#Argumentos:
#Base1.-Base a la que le quieres insertar una(s) fila(s).
#Base2.-Base que contiene la(s) fila(s) a insertar.
#Position.-Posicion en la que deseas ubicar la(s) fila(s) en la Base1, si no ingresas ninguna posicion se insertara
#en la ultima posicion.

R.Bind <- function(Base1, Base2, Position = NULL){
  
  if(is.null(Position)){
    
    return(rbind(Base1, Base2))
    
  }else{
    
    n <- dim(as.data.frame(Base1))[1] + dim(as.data.frame(Base2))[1]
    
    if(Position == 1){
      
      return(data.frame(rbind(Base2, Base1), row.names = NULL))
      
    }else if(Position > 1 & Position < n){
      
      return(data.frame(rbind(Base1,Base2)[c(1:(Position-1), n, (Position):(n-1)), ], row.names = NULL))
      
    }else if(Position == n){
      
      return(rbind(Base1, Base2))
      
    }else{
      
      cat("La posicion ingresada es incorrecta, por favor verificala.")
      
      return(NA)
      
    }
    
  }
  
}
