#Función para exportar objetos data frame en archivos con extensión (.csv) o (.txt).
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 18/07/2017
#Fecha de ultima modificacion: 30/04/2018
#Argumentos:
#La funcion no necesita ningun argumento.

Save.D.F <- function(){
  
  cat("Ingresa el nombre del data frame que deseas guardar:")
  
  Base <- readline()
  
  Base <- trimws(strsplit(Base, split = "\"")[[1]][which(unlist(strsplit(Base, split = "\"")) != character(1))])
  
  if(exists(Base)){
    
    if(is.data.frame(eval(as.symbol(Base)))){
      
      cat("\nIngresa el nombre con el que deseas guardar el data frame y pulsa enter:")
      
      name <- readline()
      
      r <- character(1)
      
      while(r != "No" & r != "NO" & r != "no" & r != "nO" & r != "Si" & r != "SI" & r != "si" & r != "sI"){
        
        cat(paste0("\nEl data frame se guardara en la siguiente direccion:\n\n", getwd(), "\n\n¿Desea utilizar esta direccion?"))
        
        r <- readline()
        
        if(r == "No" | r == "NO" | r == "no" | r == "nO"){
          
          cat("\nIngresa la dirección donde deseas que se guarde el data frame siguiendo el formato que se mostro anteriormente y pulsa enter:")
          
          name_dir <- readline()
          
        }else if(r == "Si" | r == "SI" | r == "si" | r == "sI"){
          
          name_dir <- getwd()
          
        }else{
          
          cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
          
        }
        
      }
      
      r1 <- character(1)
      
      while(r1 != "1" & r1 != "2" & r1 != "3"){
        
        cat("\nIngresa la opción del tipo de extension con el que deseas guardar el data frame y pulsa enter:\n1.-(.csv)\n2.-(.txt)\n3.-Ambos") 
        
        r1 <- readline()
        
        if(r1 == "1"){
          
          write.csv(eval(as.symbol(Base)), paste0(name_dir, "/", name, ".csv"), row.names = FALSE) 
          
          cat("\nEl data frame se guardo satisfactoriamente.")

        }else if(r1 == "2"){
          
          write.table(eval(as.symbol(Base)), paste0(name_dir, "/", name, ".txt"), row.names = FALSE, sep = '\t') 
          
          cat("\nEl data frame se guardo satisfactoriamente.")
          
        }else if(r1 == "3"){
          
          write.csv(eval(as.symbol(Base)), paste0(name_dir, "/", name, ".csv"), row.names = FALSE) 
          
          write.table(eval(as.symbol(Base)), paste0(name_dir, "/", name, ".txt"), row.names = FALSE, sep = '\t') 
          
          cat("\nEl data frame se guardo satisfactoriamente.")
          
        }else{
          
          cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
          
        }
        
      }
      
    }else{
      
      cat("\nEl objeto que ingresaste no es una base de datos por favor confirma la clase del mismo.")
      
    }
    
  }else{
    
    cat("\nEl objeto ingresado no existe en el ambiente de trabajo, por favor verifica su nombre.")
    
  }
  
}

#Ejemplo:
#dat <- data.frame(Nombre = c("Oscar", "Aide", "Monica", "Daniel"), Edad = c(12, 34, 54, 72), Peso = c(13.6, 34.7, 78.9, 39.5)) 
