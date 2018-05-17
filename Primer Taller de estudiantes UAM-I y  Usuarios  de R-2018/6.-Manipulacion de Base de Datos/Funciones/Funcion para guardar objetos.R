#Funcion para guardar objetos que se encuentren en el ambiente de trabajo con extension (.Rdata).
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 30/04/2018
#Fecha de ultima modificacion: 30/04/2018
#Argumentos:
#La funcion no necesita ningun argumento.
#Nota:
#Para el funcionamiento de la misma se necesita tener instalado y activado el siguiente paquete:
library("magrittr") 

Save.O <- function(){
  
  r <- character(1)
  
  while(r != "1" & r != "2" & r != "3"){
    
    cat("Elige cuantos objetos deseas guardar y pulsa enter:\n1.-Solo uno.\n2.-Mas de uno.\n3.-Todos.")
    
    r <- readline()
    
    if(r == "1"){
      
      cat("\nIngresa el nombre del objeto que deseas guardar:")
      
      Objeto <- readline()
      
      Objeto <- trimws(strsplit(Objeto, split = "\"")[[1]][which(unlist(strsplit(Objeto, split = "\"")) != character(1))])
      
      if(exists(Objeto)){
        
        cat("\nIngresa el nombre con el que deseas guardar el objeto y pulsa enter:")
        
        name <- readline()
        
        r1 <- character(1)
        
        while(r1 != "No" & r1 != "NO" & r1 != "no" & r1 != "nO" & r1 != "Si" & r1 != "SI" & r1 != "si" & r1 != "sI"){
          
          cat(paste0("\nEl objeto se guardara en la siguiente direccion:\n\n", getwd(), "\n\n¿Desea utilizar esta direccion?"))
          
          r1 <- readline()
          
          if(r1 == "No" | r1 == "NO" | r1 == "no" | r1 == "nO"){
            
            cat("\nIngresa la direccion donde deseas que se guarde el objeto siguiendo el formato que se mostro anteriormente y pulsa enter:")
            
            name_dir <- readline()
            
          }else if(r1 == "Si" | r1 == "SI" | r1 == "si" | r1 == "sI"){
            
            name_dir <- getwd()
            
          }else{
            
            cat("\nLa opcion ingresada es incorrecta, verifique su respuesta.\n")
            
          }
          
        }
        
        Objeto %>% save(., list = Objeto, file = paste0(name_dir, "/", name, ".Rdata")) 
        
        cat("\nEl objeto se guardo satisfactoriamente.")
        
      }else{
        
        cat("\nEl objeto ingresado no existe en el ambiente de trabajo, por favor verifica su nombre.")
        
      }
      
    }else if(r == "2"){
      
      cat("\nIngresa los nombres de los objetos que deseas guardar:")
      
      Objetos <- readline()
      
      Objetos <- unlist(strsplit(trimws(unlist(strsplit(strsplit(Objetos, split = "\"")[[1]][which(unlist(strsplit(Objetos, split = "\"")) != character(1))], split = ","))), split = '\\s+'))
      
      if(length(unlist(lapply(Objetos, exists))) == sum(unlist(lapply(Objetos, exists)))){
        
        cat("\nIngresa el nombre con el que deseas guardar los objetos y pulsa enter:")
        
        name <- readline()
        
        r1 <- character(1)
        
        while(r1 != "No" & r1 != "NO" & r1 != "no" & r1 != "nO" & r1 != "Si" & r1 != "SI" & r1 != "si" & r1 != "sI"){
          
          cat(paste0("\nLos objetos se guardaran en la siguiente direccion:\n\n", getwd(), "\n\n¿Desea utilizar esta direccion?"))
          
          r1 <- readline()
          
          if(r1 == "No" | r1 == "NO" | r1 == "no" | r1 == "nO"){
            
            cat("\nIngresa la direccion donde deseas que se guarden los objetos siguiendo el formato que se mostro anteriormente y pulsa enter:")
            
            name_dir <- readline()
            
          }else if(r1 == "Si" | r1 == "SI" | r1 == "si" | r1 == "sI"){
            
            name_dir <- getwd()
            
          }else{
            
            cat("\nLa opcion ingresada es incorrecta, verifique su respuesta.\n")
            
          }
          
        }
        
        Objetos %>% save(., list = Objetos, file = paste0(name_dir, "/", name, ".Rdata"))
        
        cat("\nLos objetos se guardaron satisfactoriamente.")
        
      }else{
        
        cat("\nAlguno de los objetos ingresados no existen en el ambiente de trabajo, por favor verifica sus nombres.")
        
      }
      
    }else if(r == "3"){
      
      cat("\nIngresa el nombre con el que deseas guardar los objetos y pulsa enter:")
      
      name <- readline()
      
      r1 <- character(1)
      
      while(r1 != "No" & r1 != "NO" & r1 != "no" & r1 != "nO" & r1 != "Si" & r1 != "SI" & r1 != "si" & r1 != "sI"){
        
        cat(paste0("\nLos objetos se guardaran en la siguiente direccion:\n\n", getwd(), "\n\n¿Desea utilizar esta direccion?"))
        
        r1 <- readline()
        
        if(r1 == "No" | r1 == "NO" | r1 == "no" | r1 == "nO"){
          
          cat("\nIngresa la direccion donde deseas que se guarden los objetos siguiendo el formato que se mostro anteriormente y pulsa enter:")
          
          name_dir <- readline()
          
        }else if(r1 == "Si" | r1 == "SI" | r1 == "si" | r1 == "sI"){
          
          name_dir <- getwd()
          
        }else{
          
          cat("\nLa opcion ingresada es incorrecta, verifique su respuesta.\n")
          
        }
        
      }
      
      save.image(file = paste0(name_dir, "/", name, ".Rdata"))
      
      cat("\nLos objetos se guardaron satisfactoriamente.")
      
    }else{
      
      cat("\nLa opcion ingresada es incorrecta, verifique su respuesta.\n\n")
      
    }
    
  }
  
}
