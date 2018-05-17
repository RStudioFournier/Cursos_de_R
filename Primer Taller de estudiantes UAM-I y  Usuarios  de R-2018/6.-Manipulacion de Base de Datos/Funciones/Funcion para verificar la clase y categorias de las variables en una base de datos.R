#Función para verificar la clase y categorias de las variables en una base de datos.
#Autor: Arnoldo Daniel Miranda Fournier
#Fecha de creacion: 30/04/2018
#Fecha de ultima modificacion: 30/04/2018
#Argumentos:
#Base.-Base la cual se quiere verificar.

Ver.D.F <- function(Base){
  
  Read.Char <- function(){
    
    #################################
    #Programa auxiliar
    Hom.Name <- function(Nombre){
      
      Nombre <- trimws(Nombre)
      
      Nombre <- casefold(paste0(replace(unlist(strsplit(Nombre, split = "")), which(unlist(strsplit(Nombre, split = "")) == " " | unlist(strsplit(Nombre, split = "")) == "."), "_"), collapse = ""), upper = TRUE)
      
      return(Nombre)
      
    }
    #################################
    
    Entrada <- readline()
    
    Salida <- sapply(trimws(unlist(strsplit(Entrada, split = ","))), Hom.Name, USE.NAMES = FALSE)
    
    return(Salida)
    
  }
   
  Hom.Name <- function(Nombre){
    
    Nombre <- trimws(Nombre)
    
    Nombre <- casefold(paste0(replace(unlist(strsplit(Nombre, split = "")), which(unlist(strsplit(Nombre, split = "")) == " " | unlist(strsplit(Nombre, split = "")) == "."), "_"), collapse = ""), upper = TRUE)
    
    return(Nombre)
    
  }
  
  
  if(is.data.frame(Base)){
    
    cat("La base ingresada cuenta con un total de", dim(Base)[1], "registros(renglones) y", dim(Base)[2], "variables(columnas).\n")
    
    options(warn =-1)
    
    colnames(Base) <- sapply(colnames(Base), Hom.Name)
    
    Base[, which(sapply(Base, is.factor) == TRUE | sapply(Base, is.character) == TRUE)] <- lapply(Base[, which(sapply(Base, is.factor) == TRUE | sapply(Base, is.character) == TRUE)], as.factor)
    
    cat("\nEl siguiente listado corresponde al nombre de cada una de las variables, pulsa enter para visualizarlo:\n")
    
    readline()
    
    cat(colnames(Base), sep = ", ")
    
    r <- character(1)
    
    while(r != "No" & r != "NO" & r != "no" & r != "nO" & r != "Si" & r != "SI" & r != "si" & r != "sI"){
      
      cat("\n¿Desea analizar la clase y categorias de todas las variables?\n")
      
      r <- readline()
      
      if(r == "Si" | r == "SI" | r == "si" | r == "sI"){
      
        Variables_de_Interes <-  colnames(Base)
        
      }else if(r == "No" | r == "NO" | r == "no" | r == "nO"){
        
        cat("\n\nA continuación elige únicamente las variables de interés, evitando poner comillas y separandolas con una coma si son más de dos:")
        
        Variables_de_Interes <-  Read.Char()
        
      }else{
        
        cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
        
      }
      
    }
    
    cat("\nNota: Todos los cambios y modificaciones que se realicen en la base a continuación no afectarán la base originalmente ingresada.\n")
    
    cat("\nPulsa enter para analizar las clases y categorías de cada una de las", length(Variables_de_Interes), "variables de interés:")
    
    readline()  
    
    for(i in 1:length(Variables_de_Interes)){
      
      cat("\nEsta es la clase para la variable", Variables_de_Interes[i], ":\n\n")
      
      print(class(Base[, Variables_de_Interes[i]]))
      
      r1 <- character(1)
      
      while(r1 != "No" & r1 != "NO" & r1 != "no" & r1 != "nO" & r1 != "Si" & r1 != "SI" & r1 != "si" & r1 != "sI"){
        
        cat("¿La clase para la variable", Variables_de_Interes[i],"es correcta?\n")
        
        r1 <- readline()
        
        if(r1 == "Si" | r1 == "SI" | r1 == "si" | r1 == "sI"){
          
          clasificacion <- Hom.Name(class(Base[, Variables_de_Interes[i]]))
          
        }else if(r1 == "No" | r1 == "NO" | r1 == "no" | r1 == "nO"){
          
          clasificacion <- character(1)
          
          while(clasificacion != "FACTOR" & clasificacion != "\"FACTOR\"" & clasificacion != "NUMERIC" & clasificacion != "\"NUMERIC\"" & clasificacion != "INTEGER" & clasificacion != "\"INTEGER\""){
            
            cat("\nPara corregir la clasificación favor utiliza siguiente codificación:\n\"Factor\" si la variable es cualitativa.\n\"Numeric\" o \"Integer\" si la variable cuantitativa\n\nIngresa la correcta clasificación para la variable", Variables_de_Interes[i],":\n")
            
            clasificacion <- Read.Char()
            
            if(clasificacion == "FACTOR" | clasificacion == "\"FACTOR\""){
              
              Base[, Variables_de_Interes[i]] <- as.factor(Base[, Variables_de_Interes[i]])
              
            }else if(clasificacion == "NUMERIC" | clasificacion == "\"NUMERIC\""){
              
              Base[, Variables_de_Interes[i]] <- as.numeric(Base[, Variables_de_Interes[i]])
              
            }else if(clasificacion == "INTEGER" | clasificacion == "\"INTEGER\""){
              
              Base[, Variables_de_Interes[i]] <- as.integer(Base[, Variables_de_Interes[i]])
              
            }else{
              
              cat("\nLa opción ingresada es incorrecta, verifique su respuesta recordando seguir la codificación dada.\n")
              
            }
            
          }
          
        }else{
          
          cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n\n")
          
        }
        
      }
      
      if(clasificacion == "FACTOR" | clasificacion == "\"FACTOR\""){
        
        levels(Base[, Variables_de_Interes[i]]) <- sapply(levels(Base[, "SEXO"]), Hom.Name, USE.NAMES = FALSE)
        
        cat("\nEstas son las categorías para la variable", Variables_de_Interes[i], ":\n\n")
        
        print(levels(Base[, Variables_de_Interes[i]]))
        
        r2 <- character(1)
        
        while(r2 != "No" & r2 != "NO" & r2 != "no" & r2 != "nO" & r2 != "Si" & r2 != "SI" & r2 != "si" & r2 != "sI"){
          
          cat("\n¿Las categorías para la variable", Variables_de_Interes[i],"son correctas?\n")
          
          r2 <- readline()
          
          if(r2 == "Si" | r2 == "SI" | r2 == "si" | r2 == "sI"){
            
            ######
            
          }else if(r2 == "No" | r2 == "NO" | r2 == "no" | r2 == "nO"){
            
            cat("\n")
            
            cat(levels(Base[, Variables_de_Interes[i]]), sep = ", ")
            
            #t <- levels(Base[, Variables_de_Interes[i]]) checar esto
            
            #levels(Base[, Variables_de_Interes[i]]) <- sapply(levels(Base[, Variables_de_Interes[i]]), Cambiar_Nombres, USE.NAMES = FALSE)
            
            cat("\nIngresa las categorías que son incorrectas, evitando poner comillas y separandolas con una coma si son más de dos:\n")
            
            Categorias_no_gratas <- Read.Char()
            
            r3 <- character(1)
            
            while(r3 != "No" & r3 != "NO" & r3 != "no" & r3 != "nO" & r3 != "Si" & r3 != "SI" & r3 != "si" & r3 != "sI"){
              
              cat("\n¿Alguna de estas categorías es corregible, es decir, se puede intercambiar por alguna de las que son correctas?\n")
              
              r3 <- readline()
              
              if(r3 == "No" | r3 == "NO" | r3 == "no" | r3 == "nO"){
                
                Quitar <- integer() ; p <- 0
                
                for(l in 1:length(Categorias_no_gratas)){
                  
                  r4 <- character(1)
                  
                  while(r4 != "No" & r4 != "NO" & r4 != "no" & r4 != "nO" & r4 != "Si" & r4 != "SI" & r4 != "si" & r4 != "sI"){
                    
                    cat("\nHay", length(which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas[l])), "registros que contienen la categoría:\n\n")
                    
                    print(Categorias_no_gratas[l])
                    
                    cat("\n¿Deseas eliminar estos registros de tu base?\n")
                    
                    r4 <- readline()
                    
                    if(r4 == "No" | r4 == "NO" | r4 == "no" | r4 == "nO"){
                      
                      #####
                      
                    }else if(r4 == "Si" | r4 == "SI" | r4 == "si" | r4 == "sI"){
                      
                      Quitar[(1 + p):(length(which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas[l])) + p)] <- which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas[l])
                      
                      p <- p + length(which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas[l]))
                      
                      Base <- Base[-unique(Quitar), ]
                      
                      #Base[, which(sapply(Base, is.factor) == TRUE)] <- lapply(Base[, which(sapply(Base, is.factor) == TRUE)], factor)
                      
                      Base[, Variables_de_Interes[i]] <- factor(Base[, Variables_de_Interes[i]])
                      
                    }else{
                      
                      cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
                      
                    }
                    
                  }
                  
                }
                
                
              }else if(r3 == "Si" | r3 == "SI" | r3 == "si" | r3 == "sI"){
                
                cat("\nIngresa aquellas que sean corregibles, separandolas con una coma si son más de dos:\n")
                
                Categorias_Corregibles <- Read.Char()
                
                if(length(which(Categorias_no_gratas != Categorias_Corregibles)) != 0){
                  
                  for(j in 1:length(Categorias_Corregibles)){
                    
                    cat("\nPara la categoría:\n\n")
                    
                    print(Categorias_Corregibles[j])
                    
                    cat("\nIngresa la corrección correspondiente:\n")
                    
                    Categoria_de_cambio <- Read.Char()
                    
                    levels(Base[, Variables_de_Interes[i]])[which(levels(Base[, Variables_de_Interes[i]]) == Categorias_Corregibles[j])] <- Categoria_de_cambio
                    
                  }
                  
                  Categorias_no_gratas_2 <- Categorias_no_gratas[which(Categorias_no_gratas != Categorias_Corregibles)]
                  
                  Quitar <- integer() ; p <- 0
                  
                  for(l in 1:length(Categorias_no_gratas_2)){
                    
                    r <- character(1)
                    
                    while(r != "No" & r != "NO" & r != "no" & r != "nO" & r != "Si" & r != "SI" & r != "si" & r != "sI"){
                      
                      cat("\nHay", length(which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas_2[l])), "registros que contienen la categoría:\n\n")
                      
                      print(Categorias_no_gratas_2[l])
                      
                      cat("\n¿Deseas eliminar estos registros de tu base?\n")
                      
                      r <- readline()
                      
                      if(r == "No" | r == "NO" | r == "no" | r == "nO"){
                        
                        #####
                        
                      }else if(r == "Si" | r == "SI" | r == "si" | r == "sI"){
                        
                        Quitar[(1 + p):(length(which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas_2[l])) + p)] <- which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas_2[l])
                        
                        p <- p + length(which(Base[, Variables_de_Interes[i]] == Categorias_no_gratas_2[l]))
                        
                        Base <- Base[-unique(Quitar), ]
                        
                        #Base[, which(sapply(Base, is.factor) == TRUE)] <- lapply(Base[, which(sapply(Base, is.factor) == TRUE)], factor)
                        
                        Base[, Variables_de_Interes[i]] <- factor(Base[, Variables_de_Interes[i]])
                        
                      }else{
                        
                        cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
                        
                      }
                      
                    }
                    
                  }
                  
                  
                }else{
                  
                  for(j in 1:length(Categorias_Corregibles)){
                    
                    cat("\nPara la categoría:\n\n")
                    
                    print(Categorias_Corregibles[j])
                    
                    cat("\nIngresa la corrección correspondiente:\n")
                    
                    Categoria_de_cambio <- Read.Char()
                    
                    levels(Base[, Variables_de_Interes[i]])[which(levels(Base[, Variables_de_Interes[i]]) == Categorias_Corregibles[j])] <- Categoria_de_cambio
                    
                  }
                  
                  Base[, Variables_de_Interes[i]] <- factor(Base[, Variables_de_Interes[i]])
                  
                }
                
              }else{
                
                cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
                
              }
              
            }
            
          }else{
            
            cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
            
          }
          
        }
        
      }
      
      if(length(Variables_de_Interes) == 1){
        
        cat("\nLa clase y categorías para la variable", Variables_de_Interes[i]," ya son correctas, pulsa enter para pasar a lo siguiente.\n")
        
        readline()
        
      }else{
        
        if(i != length(Variables_de_Interes)){
          
          cat("\nLa clase y categorías para la variable", Variables_de_Interes[i],"ya son correctas, pulsa enter para pasar a la siguiente variable.\n")
          
          readline()
          
        }else{
          
          cat("\nLa clase y categorías para la variable", Variables_de_Interes[i]," ya son correctas, pulsa enter para pasar a lo siguiente.\n")
          
          readline()
          
        }
        
      }
      
    }
    
    s <- character(1)
    
    while(s != "No" & s != "NO" & s != "no" & s != "nO" & s != "Si" & s != "SI" & s != "si" & s != "sI"){
      
      cat("\n¿Deseas crear una base unicamente con las", length(Variables_de_Interes), "variables de interes introducidas previamente?")
      
      s <- readline()
      
      if(s == "No" | s == "NO" | s == "no" | s == "nO"){
        
        return(Base)
        
      }else if(s == "Si" | s == "SI" | s == "si" | s == "sI"){
        
        return(Base[ , Variables_de_Interes])
        
      }else{
        
        cat("\nLa opción ingresada es incorrecta, verifique su respuesta.\n")
        
      }
      
    }
    
  }else{
    
    cat("El objeto que ingresaste no es una base de datos, por favor confirma el objeto o la clase del mismo.")
    
  }
  
}
