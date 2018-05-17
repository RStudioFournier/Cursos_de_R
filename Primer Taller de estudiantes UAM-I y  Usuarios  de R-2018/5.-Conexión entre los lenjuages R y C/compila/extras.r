#                                      #
#       Modelo de RLS                  #
# Predicción vía re-muestreo Bootstrap #
# Función programada directamente en R #
#                                      #
inferencia_boot_pred_R <- function(m, x, y, x_star){
  n <- length(x)
  x_bar <- mean(x)
  y_bar <- mean(y)
  #### RLS usual ####
  sxx <- sum((x-x_bar)^2)
  b1_hat <- sum((x-x_bar)*(y-y_bar))/sxx
  b0_hat <- y_bar - b1_hat*x_bar
  y_hat <- b0_hat + b1_hat*x
  e_hat <- y - y_hat
  h <- 1/n + ((x - x_bar)^2)/sxx
  ## Estimación puntual para y_star ###
  n_star <- length(x_star) 
  y_star_hat <- b0_hat + b1_hat*x_star
  #################################################
  y_star_hat_boot <- array(NA, c(m, n_star))
  for(t in 1:m){
    y_boot <- y + sample(e_hat, n, replace = TRUE)
    y_bar_boot <- mean(y_boot)
    b1_hat_boot <- sum((x-x_bar)*(y_boot-y_bar_boot))/sum((x-x_bar)^2)
    b0_hat_boot <- y_bar_boot - b1_hat_boot*x_bar
    y_hat_boot <- b0_hat_boot + b1_hat_boot*x
    e_hat_boot <- y_boot - y_hat_boot
    ##################################################
    s_hat_bot <- e_hat_boot/sqrt(1 - h)
    s_hat_bot_adj <- s_hat_bot - mean(s_hat_bot)
    ##################################################
    tmp1 <- (b0_hat - b0_hat_boot) + (b1_hat - b1_hat_boot)*x_star
    e_star_hat_boot <- tmp1 + sample(s_hat_bot_adj, n_star, replace = TRUE)
    y_star_hat_boot[t, ]<- y_star_hat + e_star_hat_boot
  }
  res <- array(NA, c(n_star, 3))
  for(j in 1:n_star){
    res[j, ] <- quantile(y_star_hat_boot[,j], probs = c(0.5, 0.025 ,0.975), type = 8)
  }
  colnames(res) <- c("fit", "lwr", "upr")
  res
}
#                                      #
#       Modelo de RLS                  #
# Predicción vía re-muestreo Bootstrap #
# Función programada en R con ciclo en # 
# paralelo                             #
#                                      #
inferencia_boot_pred_paralelo <- function(m, x, y, x_star){
  n <- length(x)
  x_bar <- mean(x)
  y_bar <- mean(y)
  #### RLS usual ####
  sxx <- sum((x-x_bar)^2)
  b1_hat <- sum((x-x_bar)*(y-y_bar))/sxx
  b0_hat <- y_bar - b1_hat*x_bar
  y_hat <- b0_hat + b1_hat*x
  e_hat <- y - y_hat
  h <- 1/n + ((x - x_bar)^2)/sxx
  ## Estimación puntual para y_star ###
  n_star <- length(x_star) 
  y_star_hat <- b0_hat + b1_hat*x_star
  #################################################
  y_star_hat_boot <- foreach(i = 1:m, .combine = rbind)%dorng%{
    y_boot <- y + sample(e_hat, n, replace = TRUE)
    y_bar_boot <- mean(y_boot)
    b1_hat_boot <- sum((x-x_bar)*(y_boot-y_bar_boot))/sum((x-x_bar)^2)
    b0_hat_boot <- y_bar_boot - b1_hat_boot*x_bar
    y_hat_boot <- b0_hat_boot + b1_hat_boot*x
    e_hat_boot <- y_boot - y_hat_boot
    ##################################################
    s_hat_bot <- e_hat_boot/sqrt(1 - h)
    s_hat_bot_adj <- s_hat_bot - mean(s_hat_bot)
    ##################################################
    tmp1 <- (b0_hat - b0_hat_boot) + (b1_hat - b1_hat_boot)*x_star
    e_star_hat_boot <- tmp1 + sample(s_hat_bot_adj, n_star, replace = TRUE)
    y_star_hat + e_star_hat_boot
  }
  res <- array(NA, c(n_star, 3))
  for(j in 1:n_star){
    res[j, ] <- quantile(y_star_hat_boot[,j], probs = c(0.5, 0.025 ,0.975), type = 8)
  }
  colnames(res) <- c("fit", "lwr", "upr")
  res
}
#                                        #
#       Modelo de RLS                    #
# Predicción vía re-muestreo Bootstrap   #
# Función programada en R con ciclo en C # 
#                                        #
inferencia_boot_pred_C <- function(m, x, y, x_star){
  n <- length(x)
  x_bar <- mean(x)
  y_bar <- mean(y)
  #### RLS usual ####
  sxx <- sum((x-x_bar)^2)
  b1_hat <- sum((x-x_bar)*(y-y_bar))/sxx
  b0_hat <- y_bar - b1_hat*x_bar
  y_hat <- b0_hat + b1_hat*x
  e_hat <- y - y_hat
  h <- 1/n + ((x - x_bar)^2)/sxx
  ## Estimación puntual para y_star ###
  n_star <- length(x_star) 
  y_star_hat <- b0_hat + b1_hat*x_star
  #################################################
  y_star_hat_boot <- loop_C(m, n, x, y, e_hat, n_star, x_star, y_star_hat, 
                            sxx, x_bar, b0_hat, b1_hat, h)
  #################################################
  res <- array(NA, c(n_star, 3))
  for(j in 1:n_star){
    res[j, ] <- quantile(y_star_hat_boot[,j], probs = c(0.5, 0.025 ,0.975), type = 8)
  }
  colnames(res) <- c("fit", "lwr", "upr")
  res
}
loop_C <- function(m, n, x, y, e_hat, n_star, x_star, y_star_hat, 
                   sxx, x_bar, b0_hat, b1_hat, h){
  y_star_boot_hat <- rep(0, m*n_star)
  res <- .C("loop_pred_bootstrap", as.integer(m), as.integer(n), as.double(x), 
            as.double(y), as.double(e_hat), as.integer(n_star), as.double(x_star), 
            as.double(y_star_hat), as.double(y_star_boot_hat), as.double(sxx), 
            as.double(x_bar), as.double(b0_hat), as.double(b1_hat), as.double(h))
  matrix(res[[9]], nrow = m, ncol = n_star, byrow = TRUE)
}