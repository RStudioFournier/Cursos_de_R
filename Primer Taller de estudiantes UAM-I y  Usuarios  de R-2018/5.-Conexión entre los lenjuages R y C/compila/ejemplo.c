#include "headers.h"
void loop_pred_bootstrap(int *m, int *n, double *x, double *y, double *e_hat,
                         int *n_star, double *x_star, double *y_star_hat,
                         double *y_star_boot_hat, double *sxx, double *x_bar,
                         double *b0_hat, double *b1_hat, double *h){
  int t, i, z, j=0;
	double y_boot[*n], y_boot_bar, b0_boot_hat, b1_boot_hat, y_boot_hat[*n],
         e_boot_hat[*n], s_boot_hat[*n], s_boot_hat_bar, s_boot_hat_adj[*n],
         e_star_boot_hat[*n_star], tmp;
  GetRNGstate(); /* Inicializando la semilla para generar v.a. */
  for(t = 0; t < *m; t++){
    y_boot_bar = 0.0;
    b1_boot_hat = 0.0;
    s_boot_hat_bar = 0.0;
    for(i = 0; i < *n; i++){
      z = floor(*n*unif_rand());
      y_boot[i] = y[i] + e_hat[z];
      y_boot_bar += y_boot[i];
    }
    y_boot_bar /= *n;
    for(i = 0; i < *n; i++){
      b1_boot_hat += (x[i] - *x_bar)*(y_boot[i] - y_boot_bar);
    }
    b1_boot_hat /= (*sxx);
    b0_boot_hat = y_boot_bar - b1_boot_hat*(*x_bar);
    for(i = 0; i < *n; i++){
      y_boot_hat[i] = b0_boot_hat + b1_boot_hat*x[i];
      e_boot_hat[i] = y_boot[i] - y_boot_hat[i];
      s_boot_hat[i] = e_boot_hat[i]/sqrt(1.0 - h[i]);
      s_boot_hat_bar += s_boot_hat[i];
    }
    s_boot_hat_bar /= *n;
    for(i = 0; i < *n; i++){
      s_boot_hat_adj[i] = s_boot_hat[i] - s_boot_hat_bar;
    }
    for(i = 0; i < *n_star; i++){
      tmp = (*b0_hat - b0_boot_hat) + (*b1_hat - b1_boot_hat)*x_star[i];
      z = floor((*n)*unif_rand());
      e_star_boot_hat[i] = tmp + s_boot_hat_adj[z];
      y_star_boot_hat[j] = y_star_hat[i] + e_star_boot_hat[i];
      j++;
    }
  }
	PutRNGstate(); /* Deteniendo el seguimiento de la semilla */
}
