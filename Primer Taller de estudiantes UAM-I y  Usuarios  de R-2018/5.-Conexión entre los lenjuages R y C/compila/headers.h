#include <R.h>
#include <Rmath.h>
void loop_pred_bootstrap(int *m, int *n, double *x, double *y, double *e_hat,
                         int *n_star, double *x_star, double *y_star_hat, double *y_star_boot_hat,
                         double *sxx, double *x_bar, double *b0_hat, double *b1_hat,
                         double *h);
