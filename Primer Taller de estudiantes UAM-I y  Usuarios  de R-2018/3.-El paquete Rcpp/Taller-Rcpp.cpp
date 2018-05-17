#include <Rcpp.h>
#include <numeric>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]    
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

/*** R
library(microbenchmark)
  x <- runif(1e5)
  microbenchmark(
    mean(x),
    meanC(x)
  )
  */

// [[Rcpp::export]]
double mpe(List mod) {
  if (!mod.inherits("lm")) stop("Entrada tiene que ser un modelo lineal");
  
  NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  
  int n = resid.size();
  double err = 0;
  for(int i = 0; i < n; ++i) {
    err += resid[i] / (fitted[i] + resid[i]);
  }
  return err / n;
}

// [[Rcpp::export]]
RObject callWithOne(Function f) {
  return f(1);
}

// [[Rcpp::export]]
double sum3(NumericVector x) {
  double total = 0;
  
  NumericVector::iterator it;
  for(it = x.begin(); it != x.end(); ++it) {
    total += *it;
  }
  return total;
}

// [[Rcpp::export]]
double sum4(NumericVector x) {
  return std::accumulate(x.begin(), x.end(), 0.0);
}

// [[Rcpp::export]]
IntegerVector findInterval2(NumericVector x, NumericVector breaks) {
  IntegerVector out(x.size());
  
  NumericVector::iterator it, pos;
  IntegerVector::iterator out_it;
  
  for(it = x.begin(), out_it = out.begin(); it != x.end(); 
      ++it, ++out_it) {
    pos = std::upper_bound(breaks.begin(), breaks.end(), *it);
    *out_it = std::distance(breaks.begin(), pos);
  }
  
  return out;
}

// [[Rcpp::export]]
List rleC(NumericVector x) {
  std::vector<int> lengths;
  std::vector<double> values;
  
  // Inicializar primer valor
  int i = 0;
  double prev = x[0];
  values.push_back(prev);
  lengths.push_back(1);
  
  NumericVector::iterator it;
  for(it = x.begin() + 1; it != x.end(); ++it) {
    if (prev == *it) {
      lengths[i]++;
    } else {
      values.push_back(*it);
      lengths.push_back(1);
      
      i++;
      prev = *it;
    }
  }
  
  return List::create(
    _["lengths"] = lengths, 
    _["values"] = values
  );
}

// [[Rcpp::export]]
std::map<double, int> tableC(NumericVector x) {
  std::map<double, int> counts;
  
  int n = x.size();
  for (int i = 0; i < n; i++) {
    counts[x[i]]++;
  }
  
  return counts;
}

// [[Rcpp::export]]
NumericMatrix gibbs_cpp(int N, int thin) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0;
  
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rgamma(1, 3, 1 / (y * y + 4))[0];
      y = rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  
  return(mat);
}
