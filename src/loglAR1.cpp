// loglAR1 function(obs, hat, rho = 0) 
//  calculates likelihood for AR(1) process
//for( int t=0; t< nyears; t++){
//             n <- length(obs)
//             rsdl <- (obs[-1] - rho * obs[-n] - hat[-1] + rho * hat[-n])
//             s2 <- sum(rsdl^2, na.rm = T)
//             s1 <- s2
//             if (!all(is.na(rsdl[1]))) 
//               s1 <- s1 + (1 - rho^2) * (obs[1] - hat[1])^2
//             sigma2 <- sum((obs - hat)^2)
//               n <- length(obs[!is.na(obs)])
//               sigma2.a <- (1 - rho^2) * sigma2
//               res <- (log(1/(2 * pi)) - n * log(sigma2.a) + log(1 - rho^2) - s1/(2 * sigma2.a))/2
//             if (!is.finite(res)) res <- -1e+100
//             return(res)}) # }}}

//export PKG_CPPFLAGS="-I/home/laurie/R/x86_64-pc-linux-gnu-library/3.6/Rcpp/include"
//export CXXFLAGS="-I/home/laurie/R/x86_64-pc-linux-gnu-library/3.6/Rcpp/include"laurie@laurie-XPS-13-9310:~/Desktop/flr$ 
  
#include <Rcpp.h>
using namespace Rcpp;

// Define the loglAR1 function
double loglAR1(NumericVector obs, NumericVector hat, double rho) {
  int n = obs.size();
  NumericVector rsdl(n - 1);
  double s2 = 0.0;
  double s1 = 0.0;
  
  for (int i = 0; i < n - 1; i++) {
    rsdl[i] = obs[i + 1] - rho * obs[i] - hat[i + 1] + rho * hat[i];
    s2 += pow(rsdl[i], 2);
  }
  
  if (!NumericVector::is_na(rsdl[0])) {
    s1 += (1 - pow(rho, 2)) * pow(obs[0] - hat[0], 2);
  }
  
  double sigma2 = 0.0;
  for (int i = 0; i < n; i++) {
    sigma2 += pow(obs[i] - hat[i], 2);
  }
  
  int valid_count = n;
  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(obs[i])) {
      valid_count--;
    }
  }
  
  double sigma2_a = (1 - pow(rho, 2)) * sigma2;
  double res = (log(1 / (2 * M_PI)) - valid_count * log(sigma2_a) + log(1 - pow(rho, 2)) - s1 / (2 * sigma2_a)) / 2;
  
  if (!std::isfinite(res)) {
    res = -1e+100;
  }
  
  return res;
}