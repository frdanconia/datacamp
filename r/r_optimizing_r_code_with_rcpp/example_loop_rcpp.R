library(Rcpp)

#example loop
Rcpp::cppFunction("
NumericVector vec(int n) {
  NumericVector vector(n);
  int i;
  for (i = 0; i < n; i++) {
    vector[i] = R::rnorm(1.0,0.5);
  }
  return vector;
}")

vec(100)


