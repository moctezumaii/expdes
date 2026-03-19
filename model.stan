data { // This is the first line of Stan code
  int <lower = 0> n; // Define the format of all data
  vector[n] y; //. . . including the dimension of vectors
  vector[n] x; //
  array[n] int<lower=0, upper=1> x2;
}
parameters { // Define format for all parameters
  real alpha;
  real beta;
  real beta2;
  real beta3;
  real <lower = 0> sigma; // sigma (sd) cannot be negative
}
transformed parameters {
  vector[n] mu;
  for (i in 1:n){
    mu[i] = alpha + beta * x[i] + beta2 * x2[i] + beta3 * x[i] * x2[i] ; // Calculate linear predictor
  }
}
model {
  // Priors
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  beta2 ~ normal(0, 100);
  beta3 ~ normal(0, 100);
  sigma ~ cauchy(0, 10);
  // Likelihood (could be vectorized to increase speed)
  for (i in 1:n){
    y[i] ~ normal(mu[i], sigma);
  }
}

