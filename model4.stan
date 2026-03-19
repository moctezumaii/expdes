data {

  int nsites3;

  array[nsites3] int y;

  vector[nsites3] selev3;
}
parameters {
  real alpha;
  real beta;
}
model {

  vector[nsites3] psi;

  // Priors
  alpha ~ uniform(-10, 10);
  beta ~ normal(0, 100);

  // Likelihood

  for (k in 1:nsites3){
    psi[k] = inv_cloglog(alpha + beta * selev3[k]);
    y[k] ~ bernoulli(psi[k]);
  }
}

generated quantities {
  real mean_lam = exp(alpha);
}
