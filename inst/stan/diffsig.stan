//lamusig
data {
  int<lower=0> N; // number of samples
  int<lower=0> K; // number of signatures
  int<lower=0> M; // number of risk factors
  int<lower=0> L; // number of single-base mutation context
  int<lower=0> Y[L,N]; // observed mutation counts
  real<lower=0> beta_sd; // standard deviation of beta
  matrix<lower=0,upper=1>[L,K] C; // K-signatures from COSMIC mutational signature data
  matrix[M,N] X; // risk factor data (risk factor by samples)
}

parameters {
  real<lower=0> tau; // precision parameter for dirichlet sampling
  simplex[K] phi[N]; // probability of each mutation type for each sample sampled from dirichlet distribution
  matrix[M,K] beta; // latent association of risk factors and signatures
}

transformed parameters {
  matrix[K,N] alpha; // latent association of samples and signatures
  simplex[K] sm_alpha[N]; // softmax-ed alpha
  vector<lower=0>[L] pi[N]; // probability of each mutation type for each sample based on COSMIC

  alpha = beta'*X;

  for (n in 1:N) {
    sm_alpha[n] = softmax(alpha[,n]);
  }

  for (n in 1:N) {
    pi[n] = C * phi[n];
  }
}

model {
  tau ~ lognormal(0,2);

  // give prior to beta
  for (m in 1:M)
    for (k in 1:K)
      beta[m,k] ~ normal(0, beta_sd);

  for (n in 1:N)
    phi[n] ~ dirichlet(sm_alpha[n]*tau);

  for (n in 1:N)
    Y[,n] ~ multinomial(pi[n]);
}
