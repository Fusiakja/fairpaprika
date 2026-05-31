data {
  int<lower=1> K;
  array[K] int<lower=1> L;
  int<lower=1> N;
  array[N] int<lower=1,upper=3> y;
  array[N, K] int<lower=1> idxA;
  array[N, K] int<lower=1> idxB;
  int<lower=0> K_inter;
  array[K_inter, 2] int<lower=1> inter_pairs;
  int<lower=0> D_inter;
  array[D_inter] int<lower=1> inter_i;   // pair index (groups shrinkage)
  array[D_inter] int<lower=1> inter_li;  // level index for criterion i
  array[D_inter] int<lower=1> inter_lj;  // level index for criterion j
  real<lower=0> slab_scale;
  real<lower=0> slab_df;
}
transformed data {
  array[K] int offsets;
  offsets[1] = 1;
  for (k in 2:K) offsets[k] = offsets[k - 1] + (L[k - 1] - 1);
  int D = offsets[K] + L[K] - 2;
  int Lsum = 0;
  for (k in 1:K) Lsum += L[k];
  array[K + 1] int cumL;
  cumL[1] = 0;
  for (k in 1:K) cumL[k + 1] = cumL[k] + L[k];
}
parameters {
  vector<lower=0>[D] delta;
  real<lower=0> beta;
  vector[D_inter] u_inter_raw;
  vector<lower=0>[K_inter] tau_inter;
  real<lower=0> lambda_global;
}
transformed parameters {
  vector[Lsum] util;
  {
    for (k in 1:K) {
      util[cumL[k] + 1] = 0;
      for (l in 2:L[k]) {
        util[cumL[k] + l] = util[cumL[k] + l - 1] + delta[offsets[k] + l - 2];
      }
    }
  }
}
model {
  delta ~ normal(0, 1);
  beta ~ gamma(2, 1);
  lambda_global ~ normal(0, 1);
  tau_inter ~ normal(0, 1);
  {
    vector[D_inter] lambda = lambda_global * tau_inter[inter_i];
    vector[D_inter] lambda_tilde;
    for (d in 1:D_inter) {
      lambda_tilde[d] = sqrt( square(lambda[d]) * slab_scale^2 / (slab_scale^2 + square(lambda[d])) );
    }
    u_inter_raw ~ normal(0, 1);
    target += normal_lpdf(u_inter_raw | 0, lambda_tilde);
  }
  for (n in 1:N) {
    real uA = 0;
    real uB = 0;
    for (k in 1:K) {
      uA += util[cumL[k] + idxA[n, k]];
      uB += util[cumL[k] + idxB[n, k]];
    }
    for (d in 1:D_inter) {
      int ci = inter_pairs[inter_i[d], 1];
      int cj = inter_pairs[inter_i[d], 2];
      uA += u_inter_raw[d] * ((idxA[n, ci] == inter_li[d]) && (idxA[n, cj] == inter_lj[d]));
      uB += u_inter_raw[d] * ((idxB[n, ci] == inter_li[d]) && (idxB[n, cj] == inter_lj[d]));
    }
    vector[3] eta;
    eta[1] = beta * (uA - uB);
    eta[2] = beta * (uB - uA);
    eta[3] = 0;
    y[n] ~ categorical_logit(eta);
  }
}
generated quantities {
  vector[Lsum + D_inter] w;
  for (k in 1:Lsum) w[k] = util[k];
  for (d in 1:D_inter) w[Lsum + d] = u_inter_raw[d];
}
