data {
 int<lower=1> J; // Legislators
 int<lower=1> M; // Roll calls
 int<lower=1> N; // Num of observations (< M*J)
 int<lower=1> N_obs; // Num of observed
 int<lower=1> N_mis; // Num of missing
 int<lower=1> K; // Number of unique issues codes
 int<lower=1, upper=J> j[N]; // Legislator for observation n
 int<lower=1, upper=M> m[N]; // Roll call for observation n
 int<lower=0, upper=1> y_obs[N_obs]; // Vote of observation n
 int<lower=1> idx_obs[N_obs]; // Index of observed
 int<lower=1> idx_mis[N_mis]; // Index of missing
 int<lower=1, upper=K> z[M]; // Issue codes for roll call m
 real<lower=0> a; // Hyperparameter for rho
 real<lower=0> b; // Hyperparameter for theta
}
parameters {
  real alpha[M]; // Difficulty of roll call m i.e. alpha_j = (O_yj'O_yj - O_nj'O_nj)/sigma_j
  real<lower=0> w[M]; // Scale for discrimination parameter
  real x_coord_theta[K]; // Issue vector in x,y-coordinates
  real y_coord_theta[K];
  real x_coord_u[M];  // Roll call vector in x,y-coordinates
  real y_coord_u[M];
  row_vector[2] x[J]; // Ideal point of legislator j
  real<lower=0> rho; // Concentration parameter
}
transformed parameters {
  real<lower=0> r_theta[K]; // Auxiliary variable for theta
  real theta[K]; // Issue vector in angle
  real<lower=0> r_u[M]; // Auxiliary variable for u
  real u[M]; // Roll call vector in angle
  for (k in 1:K) { // change from x,y-coordinates to polar coordinates
    theta[k] = atan2(y_coord_theta[k], x_coord_theta[k]);
    r_theta[k] = hypot(x_coord_theta[k], y_coord_theta[k]);
  }
  for (i in 1:M) { // change from x,y-coordinates to polar coordinates
    u[i] = atan2(y_coord_u[i], x_coord_u[i]);
    r_u[i] = hypot(x_coord_u[i], y_coord_u[i]);
  }
}
model {
  for (k in 1:J) { // Ideal point
    x[k][1] ~ normal(0,1);
    x[k][2] ~ normal(0,1);
  }
  target += -a*log(modified_bessel_first_kind(0, rho)); // Prior for rho
  for (k in 1:K) {
    r_theta[k] ~ normal(1.0, 0.1); // Sample auxiliary variable
    target += -log(r_theta[k]); // Log of determinant of Jacobian of Cartesian (x,y) -> Polar (theta, r)
    // (i.e. determinanot of matrix of dr/dx, dr/dy, dtheta/dx, dtheta/dy)
    // target += b*rho*cos(theta[k]); // Alternative to von_mises()
    // target += -log(modified_bessel_first_kind(0, b*rho));
    theta[k] ~ von_mises(0, b*rho); // Sample issue vector
  }
  for (i in 1:M) {
    r_u[i] ~ normal(1.0, 0.1); // Sample auxiliary variable
    target += -log(r_u[i]);
    // target += rho*cos(u[i] - theta[z[i]]);
    // target += -log(modified_bessel_first_kind(0, rho));
    u[i] ~ von_mises(theta[z[i]], rho); // Sample roll call vector
    w[i] ~ normal(0,5)T[0,]; // Scale parameter
    alpha[i] ~ normal(0,5); // Difficulty parameter
  }
  for (n in 1:N_obs) { // Sample observed votes
    y_obs[n] ~ bernoulli(Phi(dot_product(x[j[idx_obs[n]]], [cos(u[m[idx_obs[n]]]), sin(u[m[idx_obs[n]]])]) * w[m[idx_obs[n]]] - alpha[m[idx_obs[n]]]));
  }
}
generated quantities {
  int<lower=0, upper=1> y_mis[N_mis];
  for (n in 1:N_mis) { // Impute missing votes
    y_mis[n] = bernoulli_rng(Phi(dot_product(x[j[idx_mis[n]]], [cos(u[m[idx_mis[n]]]), sin(u[m[idx_mis[n]]])]) * w[m[idx_mis[n]]] - alpha[m[idx_mis[n]]]));
  }
}
