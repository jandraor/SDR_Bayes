functions {
  vector SEIR(real time, vector y, real[] params) {
    vector[5] dydt;
    real DR;
    real RR;
    real RC;
    real lambda;
    real IR;
    DR = y[2]*0.5;
    RR = y[3]*0.5;
    RC = params[2]*DR;
    lambda = y[3]*params[1]/5234;
    IR = lambda*y[1];
    dydt[1] = -IR;
    dydt[2] = IR-DR;
    dydt[3] = DR-RR;
    dydt[4] = RR;
    dydt[5] = RC;
    return dydt;
  }
}
data {
  int<lower = 1> n_obs;
  int<lower = 1> n_params;
  int<lower = 1> n_difeq;
  int y[n_obs];
  real t0;
  real ts[n_obs];
}
parameters {
  real<lower = 0> beta;
  real<lower = 0, upper = 1> rho;
  real<lower = 0> I0;
  real<lower = 0> R0;
}
transformed parameters{
  vector[n_difeq] o[n_obs]; // Output from the ODE solver
  real x[n_obs];
  vector[n_difeq] x0;
  real params[n_params];
  x0[1] = 5234 - I0 - R0;
  x0[2] = 0;
  x0[3] = I0;
  x0[4] = R0;
  x0[5] = I0;
  params[1] = beta;
  params[2] = rho;
  o = ode_rk45(SEIR, x0, t0, ts, params);
  x[1] =  o[1, 5]  - x0[5];
  for (i in 1:n_obs-1) {
    x[i + 1] = o[i + 1, 5] - o[i, 5] + 1e-4;
  }
}
model {
  beta  ~ lognormal(0, 1);
  rho   ~ beta(2, 2);
  I0    ~ lognormal(0, 1);
  R0    ~ normal(0, 1e4);
  y     ~ poisson(x);
}
