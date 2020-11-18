generate_stan_files <- function(filepath, mdl) {
  
  dists            <- c("nbinom", "norm", "pois")
  filepaths        <- map_chr(dists, build_files)
  names(filepaths) <- dists
  filepaths
}
 
build_files <- function(dstr) {
  consts       <- sd_constants(mdl)
  ODE_fn       <- "SEIR"
  fit_consts   <- consts$name[c(2, 1)]
  stan_fun     <- stan_ode_function(filepath, ODE_fn, pars = fit_consts)
  fun_exe_line <- str_glue("  o = ode_rk45({ODE_fn}, y0, t0, ts, params);") 
  
  if(dstr == "norm") {
    stan_data <- stan_data("y", "real", inits = FALSE)
  }
  
  if(dstr != "norm") {
    stan_data <- stan_data("y", "int", inits = FALSE)
  }
  
  stan_params <- paste(
    "parameters {",
    "  real<lower = 0>            beta;",
    "  real<lower = 0, upper = 1> rho;",
    "  real<lower = 0> I0;",sep = "\n")
  
  if(dstr == "nbinom") {
    stan_params <- paste(stan_params, 
                         "  real<lower = 0> reciprocal_phi;", sep = "\n")
  }
  
  if(dstr == "norm") {
    stan_params <- paste(stan_params, 
                         "  real<lower = 0> s;", sep = "\n")
  }
  
  stan_params <- paste(stan_params, "}", sep = "\n")
  
  nbinom_extra <- list(NULL, NULL)
  
  if(dstr == "nbinom") {
    nbinom_extra[[1]] <- "  real phi;"
    nbinom_extra[[2]] <- "  phi = 1 / reciprocal_phi;"
  }
  
  stan_tp <- paste(
    "transformed parameters{",
    "  vector[n_difeq] o[n_obs]; // Output from the ODE solver",
    "  real y_hat[n_obs];",
    "  vector[n_difeq] y0;",
    "  real params[n_params];",
    nbinom_extra[[1]],
    "  y0[1] = 7000 - I0;",
    "  y0[2] = 0;",
    "  y0[3] = I0;",
    "  y0[4] = 3000;",
    "  y0[5] = I0;",
    "  params[1] = beta;",
    "  params[2] = rho;",
    fun_exe_line,
    "  y_hat[1] =  o[1, 5]  - y0[5];",
    "  for (i in 1:n_obs-1) {",
    "    y_hat[i + 1] = o[i + 1, 5] - o[i, 5] + 1e-5;",
    "  }",
    nbinom_extra[[2]],
    "}", sep = "\n")
  
  stan_model <- paste(
    "model {",
    "  rho            ~ beta(2, 2);",
    "  beta           ~ lognormal(0, 1);",
    "  I0             ~ lognormal(0, 1);", sep = "\n")
  
  if(dstr == "nbinom") {
    stan_model <- paste(
      stan_model, 
      "  reciprocal_phi ~ normal(0, 1);",
      "  y              ~ neg_binomial_2(y_hat, phi);", 
      "}", sep = "\n")
    
  }
  
  if(dstr == "norm") {
    stan_model <- paste(
      stan_model, 
      "  s              ~ cauchy(0, 2);",
      "  y              ~ normal(y_hat, s);", 
      "}", sep = "\n")
    
  }
  
  if(dstr == "pois") {
    stan_model <- paste(
      stan_model, 
      "  y              ~ poisson(y_hat);", 
      "}", sep = "\n")
    
  }
  
  stan_text   <- paste(stan_fun, stan_data, stan_params,
                       stan_tp, stan_model, sep = "\n")
  
  stan_filepath <- str_glue("./Stan_files/synth_data/mdl_{dstr}.stan")
  
  create_stan_file(stan_text, stan_filepath)
  
  stan_filepath
}