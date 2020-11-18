run_stan <- function(seed, lbl, actual_data, filepaths) {
  
  n_obs <- nrow(actual_data)
  
  print(actual_data$x)
  
  stan_d <- list(n_obs    = n_obs,
                 y        = actual_data$x,
                 n_params = 2,
                 n_difeq  = 5,
                 t0       = 0,
                 ts       = 1:n_obs)
  
  set_cmdstan_path(file.path(Sys.getenv("HOME"), ".cmdstanr", 
                             "cmdstan-2.24.0-rc1"))
  
  stan_filepath <- filepaths[[lbl]]
  mod           <- cmdstan_model(stan_filepath)
  
  fit <- mod$sample(data            = stan_d,
                    seed            = seed,
                    chains          = 4,
                    parallel_chains = 4,
                    iter_warmup     = 1000,
                    iter_sampling   = 1000,
                    refresh         = 5,
                    save_warmup     = TRUE)
  
  rstan::read_stan_csv(fit$output_files())
}