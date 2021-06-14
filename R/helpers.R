construct_incidence <- function(sim_df, dist) {
  true_inc <- sim_df %>% filter(time - trunc(time) == 0) %>%
    mutate(x = C - lag(C), 0) %>% filter(time > 0)

  if(dist == "pois") {
    meas_inc <- true_inc %>% mutate(y = rpois(length(x), x)) %>%
      dplyr::select(time, y)
  }

  if(dist == "norm") {
    meas_inc <- true_inc %>% mutate(y = rnorm(length(x),
                                  mean = x,
                                  sd = rhcauchy(length(x)))) %>%
      dplyr::select(time, y)
  }

  if(dist == "nbinom") {
    meas_inc <- true_inc %>% mutate(y = rnbinom(length(x),
                                                mu = x,
                                                size = rhnorm( length(x) ))) %>%
      dplyr::select(time, y)
  }

  meas_inc
}

create_stan_file <- function(stan_text, filename) {
  file_connection <- file(filename)
  writeLines(stan_text, file_connection)
  close(file_connection)
}

predictive_checks <- function(n_sims, sens_o, dist = "pois") {
  map_df(1:n_sims, function(index, sim_df) {

    sim_df <- filter(sim_df, iter == index)

    construct_incidence(sim_df, dist) %>% mutate(iter = index)
  }, sim_df = sens_o)
}

calculate_time <- function(t_list) {
  t_obj <- t_list[[1]]
  (t_obj$toc - t_obj$tic) / 60
}

sd_net_change <- function(sim_df, cumulative_var) {
  temp_df       <- sim_df[, c("time", cumulative_var)]
  temp_df       <- dplyr::filter(temp_df, time - trunc(time) == 0)
  cml_vals      <- temp_df[ , cumulative_var]
  temp_df$value <- cml_vals  - dplyr::lag(cml_vals )
  temp_df       <- dplyr::slice(temp_df, -1)
  temp_df$var   <- paste0("delta_", cumulative_var)
  
  temp_df[, c("time", "value", "var")]
}
