incidence_comparison <- function(pars_df, total_pop){
  sample_rows  <- sample(1:nrow(pars_df), 500, replace = TRUE)
  sample_df    <- pars_df[sample_rows, ]
  consts_df     <- sample_df[ , c("beta", "rho")] %>% 
    rename(par_beta = beta) %>% 
    mutate(N = total_pop)
  
  
  stocks_df     <- dplyr::select(sample_df, I0) %>% rename(I = I0) %>% 
    mutate(S = total_pop - I,
           E = 0,
           R = 0,
           C = I)
  
  cor_out <- sd_sensitivity_run(mdl$deSolve_components, consts_df = consts_df,
                                stocks_df = stocks_df, start_time = 0, 
                                stop_time = 90, multicore = TRUE, n_cores = 4)
  
  split_df <- split(cor_out, cor_out$iter)
  
  cor_inc <- imap_dfr(split_df, function(df, lbl) {
    construct_incidence(df, "pois") %>% mutate(iter = lbl)
  }) %>% 
    mutate(Case = "Correlated parameters")
  
  sample_df <- imap_dfc(pars_df, function(par_col, lbl) {
    samples <- sample(par_col, 500, replace = TRUE)
    df      <- data.frame(samples)
    lbl     -> colnames(df)
    df
  })
  
  consts_df     <- sample_df[ , c("beta", "rho")] %>% 
    rename(par_beta = beta) %>% 
    mutate(N = total_pop)
  
  
  stocks_df     <- dplyr::select(sample_df, I0) %>% rename(I = I0) %>% 
    mutate(S = total_pop - I,
           E = 0,
           R = 0,
           C = I)
  
  ind_out <- sd_sensitivity_run(mdl$deSolve_components, consts_df = consts_df,
                                stocks_df = stocks_df, start_time = 0, 
                                stop_time = 90, multicore = TRUE, n_cores = 4)
  
  split_df <- split(ind_out, ind_out$iter)
  
  ind_inc <- imap_dfr(split_df, function(df, lbl) {
    construct_incidence(df, "pois") %>% mutate(iter = lbl)
  }) %>% 
    mutate(Case = "Independent parameters")
  
  inc_df <- bind_rows(ind_inc, cor_inc)
}