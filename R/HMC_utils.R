construct_traj_df <- function(HMC_results, n_samples, q_init) {
  map_df(HMC_results[1:n_samples], function(sample_obj) {
    traj <- sample_obj$traj
    
    x_vals <- traj[, 1]
    y_vals <- traj[, 2]
    
    df <- data.frame(x = x_vals[-1], y = y_vals[-1], is_sample = FALSE,
                     is_starting_point = FALSE)
    
    df[nrow(df), "is_sample"] <- TRUE
    
    df
  }) -> traj_df
  
  traj_df <- bind_rows(data.frame(x = q_init[[1]],
                                  y = q_init[[2]],
                                  is_sample = FALSE,
                                  is_starting_point = TRUE), traj_df) 
  
  traj_df
}
