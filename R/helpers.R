construct_incidence <- function(sim_df) {
  sim_df %>% filter(time - trunc(time) == 0) %>% 
    mutate(y = round(C - lag(C), 0)) %>% filter(time > 0) %>% select(time, y)
}

create_stan_file <- function(stan_text, filename) {
  file_connection <- file(filename)
  writeLines(stan_text, file_connection)
  close(file_connection)
}