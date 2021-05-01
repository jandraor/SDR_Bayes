expit <- function(x) {
  1 / (1 + exp(-x))
}

logit <- function(p) {
  log(p / (1 - p))
}


generate_loglik_fun <- function(deSolve_components, data) {
  

  
  init_stocks <- deSolve_components$stocks
  consts      <- deSolve_components$consts
  
  function(pars) {
    
    pars[[1]] <- exp(pars[[1]])
    pars[[2]] <- exp(pars[[2]])
    pars[[3]] <- expit(pars[[3]])
    
    init_stocks[c("I", "C")] <- pars[[1]]
    init_stocks["S"]         <- pop_size - 1570 - pars[[1]] # 1570 is R(0)
    consts["par_beta"]       <- pars[[2]]
    consts["rho"]            <- pars[[3]]
    
    simtime <- seq(0, 91, 1 / 64)
    
    o <- deSolve::ode(y      = init_stocks,
                      times  = simtime,
                      func   = deSolve_components$func,
                      parms  = consts, 
                      method = "rk4")
    
    o_df     <- data.frame(o)
    
    sim_data <- sd_net_change(o_df, "C")
    
    sum(dpois(x = data, lambda = sim_data$value + 1e-5, log = TRUE))

  }
}

logprior <- function(pars) {
  par_I      <- exp(pars[1])
  par_beta   <- exp(pars[2])
  par_rho    <- expit(pars[3])
  
  dlnorm(par_I, meanlog = 0, sd = 1, log = TRUE)      +
    dlnorm(par_beta, meanlog = 0, sd = 1, log = TRUE) +
    dbeta(par_rho, shape1 = 2, shape2 = 2, log = TRUE)
}
