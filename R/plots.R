plot_daily_incidence <- function(flu_data){
  ggplot(flu_data, aes(x = Date, y = y)) +
    geom_col(fill = "steelblue") +
    theme_pubclean() +
    labs(x = "Time (days)",
         y = "Incidence (People per day)")
}


# ===================Pairs======================================================

dens_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
    scale_fill_viridis_c()
  p
}

cor_fun <- function(data, mapping, method = "pearson", ndp = 2, sz=5, 
                    stars=TRUE, ...){
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  lb.size <- sz* abs(est) 
  
  palette <- gradient_n_pal(c("lightgrey", "black"))
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, 
                                                  c(0, 0.001, 0.01, 0.05, 1))]
    
    lbl  <- paste0(round(est, ndp), stars)
    cor_colour <-  palette(abs(est))
  }else{
    lbl <- round(est, ndp)
  }
  
  ggplot(data = data, mapping = mapping) + 
    annotate("text", x = mean(x, na.rm = TRUE), y = mean(y, na.rm=TRUE), 
             label=lbl, size = 3, colour = cor_colour,...)+
    theme(panel.grid = element_blank())
}

pairs_posterior <- function(posterior, strip_text = 3) {
  ggpairs(posterior, lower = list(continuous = dens_fn),
          upper = list(continuous = cor_fun)) +
    theme_pubr() +
    theme(axis.text = element_text(size = 4),
          strip.text = element_text(size = strip_text))
}

#===============================================================================

fits_plot <- function(posteriors, data_df) {
  summaries_df <- imap_dfr(posteriors, function(pos, lbl) {
    pos$summary_df %>% mutate(ll = lbl)
  })
  
  dists <- names(posteriors)
  
  data_df2 <- map_df(dists, function(ll) {
    data_df %>% mutate(ll = ll)
  })
  
  ggplot(data_df2, aes(x = time, y = y)) +
    geom_ribbon(data = summaries_df, aes(ymin = lb, ymax = ub), fill = "grey90") +
    geom_line(data = summaries_df, colour = "steelblue") +
    geom_point(aes(shape = data), size = 0.5) +
    facet_grid(ll~data) +
    theme_pubr() +
    theme(legend.position = "none")
}

prior_posterior_plot <- function(pars_df) {
  beta_df <- dplyr::select(pars_df, beta)
  
  base <- ggplot(beta_df, aes(x = beta)) +
    geom_density(colour = "steelblue") +
    scale_x_continuous(limits = c(0, 3))
  
  g1 <- base + geom_function(fun = dlnorm, args = list(meanlog = 0, sdlog = 1),
                       colour = "grey50") +
    theme_pubr() +
    labs(x = bquote(beta), y = "") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
  
  rho_df <- dplyr::select(pars_df, rho)
  
  base <- ggplot(rho_df, aes(x = rho)) +
    geom_density(colour = "steelblue") +
    scale_x_continuous(limits = c(0, 1))
  
  g2 <- base + geom_function(fun = dbeta, args = list(shape1 = 2, shape = 2),
                       colour = "grey50") +
    theme_pubr() +
    labs(x = bquote(rho), y = "") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
  
  I0_df <- dplyr::select(pars_df, I0)
  
  base <- ggplot(I0_df, aes(x = I0)) +
    geom_density(colour = "steelblue") +
    scale_x_continuous(limits = c(0, 3))
  
  g3 <- base + geom_function(fun = dlnorm, args = list(meanlog = 0, sdlog = 1),
                       colour = "grey50") +
    theme_pubr() +
    labs(x = "I(0)", y = "") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
  
  grid.arrange(g1, g2, g3, nrow = 1)
}

trace_plot <- function(sf, n_samples = 2000, subtitle = "") {
  posterior  <- rstan::extract(sf, inc_warmup = TRUE, permuted = FALSE)
  
  posterior <- posterior[1:n_samples, ,]
  
  mcmc_trace(posterior, pars = c("beta", "rho", "I0"),
                           facet_args = list(labeller = label_parsed),
                           n_warmup = min(n_samples, 1000)) +
    labs(subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

fit_plot <- function(summary_df, data_df) {
  ggplot(data_df, aes(x = time, y = y)) +
    geom_ribbon(data = summary_df, aes(ymin = lb, ymax = ub), fill = "grey90") +
    geom_line(data = summary_df, colour = "steelblue") +
    geom_point(aes(shape = data)) +
    facet_wrap(~data) +
    theme_pubclean()
}