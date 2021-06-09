get_mase_fit <- function(posterior_df, actual_data) {
  # x is the true hidden incidence
  x_df <- extract_timeseries_var("x", posterior_df)

  df_list <- split(x_df, f = x_df$iter)

  map_df(df_list, function(df) {
    mase_val <- mase(actual_data, df$value)

    data.frame(iter = unique(df$iter), mase = mase_val)
  })
}
