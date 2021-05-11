model_params <- list(
    chains = 4,                       # stan chains
    iter = 20000,                     # stan iterations
    adapt_delta = 0.9,                # stan adapt delta
    td = 12,                          # stan tree depth
    va = 0                            # number of validation years
)

forecast_params <- list(
    fc = 10                           # number of forecast years
)