model_params <- list(
    # number of validation years to leave out                 
    va = 0,

    # stan chains
    chains = 4,
    
    # stan iterations
    iter = 50000,     

    # stan adapt delta               
    adapt_delta = 0.95, 

    # stan tree depth
    td = 12
)

forecast_params <- list(
    # number of forecast years
    fc = 10
)

