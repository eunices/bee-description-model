source(paste0('code/libraries.r'))
source(paste0('code/init.r'))
source(paste0('code/model/util.r'))       # posterior_sim
source(paste0('code/forecast/util.r'))    # posterior_forecast
source(paste0('code/validate/util.r'))

# Params
dir_model_validate <- paste0(dir_model, 'validate/')
if (!dir.exists(dir_model_validate)) dir.create(dir_model_validate)
