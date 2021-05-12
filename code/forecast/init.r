source(paste0('code/libraries.r'))
source(paste0('code/init.r'))
source(paste0('code/forecast/util.r'))
source(paste0('code/model/util.r')) # need sample_model_posterior_parameters &
                                    # convert_data_to_df, create_mapping

# Params
dir_model_forecast <- paste0(dir_model, 'forecast/')
if (!dir.exists(dir_model_forecast)) dir.create(dir_model_forecast)
