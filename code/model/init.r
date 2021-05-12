source(paste0('code/libraries.r'))
source(paste0('code/init.r'))
source(paste0('code/model/util.r'))

dir_model_results <- paste0(dir_model, "results/")
if (!dir.exists(dir_model_results)) dir.create(dir_model_results)
