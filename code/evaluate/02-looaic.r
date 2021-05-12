load(paste0(dir_model, "fit.data"))  
loo <- get_loo(fit)
print(loo)

rm(fit)

# To compare between models, use get_loo (util.r) to obtain model fit and  
# use loo::loo_compare e.g., loo_compare(model1, model2, model3) where
# the best model would have highest ELPD.