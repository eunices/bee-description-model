print(paste0(Sys.time(), " --- 03-analyse.r"))

# Load data
files <- dir(dir_model, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Load model
load(paste0(dir_model, "fit.data"))

# Save summary of model fit
file <- paste0(dir_model, 'fit.csv')
write.csv(summary(fit)$summary, file)

# Posterior simulations
print(paste0(Sys.time(), " --- Making posterior simulation"))
post <- mclapply(1:1000, mc.cores = 1, function(ii) {
    posterior_sim(data = data, model = fit)
})

# Save posterior simulations
save(post, file = paste0(dir_model, "post.data"))


rm(data, fit, post)


