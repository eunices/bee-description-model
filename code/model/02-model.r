print(paste0(Sys.time(), " --- 02-model.r"))

#  Write to logfile
start <- Sys.time()
conn <- file(filepath_log, "a")
model_li_str <- convert_model_params_to_string(model_params)
write(paste0("Model params: ", model_li_str), conn, sep="\n")
write(paste0("Model started at: ", start), conn, sep="\n", append=T)
close(conn)

# Initial data
files <- dir(dir_model, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Fit model
fit <- stan(

    file = paste0('code/model/discovery.stan'),
    data = data,

    chains = as.numeric(model_params$chains),
    warmup = round(as.numeric(model_params$iter)*0.3, 0), # 30%
    iter = as.numeric(model_params$iter),

    init = 0, thin = 5, cores = 4, verbose = TRUE, seed = 2020,
    
    control = list(
        max_treedepth = as.numeric(model_params$td),
        adapt_delta = as.numeric(model_params$ad)
    )
)

save(fit, file = paste0(dir_model, "fit.data"))

# Write to log file
stop <- Sys.time()
conn <- file(filepath_log, "a")
write(paste0("Model stopped at: ", stop), conn, sep="\n")
write(paste0("Model time elapsed: ", stop-start), conn, sep="\n")
close(conn)



rm(fit)