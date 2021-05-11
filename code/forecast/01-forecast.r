print(paste0(Sys.time(), " --- forecast.r"))

# Load params
forecast_years <- forecast_params$fc

# Load data
files <- dir(dir_model, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Load model
load(paste0(dir_model, "fit.data"))

# Simulate the forecast
forecast <- mclapply(1:1000, mc.cores = 1, function(ii) {
	posterior_forecast(data = data, ftime = forecast_years, model = fit)	 
})

# Save forecast
wfile <- format_forecast_filename(dir_model_forecast, forecast_years) 
save(forecast, file = wfile)



rm(data, fit, forecast)

