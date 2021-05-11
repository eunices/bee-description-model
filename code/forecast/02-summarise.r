# Load data/ model

# Original data
rfile <- paste0(dir_data, "data.csv")
data_raw <- read.csv(rfile, na.strings=c(""), stringsAsFactor = TRUE) 

# Formatted data
data <- read_rdump(paste0(dir_model, "count_info.data.R")) 

# Load forecast
forecast_years <- forecast_params$fc
wfile <- format_forecast_filename(dir_model_forecast, forecast_years) 
load(wfile) # as "forecast"

# Create mapping
mapping <- create_mapping(data_raw)

# Convert data to df
obs <- convert_data_to_df(data)

# Summarize and output forecast results
forecast_results <- summarize_forecasts(data, data_raw, forecast, obs, mapping)
ofile <- format_forecast_output_filename(dir_model_forecast, forecast_years)
fwrite(forecast_results, ofile)
