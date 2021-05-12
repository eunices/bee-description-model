# Load raw data
file <- paste0(dir_data, "data.csv")
data_raw <- read.csv(file, na.strings=c(""), stringsAsFactors = T) 
mapping <- create_mapping(data_raw)

# Read data (without validation data)
files <- dir(dir_model, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Read ref data (with validation data)
files <- dir(dir_model, pattern = 'count_info_ref.data.R', full.names = TRUE)
data_ref <- read_rdump(files)

# Read model
load(paste0(dir_model, "fit.data"))


# Create time series file
ofile <- paste0(dir_model_validate, 'ts.csv')
if(!file.exists(ofile)) {
    n_samples <- 1000
    validation_years <- model_params$va # number of yrs left out for validation
    first_year <- max(data_raw$year) - validation_years
    prediction_windows <- seq(from = 5, to = validation_years, by = 5)

    dfs <- lapply(prediction_windows, get_naive_validation_forecast)

    # Calculate MAPE
    dfs <- rbindlist(dfs)
    dfs$diff <- abs(dfs$model - dfs$obs)
    dfs$perc <- dfs$diff / dfs$obs
    fwrite(dfs, ofile) # persist
}
