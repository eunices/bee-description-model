source('params.r')
source('code/validate/init.r')

source('code/validate/01-validate.r')

# Load raw data
file <- paste0(dir_model, "data.csv")
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

# Plot summary of MAPE
dfs <- fread(ofile)
summary <- dfs[, list(MAPE = mean(perc)), by = c("ptime", "type")]

lab_naive <- "Naive forecast (no data)"
p1 <- ggplot(summary, aes(x = ptime, y = summary$MAPE, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") + theme +
    ylab("MAPE (%)\n") + xlab("\nPrediction duration") +
    scale_fill_manual(
        values = c('#999999','#E69F00'),
        name = "",
        labels = c("Validation forecast (using data)", lab_naive)
    ) + theme(legend.position="bottom")

output <- paste0(dir_model_validate, "validate-window.png")
ggsave(p1, file = output, width = 7, height = 4)