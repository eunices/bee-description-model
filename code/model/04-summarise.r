print(paste0(Sys.time(), " --- 04-summarise.r"))

# Load data

# Original data
rfile <- paste0(dir_data, "data.csv")
data_raw <- read.csv(rfile, na.strings=c(""), stringsAsFactor = TRUE) 

# Formatted data
data <- read_rdump(paste0(dir_model, "count_info.data.R")) 

# Load zero inflated fits
load(paste0(dir_model, "fit.data"))      # as "fit"

# Load posterior simulation
load(paste0(dir_model, "post.data"))     # as "post"

# Map model indices to original variables
mapping <- create_mapping(data_raw)


####### Check for chain convergence
log_chain_sampling(fit, dir_model)


#######  Get simulations and actual data
li_df <- summarize_simulations_observed(data, post)
Z <- li_df$Z                 # Counts for each year for sim and actual, df
sum_y <- li_df$sum_y         # Cumulative counts for simulated data, dt
obs_count <- li_df$obs_count # Cumulative counts for observed data, dt


####### Get parameter (delta)
li_df_delta <- extract_delta(fit)

group_cf1 <- li_df_delta$group_cf1   # Intercept of delta coefficient
group_cf2 <- li_df_delta$group_cf2   # Slope of delta (beta) coefficient
cf2 <- li_df_delta$cf2               # Summarized slope of delta (beta) & 80 CI


####### Create results table
results <- combine_results(sum_y, obs_count, cf2, mapping)

# Output csv table
ofile <- paste0(dir_model, "results/results.csv")
fwrite(results[order(-observed_species)], ofile)


####### Save plots
# Facet labels for **all** plots
labels <- as.character(mapping$groupname)
names(labels) <- mapping$group

# Plot 1/2 data
li_df_plot1_2 <- prepare_plot1_2_data(Z) # Z as df
obs <- li_df_plot1_2$obs         # Observations with count/ cumulative, df
sims <- li_df_plot1_2$sims       # Simulations with count/ cumulative, df

# Plot 1 - cumulative_fit.pdf (cumulative counts)
save_plot1(sims, obs, labels, dir_model)

# Plot 2 - count_fit.pdf (counts)
save_plot2(sims, obs, labels, dir_model)

# Plot 3 data
li_df_plot3 <- prepare_plot3_data(data, data_raw, group_cf1, group_cf2)
sims <- li_df_plot3$sims         # Simulations for omega, df
om_mean <- li_df_plot3$om_mean   # Mean of omega, dt

# Plot 3 - regression.pdf / counts with regression line
save_plot3(obs, sims, om_mean, labels, dir_model)
# note: obs is output from prepare_plot1_2_data
# note: regression "om_mean" plotted is exp(delta[1] + delta[2] * time)



rm(fit, post, data_raw, data)

