# forecast.r
format_forecast_filename <- function(dir_model_fc, forecast_years) {
	formatted_years <- formatC(forecast_years, width=3, flag=0)
	paste0(dir_model_fc, "forecast-", formatted_years, "-yrs.data")
}


format_forecast_output_filename <- function(dir_model_fc, forecast_years) {
	formatted_years <- formatC(forecast_years, width=3, flag=0)
	paste0(dir_model_fc, "forecast-", formatted_years, "-yrs.csv")
}


posterior_forecast <- function(data, ftime, model, offset = "random") {

	#' A sample from posterior parameters for
	#' predictions of time series
	#' 
	#' Given a model, make predictions based on a predefined timestep
	#' 
	#' @param data Data as output by initial data processing step 
	#' i.e. count_info.data.R
	#' @param ftime Number of years to predict 
	#' @param model rStan model i.e. fit.data
	#' @param offset Offset type, either "random", "decrease" or "constant"
	
	#' "random" represents the last previous years
	#' "decrease" represents a 3% decrease per year
	#' "constant" represents the same as previous years

	# Sample from model posterior
	mp <- sample_model_posterior_parameters(model)
	list2env(mp, .GlobalEnv); rm(mp)

	# Number of groups
	p <- data$P 

	forecast <- list()
	for(g in seq(p)) { # by group
		
		# Offset segment starts at first year
		start <- data$str[g]
		end <- data$end[g]
		all_toff <- data$off[g, ][start:end]

		# Generate offset segment 
		len <- length(all_toff)
		if (offset == "random") {
			# by sampling past ftime of offsets
			toff <- sample(all_toff[(len-ftime):len], ftime, replace = TRUE)
		} else if (offset == "constant") {
			toff <- rep(all_toff[len], ftime)
		} else if (offset == "decreasing") {
			toff <- c(all_toff[len])
			for(i in 1:ftime) {
				toff <- c(toff, round(toff[i]*0.03, 0))
			}
		}

		# Initialize parameters
		# & set first time point
		oo <- data$counts[g, end] # final year's count
		
		lambda <- c(oo)
		theta <- c(0)
		co <- c(oo) # forecast

		for(t in 2:ftime) { # by time

			lambda[t] <- lam(
				coef0, coef1,
				alp, bet, lambda, 
				y = oo,
				grp = g, t = t
			)

			theta[t] <- tht(
				gam, eta, 
				y = oo, 
				grp = g, t = t
			)

			co[t] <- gamlss.dist::rZIP(
				n = 1, 
				mu = (toff[t] + 1) * lambda[t],
				sigma = theta[t]
			)

			oo[t] <- co[t] # current count becomes next expected count

		}

		forecast[[g]] <- co
	}

	forecast
}


# 02-summarise.r
convert_forecast_to_df <- function(data, data_raw, forecast) {

	print("----------- Convert forecast to df (cumulative sum from count)")
	
	myears <- ncol(data$counts)             # number of years used in modelling
	fyears <- length(forecast[[1]][[1]])    # number of forecast years
	min_year <- min(data_raw$year) # earliest year used in modelling
	index <- ((myears + 1):(myears + fyears)) + min_year - 1 # forecast years

	# Forecast coerce to data.frame from list of list and sum counts
	forsim <- lapply(seq(length(forecast)), function(jj) { # each simulation
	# note: there should be 1000 simulations

		lapply(seq(data$P), function(ii) { # each group

			data.frame(
				index = index,
				value = cumsum(forecast[[jj]][[ii]]), # sum the poisson counts
				group = ii,
				sim = jj
			)

		}) %>% rbind.fill
	}) %>% rbind.fill

	forsim

}


summarize_forecasts <- function(data, data_raw, forecast, obs, mapping) {

	# forecast coerced to data.frame from list of list and sum counts
	forsim <- convert_forecast_to_df(data, data_raw, forecast)

	print("----------- Summarize forecast results")
	# For each group and each sim, find the mean and CI of cumsums

	last_observed_count <- data.table(obs)[
		, list(cml_value = max(cml_value)), by = "group"
	] 

	forsim <- data.table(forsim)

	fore_results <- forsim[,
		list(
			value = max(value), # highest value
			index = max(index)  # for the last year
			), 
		by = c("group", "sim")
	][,
		list(
			fore_mu = round(mean(value), 0),
			fore_lower = round(quantile(value, 0.1), 0) ,
			fore_upper = round(quantile(value, 0.9), 0)
		),
		by = "group"
	]

	fore_results <- merge(fore_results, mapping, by='group', all.x=T, all.y=F)
	cols <-  c('groupname', names(fore_results[, 1:4]))
	fore_results[, ..cols]
}

