get_naive_validation_forecast <- function(ptime) {

    #' Get naive forecast  and validation forecast 
    #' 
    #' Naive forecast (without data) and validation forecast (with data)
    #' obtained for ptime number of years
    #' 
    #' @param ptime Forecast duration, in years

    #' note: parameters first_year, validation_years, n_samples,
    #' data_ref, fit are all scoped to 04-validate.r

    print(paste0("---- Forecast duration: ", ptime, " years"))
    
    # first_year <- max(data_raw$year) - validation_years

    # Using posterior_sim, predict using data itself
    predictions_sim <- mclapply(1:n_samples, mc.cores = 1, function(ii) {
        predictions_actual <- posterior_sim(data_ref, fit)
        predictions_actual <- lapply(
            predictions_actual, function(x) {
                idx1 <- (length(x)-validation_years+1)
                idx2 <- (length(x)-validation_years+ptime)
                x[idx1:idx2]
        })
        predictions_actual
    })

    # Using posterior_forecast, predict using naive method
    predictions_forecast <- mclapply(
        1:n_samples, mc.cores = 1, function(ii) {
            posterior_forecast(data, ptime, fit)
    })

    # Coerce these lists of lists to data.table
    sim_li <- lapply(1:n_samples, function(i) {
        x <- predictions_sim[[i]]
        list_to_df(x, i)
    })

    forecast_li <- lapply(1:n_samples, function(i) {
        x <- predictions_forecast[[i]]
        list_to_df(x, i)
    })

    sim_df <- rbindlist(sim_li)
    sim_df$type <- "forecast"
    forecast_df <- rbindlist(forecast_li)
    forecast_df$type <- "naive forecast"
    df <- rbind(forecast_df, sim_df)

    # Wide to long
    df_long <- melt(df, id.vars = c("group", "sim", "type"))
    names(df_long)[which(names(df_long) == "variable")] <- "year"
    names(df_long)[which(names(df_long) == "value")] <- "model"
    df_long$year <- 
        as.integer(gsub("year_", "", df_long$year)) + first_year - 1
    df_long$group <- mapping[match(df_long$group, mapping$group),]$groupname

    # Merge with actual counts
    df_obs <- data.table(data_raw)[,
        list(obs = .N),
        by = c("year", "group")
    ]

    df_long <- merge(
        df_long, df_obs,
        by = c("year", "group")
    )

    df_long$ptime <- ptime

    df_long

}
