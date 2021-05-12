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