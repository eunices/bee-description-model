list_to_df <- function(x, i) {
    x <- setDT(tstrsplit(as.character(x), ", ", fixed=TRUE))[]
    names(x) <- paste0("year_", 1:dim(x)[2])
    
    x[] <- lapply(x, function(x) {
        x <- gsub("c\\(|\\)", "", x)
        as.numeric(x)
    })

    x$group <- 1:dim(x)[1]
    x$sim <- i
    x
}
