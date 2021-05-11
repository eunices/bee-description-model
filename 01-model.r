source('params.r')
source('code/model/init.r')

# Create logfiles
filepath_log <- paste0(dir_model,  "model.log")
warnings_log <- paste0(dir_model, "warnings.log")
file.create(filepath_log)
file.create(warnings_log)

# Main script with warning logging
tryCatch(
    withCallingHandlers(
        analysis(), 
        warning = function(w) write_to_log(w, warnings_log)
    ),
    error = function(e) {print(paste0("ERROR: ", conditionMessage(e)))}
) 
# solution from https://stackoverflow.com/questions/37836392/ 


