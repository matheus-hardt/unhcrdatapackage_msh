# Debug script for failing region plots
source("dev/playground_reg_plots.R")

tryCatch(
    {
        print("Running plot_reg_prop_origin...")
        print(plot_reg_prop_origin(year = 2024, region = "The Americas"))
    },
    error = function(e) {
        print(paste("Error in plot_reg_prop_origin:", e$message))
        traceback()
    }
)

tryCatch(
    {
        print("Running plot_reg_map...")
        print(plot_reg_map(year = 2024, region = "The Americas"))
    },
    error = function(e) {
        print(paste("Error in plot_reg_map:", e$message))
        traceback()
    }
)
