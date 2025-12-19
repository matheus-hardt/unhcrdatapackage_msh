# Manual verification script
# Load the package
pkgload::load_all(export_all = FALSE)

# 1. Verify str_replace in plot_ctr_recognition
tryCatch(
    {
        print("Testing plot_ctr_recognition...")
        p1 <- plot_ctr_recognition(
            year = 2024,
            country_asylum_iso3c = "USA",
            top_n_countries = 10,
            measure = "RefugeeRecognitionRate",
            order_by = "TotalDecided"
        )
        print("plot_ctr_recognition successful.")
    },
    error = function(e) {
        print(paste("plot_ctr_recognition FAILED:", e$message))
    }
)

# 2. Verify plot_reg_treemap with empty data
tryCatch(
    {
        print("Testing plot_reg_treemap with empty data...")
        # Simulate empty condition for a region/year that likely has no data or we can assume 2024 might be sparse
        # Or just run the example that failed
        p2 <- plot_reg_treemap(year = 2024, region = "The Americas")
        print("plot_reg_treemap successful.")
    },
    error = function(e) {
        print(paste("plot_reg_treemap FAILED:", e$message))
    }
)
