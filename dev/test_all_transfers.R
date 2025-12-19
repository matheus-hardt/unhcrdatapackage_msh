library(unhcrdatapackage)
library(ggplot2)
library(patchwork)

# Parameters
year <- 2024
country_asylum_iso3c <- "COL"
country_origin_iso3c <- "VEN" # Venezuela

print("----------------------------------------------------------------")
print(paste("Testing plots for Year:", year, " | Asylum:", country_asylum_iso3c, " | Origin:", country_origin_iso3c))
print("----------------------------------------------------------------")

run_test <- function(func, name, ...) {
    cat(paste0("\nTesting ", name, "... "))
    tryCatch(
        {
            p <- func(...)
            if (inherits(p, "ggplot") || inherits(p, "grob")) {
                print(p)
                cat("SUCCESS\n")
            } else {
                cat("SUCCESS (No plot returned or invisible)\n")
            }
        },
        error = function(e) {
            cat(paste0("FAILED: ", e, "\n"))
        }
    )
}

# 1. Asylum
run_test(plot_ctr_asylum, "plot_ctr_asylum", year = year, country_asylum_iso3c = country_asylum_iso3c, lag = 5)

# 2. Destination (Origin based)
run_test(plot_ctr_destination, "plot_ctr_destination", year = year, country_origin_iso3c = country_origin_iso3c, label_font_size = 5, category_font_size = 15)

# 3. Diff in Pop Groups
run_test(plot_ctr_diff_in_pop_groups, "plot_ctr_diff_in_pop_groups", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 5, category_font_size = 8)

# 4. Disp Migrant
run_test(plot_ctr_disp_migrant, "plot_ctr_disp_migrant", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10)

# 5. Key Figures
run_test(plot_ctr_keyfig, "plot_ctr_keyfig", year = year, country_asylum_iso3c = country_asylum_iso3c, population_type_font_size = 4, population_size_font_size = 5, icon_size = 5)

# 6. Location
run_test(plot_ctr_location, "plot_ctr_location", year = year, country_origin_iso3c = country_origin_iso3c, pop_type = c("REF", "ASY", "OIP"), label_font_size = 4, category_font_size = 10)

# 7. Origin History
run_test(plot_ctr_origin_history, "plot_ctr_origin_history", year = year, lag = 5, country_asylum_iso3c = country_asylum_iso3c, category_font_size = 10, legend_font_size = 10)

# 8. Origin Recognition (Origin based)
run_test(plot_ctr_origin_recognition, "plot_ctr_origin_recognition", year = year, country_origin_iso3c = country_origin_iso3c, label_font_size = 4, category_font_size = 10)

# 9. Pop Type Abs
run_test(plot_ctr_population_type_abs, "plot_ctr_population_type_abs", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10)

# 10. Pop Type Per Year
run_test(plot_ctr_population_type_per_year, "plot_ctr_population_type_per_year", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10, legend_font_size = 10)

# 11. Pop Type Perc
run_test(plot_ctr_population_type_perc, "plot_ctr_population_type_perc", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10)

# 12. Process
run_test(plot_ctr_process, "plot_ctr_process", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10)

# 13. Processing Time
run_test(plot_ctr_processing_time, "plot_ctr_processing_time", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10, legend_font_size = 10)

# 14. Pyramid
run_test(plot_ctr_pyramid, "plot_ctr_pyramid", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10)

# 15. Recognition
run_test(plot_ctr_recognition, "plot_ctr_recognition", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 4, category_font_size = 10)

# 16. Solution
run_test(plot_ctr_solution, "plot_ctr_solution", year = year, country_asylum_iso3c = country_asylum_iso3c, category_font_size = 10)

# 17. Solution Recognition
run_test(plot_ctr_solution_recognition, "plot_ctr_solution_recognition", year = year, country_asylum_iso3c = country_asylum_iso3c, category_font_size = 10)

# 18. Treemap
run_test(plot_ctr_treemap, "plot_ctr_treemap", year = year, country_asylum_iso3c = country_asylum_iso3c, label_font_size = 12)

print("Done.")
