# Script to print standard UNHCR colors
library(unhcrthemes)

# Get the palette if possible, or define what we expect
# Usually unhcrthemes has a named vector or function
# Inspecting unhcr_pal documentation or structure

print("Checking unhcrthemes colors...")
tryCatch(
    {
        cols <- unhcr_pal(n = 6, "pal_unhcr_poc")
        print(cols)

        # Also check if there are named colors
        # unhcr_colors might exist
        # print(unhcrthemes::unhcr_colors)
    },
    error = function(e) {
        print("Could not retrieve palette directly via unhcr_pal")
        print(e)
    }
)
