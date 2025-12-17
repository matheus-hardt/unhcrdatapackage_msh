library(unhcrthemes)
# Try to find named colors or specific palette definitions
print("Checking unhcrthemes internal lists...")
tryCatch(
    {
        # Check for unhcr_colors object
        if (exists("unhcr_colors")) {
            print("unhcr_colors found:")
            print(unhcr_colors)
        } else {
            print("unhcr_colors NOT found.")
        }

        # Check for unhcr_pal_poc or similar
        # Sometimes palettes are stored in a list
        # lists usually: pal_unhcr, pal_unhcr_poc, etc.

        # Try to verify if we can get names from the palette
        p <- unhcr_pal(n = 10, "pal_unhcr_poc")
        print("Palette pal_unhcr_poc:")
        print(p)
    },
    error = function(e) {
        print(e)
    }
)
