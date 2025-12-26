# Playground for Region Plots (Improved)
# Goal: Improve style and parameters to match dev_plot_Country

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(unhcrthemes)
library(countrycode)
library(treemapify)
library(mapsf)
library(sf)
library(rnaturalearth)
library(WDI)
library(circlize)
library(forcats)
library(stringr)
library(patchwork)
library(fontawesome)
library(ggsvg)

# --- Standard Constants for UNHCR Style ---
# Defined to match dev_plot_Country
dict_pop_type_label <- c(
    "refugees" = "REF",
    "returned_refugees" = "RETURNED_REF",
    "asylum_seekers" = "ASY",
    "idps" = "IDP",
    "returned_idps" = "RETURNED_IDP",
    "oip" = "OIP",
    "stateless" = "STA",
    "ooc" = "OOC",
    "hst" = "HST"
)

cols_poptype <- c(
    "Refugees" = "#0072BC",
    "Asylum-seekers" = "#6CD8FD",
    "Internally displaced\npersons" = "#32C189",
    "Other people in need\nof international protection" = "#D25A45",
    "Stateless people" = "#FFC740",
    "Others of concern\nto UNHCR" = "#A097E3",
    "Host community" = "#BFBFBF",
    "Returned refugees" = "#00B398",
    "Returned idps" = "#00B398",
    "Other" = "#BFBFBF"
)

# For plots that use abbreviations keys
cols_poptype_abbr <- c(
    "REF" = "#0072BC",
    "ASY" = "#6CD8FD",
    "IDP" = "#32C189",
    "OIP" = "#D25A45",
    "STA" = "#FFC740",
    "OOC" = "#A097E3",
    "HST" = "#BFBFBF",
    "HCO" = "#BFBFBF"
)

# -------------------------------------------------------------------------
# NEW: Key Figures (Regional)
# Adapted from plot_ctr_keyfig
# -------------------------------------------------------------------------

plot_reg_keyfig <- function(
  year = 2024,
  region = "The Americas",
  population_type_font_size = 4,
  population_size_font_size = 5,
  icon_size = 5
) {
    # Calculate Total POC for current year in Region
    total_poc <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region", warn = FALSE)) |>
        dplyr::filter(unhcr_region == region) |>
        dplyr::summarise(
            total = sum(
                refugees, asylum_seekers, idps, oip, stateless, ooc, hst,
                na.rm = TRUE
            )
        ) |>
        dplyr::pull()

    # Calculate Total POC for previous year
    total_poc_prev <- refugees::population |>
        dplyr::filter(year == (!!year - 1)) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region", warn = FALSE)) |>
        dplyr::filter(unhcr_region == region) |>
        dplyr::summarise(
            total = sum(
                refugees, asylum_seekers, idps, oip, stateless, ooc, hst,
                na.rm = TRUE
            )
        ) |>
        dplyr::pull()

    perc_change_poc <- (total_poc - total_poc_prev) / total_poc_prev * 100

    # Icon mapping
    icon_map <- c(
        "refugees" = "person-walking-luggage",
        "asylum_seekers" = "scale-balanced",
        "idps" = "house-chimney-crack",
        "oip" = "hand-holding-heart",
        "stateless" = "globe",
        "ooc" = "users",
        "hst" = "handshake"
    )

    # Prepare data
    keyfig <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region", warn = FALSE)) |>
        dplyr::filter(unhcr_region == region) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::group_by(population_type) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
        dplyr::mutate(
            population_type_label = dplyr::case_when(
                population_type == "refugees" ~ "Refugees",
                population_type == "asylum_seekers" ~ "Asylum-seekers",
                population_type == "idps" ~ "Internally displaced\npersons",
                population_type == "oip" ~ "Other people in need\nof international protection",
                population_type == "stateless" ~ "Stateless people",
                population_type == "ooc" ~ "Others of concern\nto UNHCR",
                population_type == "hst" ~ "Host community",
                TRUE ~ "Other"
            ),
            population_type_label = factor(
                population_type_label,
                levels = c(
                    "Refugees", "Asylum-seekers", "Internally displaced\npersons",
                    "Other people in need\nof international protection",
                    "Host community", "Others of concern\nto UNHCR", "Stateless people"
                )
            )
        ) |>
        dplyr::filter(value > 0) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            pop_size_label = format(round(value, 0), big.mark = ","),
            color_hex = ifelse(
                as.character(population_type_label) %in% names(cols_poptype),
                cols_poptype[as.character(population_type_label)],
                "#333333"
            ),
            icon_name = ifelse(
                population_type %in% names(icon_map),
                icon_map[population_type],
                "circle-question"
            ),
            svg_text = as.character(fontawesome::fa(icon_name, fill = color_hex))
        ) |>
        dplyr::ungroup()

    p <- ggplot2::ggplot(keyfig) +
        ggsvg::geom_point_svg(
            ggplot2::aes(x = 0.5, y = 1.8, svg = svg_text),
            size = icon_size
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x = 0.5, y = 1.1, label = pop_size_label, color = population_type_label
            ),
            family = "Lato", size = population_size_font_size, fontface = "bold"
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x = 0.5, y = 0.4, label = population_type_label, color = population_type_label
            ),
            family = "Lato", size = population_type_font_size, fontface = "bold"
        ) +
        ggplot2::scale_color_manual(values = cols_poptype) +
        ggplot2::xlim(c(0, 1)) +
        ggplot2::ylim(c(0, 3)) +
        ggplot2::facet_wrap(ggplot2::vars(population_type_label), ncol = 2) +
        ggplot2::labs(
            title = paste0("Key Figures for ", region, " as of ", year),
            subtitle = paste0(
                "A total of ",
                format(round(total_poc, 0), scientific = FALSE, big.mark = ","),
                " people, ",
                ifelse(perc_change_poc > 0, "increasing", "decreasing"),
                " by ",
                round(abs(perc_change_poc), 1),
                "% compared to the previous year"
            ),
            caption = "Source: UNHCR.org/refugee-statistics"
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
            strip.text = ggplot2::element_blank(),
            legend.position = "none",
            plot.title = ggplot2::element_text(family = "Lato", face = "bold", size = 18, hjust = 0.5),
            plot.subtitle = ggplot2::element_text(family = "Lato", size = 12, hjust = 0.5, margin = ggplot2::margin(b = 20))
        )

    return(p)
}

# -------------------------------------------------------------------------
# 1. Population group in the region (Treemap) - Refactored
# -------------------------------------------------------------------------

plot_reg_treemap <- function(year = 2024,
                             region = "The Americas",
                             pop_type = c("REF", "ASY", "IDP", "OIP", "OOC", "STA"),
                             label_font_size = 15) {
    datatree_pre <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region", warn = FALSE)) |>
        dplyr::filter(unhcr_region == region)

    if (nrow(datatree_pre) == 0) {
        p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1, size = 12, label = paste0("No records for ", region, " in ", year)) +
            ggplot2::theme_void()
        return(p)
    }

    datatree <- datatree_pre |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::mutate(population_type_label = dplyr::case_when(
            population_type == "refugees" ~ "REF",
            population_type == "asylum_seekers" ~ "ASY",
            population_type == "idps" ~ "IDP",
            population_type == "oip" ~ "OIP",
            population_type == "stateless" ~ "STA",
            population_type == "ooc" ~ "OOC",
            population_type == "hst" ~ "HCO",
            TRUE ~ "Other"
        )) |>
        dplyr::mutate(population_type_name = dplyr::case_when(
            population_type == "refugees" ~ "Refugees",
            population_type == "asylum_seekers" ~ "Asylum-seekers",
            population_type == "idps" ~ "Internally displaced persons",
            population_type == "oip" ~ "Other people in need of international protection",
            population_type == "stateless" ~ "Stateless people",
            population_type == "ooc" ~ "Others of concern to UNHCR",
            population_type == "hst" ~ "Host community",
            TRUE ~ "Other"
        )) |>
        dplyr::filter(population_type_label %in% pop_type) |>
        dplyr::group_by(year, unhcr_region, population_type_label, population_type_name) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::ungroup() |>
        dplyr::mutate(
            freqinReg = scales::label_percent(accuracy = .1, suffix = "%")(value / sum(value))
        )

    if (nrow(datatree) == 0 || sum(datatree$value) == 0) {
        p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1, size = 12, label = paste0("No records for ", region, " in ", year)) +
            ggplot2::theme_void()
        return(p)
    }

    p <- ggplot2::ggplot(
        datatree,
        ggplot2::aes(
            area = value,
            fill = population_type_label,
            label = paste0(freqinReg, "\n", stringr::str_wrap(population_type_name, 20))
        )
    ) +
        treemapify::geom_treemap() +
        treemapify::geom_treemap_text(
            colour = "white",
            place = "centre", size = label_font_size,
            family = "Lato"
        ) +
        ggplot2::scale_fill_manual(values = cols_poptype_abbr) +
        unhcrthemes::theme_unhcr(font_size = 14) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(
            title = paste0("Population of Concern & Affected Host Communities in ", region),
            subtitle = paste0("As of ", year, ", a total of ", format(round(sum(datatree$value), -3), big.mark = ","), " Individuals"),
            x = "",
            y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        )

    return(p)
}

# -------------------------------------------------------------------------
# 2. Plot World Comparison - Refactored
# -------------------------------------------------------------------------

plot_reg_share <- function(year = 2024,
                           region = "The Americas",
                           pop_type = "REF",
                           label_font_size = 15) {
    pop_typelabel <- dplyr::case_when(
        pop_type == "REF" ~ "Refugees",
        pop_type == "IDP" ~ "Internally displaced persons",
        pop_type == "ASY" ~ "Asylum seekers",
        pop_type == "OOC" ~ "Others of concern to UNHCR",
        pop_type == "STA" ~ "Stateless Persons",
        pop_type == "OIP" ~ "Other people in need of international protection",
        pop_type == "HCO" ~ "Host community"
    )

    datatree <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::mutate(population_type_label = dplyr::case_when(
            population_type == "refugees" ~ "REF",
            population_type == "asylum_seekers" ~ "ASY",
            population_type == "idps" ~ "IDP",
            population_type == "oip" ~ "OIP",
            population_type == "stateless" ~ "STA",
            population_type == "ooc" ~ "OOC",
            population_type == "hst" ~ "HCO",
            TRUE ~ "Other"
        )) |>
        dplyr::filter(population_type_label %in% pop_type) |>
        dplyr::mutate(Compare = ifelse(unhcr_region == region, region, "Rest of the World")) |>
        dplyr::group_by(year, Compare, population_type_label, population_type) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(freq = scales::label_percent(accuracy = .1, suffix = "%")(value / sum(value)))

    p <- ggplot2::ggplot(
        datatree,
        ggplot2::aes(
            area = value,
            fill = Compare,
            label = paste0(format(round(datatree$value, -3), big.mark = ","), "\n in ", Compare)
        )
    ) +
        treemapify::geom_treemap() +
        treemapify::geom_treemap_text(
            colour = "white",
            place = "centre", size = label_font_size,
            family = "Lato"
        ) +
        unhcrthemes::scale_fill_unhcr_d(palette = "pal_unhcr") +
        unhcrthemes::theme_unhcr(font_size = 14) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(
            title = paste0("Share of ", pop_typelabel, " within ", region),
            subtitle = stringr::str_wrap(paste0(
                "As of ", year,
                ", about ",
                datatree |> dplyr::filter(Compare == region) |> dplyr::select(freq) |> dplyr::pull(),
                " of that global population category are hosted in the region"
            ), 80),
            x = "",
            y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        )

    return(p)
}

# -------------------------------------------------------------------------
# 3. Evolution Over Time - Refactored
# -------------------------------------------------------------------------

plot_reg_population_type_per_year <- function(year = 2024,
                                              region = "The Americas",
                                              lag = 5,
                                              pop_type = c("REF", "ASY", "IDP", "OIP", "STA", "OOC"),
                                              label_font_size = 4,
                                              category_font_size = 10,
                                              legend_font_size = 10) {
    if (length(pop_type) == 1) {
        pop_type <- pop_type
    } # Redundant but safe

    pop_type_label_dict <- c(
        "REF" = "Refugees",
        "ASY" = "Asylum-seekers",
        "IDP" = "Internally displaced persons",
        "OIP" = "Other people in need of international protection",
        "STA" = "Stateless people",
        "OOC" = "Others of concern to UNHCR",
        "HST" = "Host community"
    )

    df <- refugees::population |>
        dplyr::filter(year >= (!!year - lag) & year <= !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::mutate(
            population_type_label = dplyr::case_when(
                population_type == "refugees" ~ "REF",
                population_type == "asylum_seekers" ~ "ASY",
                population_type == "idps" ~ "IDP",
                population_type == "oip" ~ "OIP",
                population_type == "stateless" ~ "STA",
                population_type == "ooc" ~ "OOC",
                population_type == "hst" ~ "HST",
                TRUE ~ "Other"
            )
        ) |>
        dplyr::filter(population_type_label %in% pop_type) |>
        dplyr::group_by(year, population_type_label) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(population_type_name = dplyr::recode(population_type_label, !!!pop_type_label_dict))

    p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, colour = population_type_label)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 1.1, colour = "#333333") +
        ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        ggplot2::xlim(c(year - lag, year + 1)) +
        # Use standardized manual colors if possible, or unhcr_d
        ggplot2::scale_color_manual(
            values = cols_poptype_abbr,
            labels = pop_type_label_dict
        ) +
        # ggplot2::geom_label(
        #     data = df |> dplyr::filter(year == max(year)),
        #     ggplot2::aes(
        #         x = year + .5,
        #         y = value,
        #         label = population_type_name,
        #         color = population_type_label
        #     ),
        #     hjust = 0,
        #     vjust = 0.5,
        #     fill = "white",
        #     linewidth = NA,
        #     family = "Lato",
        #     size = label_font_size
        # ) +
        unhcrthemes::theme_unhcr(
            grid = "Y",
            axis = "x",
            axis_title = "y",
            font_size = 14
        ) +
        ggplot2::theme(
            legend.position = "top",
            legend.justification = "left",
            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = category_font_size),
            legend.text = ggplot2::element_text(size = legend_font_size),
            legend.title = ggplot2::element_blank()
        ) +
        ggplot2::labs(
            title = paste0("Evolution of Population of Concern in ", region),
            subtitle = paste0("Evolution in the past ", lag, " years"),
            x = "",
            y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        )

    return(p)
}

# -------------------------------------------------------------------------
# 4. Plot Population Origin-Destination - Refactored
# -------------------------------------------------------------------------
# Note: Chord diagram is base graphics, styling is limited. Kept as is.

plot_reg_origin_dest <- function(year = 2024, region = "The Americas") {
    chords <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        dplyr::mutate(
            refugees = tidyr::replace_na(refugees, 0),
            asylum_seekers = tidyr::replace_na(asylum_seekers, 0),
            oip = tidyr::replace_na(oip, 0),
            total = refugees + asylum_seekers + oip
        ) |>
        dplyr::group_by(coo_name, coa_name) |>
        dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
            CountryAsylumName = forcats::fct_lump_prop(coa_name, prop = .02, w = total),
            CountryOriginName = forcats::fct_lump_prop(coo_name, prop = .02, w = total)
        ) |>
        dplyr::group_by(CountryOriginName, CountryAsylumName) |>
        dplyr::summarize(total = sum(total), .groups = "drop") |>
        dplyr::mutate(
            CountryOriginName = stringr::str_replace(CountryOriginName, " \\(Bolivarian Republic of\\)", ""),
            CountryAsylumName = stringr::str_replace(CountryAsylumName, " \\(Bolivarian Republic of\\)", ""),
            CountryOriginName = stringr::str_replace(CountryOriginName, " \\(Plurinational State of\\)", ""),
            CountryAsylumName = stringr::str_replace(CountryAsylumName, " \\(Plurinational State of\\)", ""),
            CountryOriginName = stringr::str_replace(CountryOriginName, "United States of America", "USA"),
            CountryAsylumName = stringr::str_replace(CountryAsylumName, "United States of America", "USA")
        ) |>
        dplyr::rename(
            origin = CountryOriginName,
            destination = CountryAsylumName,
            value = total
        ) |>
        dplyr::filter(origin != destination, value > 0)

    chords <- as.data.frame(chords)

    if (nrow(chords) == 0) {
        message(paste("No significant population movement found for", region, "in", year))
        return(invisible(NULL))
    }

    n_sectors <- length(unique(c(chords$origin, chords$destination)))
    if (n_sectors < 2) {
        message(paste("Insufficient unique origins/destinations for", region))
        return(invisible(NULL))
    }

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    circlize::circos.clear()
    graphics::par(family = "Lato")

    circlize::chordDiagram(
        chords,
        directional = 1,
        direction.type = c("diffHeight", "arrows"),
        diffHeight = 0.02,
        link.arr.length = 0.1,
        link.arr.width = 0.1,
        link.arr.type = "big.arrow",
        annotationTrack = "grid",
        preAllocateTracks = list(track.height = 0.15)
    )

    circlize::circos.track(
        track.index = 1,
        panel.fun = function(x, y) {
            circlize::circos.text(
                circlize::CELL_META$xcenter,
                circlize::CELL_META$ylim[1],
                circlize::CELL_META$sector.index,
                facing = "clockwise",
                niceFacing = TRUE,
                adj = c(0, 0.5)
            )
        },
        bg.border = NA
    )

    graphics::title(
        main = "Movement of Forcibly Displaced Population",
        sub = paste0("In ", region, " as of ", year),
        cex.main = 1.5
    )
    circlize::circos.clear()
    return(invisible(NULL))
}

# -------------------------------------------------------------------------
# 5. Plot Main country of Asylum - Refactored
# -------------------------------------------------------------------------

plot_reg_population_type_abs <- function(year = 2024,
                                         region = "The Americas",
                                         top_n_countries = 5,
                                         pop_type = "REF",
                                         show_diff_label = TRUE,
                                         label_font_size = 6,
                                         category_font_size = 10) {
    pop_type_label_dict <- c(
        "REF" = "Refugees",
        "ASY" = "Asylum-seekers",
        "IDP" = "Internally displaced persons",
        "OIP" = "Other people in need of international protection",
        "STA" = "Stateless people",
        "OOC" = "Others of concern to UNHCR",
        "HST" = "Host community"
    )

    pop_type_col <- dplyr::case_when(
        pop_type == "REF" ~ "refugees",
        pop_type == "ASY" ~ "asylum_seekers",
        pop_type == "IDP" ~ "idps",
        pop_type == "OIP" ~ "oip",
        pop_type == "STA" ~ "stateless",
        pop_type == "OOC" ~ "ooc",
        pop_type == "HST" ~ "hst",
        TRUE ~ "refugees"
    )

    df <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        dplyr::select(coa_name, !!pop_type_col) |>
        dplyr::rename(value = !!pop_type_col) |>
        dplyr::group_by(coa_name) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
        dplyr::arrange(dplyr::desc(value)) |>
        utils::head(top_n_countries)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(coa_name, value), y = value)) +
        ggplot2::geom_bar(stat = "identity", position = "identity", fill = "#0072bc") +
        ggplot2::geom_label(
            ggplot2::aes(
                x = coa_name,
                y = value,
                label = scales::label_number(scale_cut = scales::cut_short_scale())(value)
            ),
            hjust = 1,
            vjust = 0.5,
            colour = "white",
            fill = NA,
            linewidth = NA,
            family = "Lato",
            size = label_font_size
        ) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 1, colour = "#333333") +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = paste0("Top ", top_n_countries, " ", pop_type_label_dict[pop_type], " hosting countries"),
            subtitle = paste0("In ", region, " as of ", year),
            x = "",
            y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        unhcrthemes::theme_unhcr(
            grid = "X",
            axis = "y",
            axis_title = "x",
            font_size = 14
        ) +
        ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = category_font_size)
        )

    return(p)
}

# -------------------------------------------------------------------------
# 6. Plot Biggest Decrease - Refactored
# -------------------------------------------------------------------------

plot_reg_decrease <- function(year = 2024,
                              lag = 5,
                              topn = 5,
                              region = "The Americas",
                              pop_type = c("REF", "ASY", "OIP"),
                              category_font_size = 10) {
    thisyear <- year
    baseline <- thisyear - lag
    data <- refugees::population |>
        dplyr::filter(year == baseline | year == thisyear) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::mutate(population_type_label = dplyr::case_when(
            population_type == "refugees" ~ "REF",
            population_type == "asylum_seekers" ~ "ASY",
            population_type == "idps" ~ "IDP",
            population_type == "oip" ~ "OIP",
            population_type == "stateless" ~ "STA",
            population_type == "ooc" ~ "OOC",
            population_type == "hst" ~ "HCO",
            TRUE ~ "Other"
        )) |>
        dplyr::filter(population_type_label %in% pop_type) |>
        dplyr::mutate(year = dplyr::case_when(
            year == thisyear ~ paste("thisyear"),
            year == baseline ~ paste("baseline")
        )) |>
        dplyr::group_by(coa_name, year) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::select(coa_name, year, value) |>
        dplyr::mutate(
            coa_name = stringr::str_replace(coa_name, " \\(Bolivarian Republic of\\)", ""),
            coa_name = stringr::str_replace(coa_name, "Iran \\(Islamic Republic of\\)", "Iran"),
            coa_name = stringr::str_replace(coa_name, "United Kingdom of Great Britain and Northern Ireland", "UK")
        ) |>
        tidyr::spread(year, value) |>
        dplyr::mutate(gap = baseline - thisyear) |>
        dplyr::arrange(dplyr::desc(gap)) |>
        utils::head(topn) |>
        tidyr::gather(
            key = year,
            value = value,
            -coa_name,
            -gap
        ) |>
        dplyr::mutate(year = dplyr::case_when(
            year == "thisyear" ~ paste(thisyear),
            year == "baseline" ~ paste(baseline)
        ))

    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = stats::reorder(coa_name, gap),
            y = value,
            fill = as.factor(year)
        )
    ) +
        ggplot2::coord_flip() +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_hline(yintercept = 0, linewidth = 1, colour = "#333333") +
        ggplot2::scale_fill_manual(values = c("#0072bc", "#FAAB18")) +
        ggplot2::labs(
            title = "Biggest Decrease of Population",
            subtitle = paste0(
                topn, " Biggest change in Refugee Population, ",
                region, " ", baseline, " - ", thisyear
            ),
            x = "", y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_number(
            accuracy = 1,
            scale_cut = scales::cut_short_scale()
        )) +
        unhcrthemes::theme_unhcr(font_size = 14) +
        ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = category_font_size)
        )

    return(p)
}

# -------------------------------------------------------------------------
# 7. Plot Biggest Increase - Refactored
# -------------------------------------------------------------------------
# Note: Keeping mostly same but improving theme if needed

plot_reg_increase <- function(year = 2024,
                              lag = 5,
                              topn = 5,
                              region = "The Americas",
                              pop_type = c("REF", "ASY", "OIP"),
                              category_font_size = 10) {
    pop_type_label_dict <- c(
        "REF" = "Refugees",
        "ASY" = "Asylum-seekers",
        "IDP" = "Internally displaced persons",
        "OIP" = "Other people in need of international protection",
        "STA" = "Stateless people",
        "OOC" = "Others of concern to UNHCR",
        "HST" = "Host community"
    )

    thisyear <- year
    baseline <- thisyear - lag

    col_this_year <- paste0("year_", thisyear)
    col_baseline <- paste0("year_", baseline)

    data_pivot <- refugees::population |>
        dplyr::filter(year == baseline | year == thisyear) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::mutate(population_type_label = dplyr::case_when(
            population_type == "refugees" ~ "REF",
            population_type == "asylum_seekers" ~ "ASY",
            population_type == "idps" ~ "IDP",
            population_type == "oip" ~ "OIP",
            population_type == "stateless" ~ "STA",
            population_type == "ooc" ~ "OOC",
            population_type == "hst" ~ "HCO",
            TRUE ~ "Other"
        )) |>
        dplyr::filter(population_type_label %in% pop_type) |>
        dplyr::group_by(coa_name, year) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
            coa_name = stringr::str_replace(coa_name, "Democratic Republic of the Congo", "DRC")
        ) |>
        dplyr::mutate(year_col = paste0("year_", year)) |>
        tidyr::pivot_wider(names_from = year_col, values_from = value, values_fill = NA)

    required_cols <- c("coa_name", col_this_year, col_baseline)

    data <- data_pivot |>
        dplyr::select(tidyselect::any_of(required_cols)) |>
        dplyr::mutate(
            !!col_this_year := dplyr::coalesce(dplyr::if_any(tidyselect::all_of(col_this_year), .default = NA, ~.), 0),
            !!col_baseline := dplyr::coalesce(dplyr::if_any(tidyselect::all_of(col_baseline), .default = NA, ~.), 0)
        ) |>
        dplyr::mutate(gap = .data[[col_this_year]] - .data[[col_baseline]]) |>
        dplyr::arrange(dplyr::desc(gap)) |>
        utils::head(topn)

    if (nrow(data) == 0) {
        message(paste("No increase data found for", region, "between", baseline, "and", thisyear))
        return(invisible(NULL))
    }

    aes_x_baseline <- paste0("year_", baseline)
    aes_x_thisyear <- paste0("year_", thisyear)

    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[[aes_x_baseline]],
            xend = .data[[aes_x_thisyear]],
            y = stats::reorder(coa_name, gap),
            group = coa_name
        )
    ) +
        ggplot2::geom_segment(
            ggplot2::aes(
                x = .data[[aes_x_baseline]],
                xend = .data[[aes_x_thisyear]],
                y = stats::reorder(coa_name, gap),
                yend = stats::reorder(coa_name, gap)
            ),
            colour = "#dddddd",
            linewidth = 3
        ) +
        ggplot2::geom_point(
            ggplot2::aes(x = .data[[aes_x_baseline]], y = stats::reorder(coa_name, gap)),
            colour = "#0072bc",
            size = 3
        ) +
        ggplot2::geom_point(
            ggplot2::aes(x = .data[[aes_x_thisyear]], y = stats::reorder(coa_name, gap)),
            colour = "#FAAB18",
            size = 3
        ) +
        ggplot2::labs(
            title = paste0("Biggest Increase in Population of Concern (", paste(pop_type_label_dict[pop_type], collapse = ", "), ")"),
            subtitle = paste0("Top ", topn, " Asylum Countries showing the biggest increase, ", baseline, " - ", thisyear),
            x = "Population", y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        ) +
        ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        unhcrthemes::theme_unhcr(font_size = 14) +
        ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = category_font_size)
        )

    return(p)
}

# -------------------------------------------------------------------------
# 8. Proportion of the population who are refugees (origin) - Refactored
# -------------------------------------------------------------------------

plot_reg_prop_origin <- function(year = 2024, region = "The Americas", label_font_size = 6, category_font_size = 10) {
    wb_data <- WDI::WDI(
        country = "all",
        indicator = c(
            "SP.POP.TOTL", "NY.GDP.MKTP.CD",
            "NY.GDP.PCAP.CD", "NY.GNP.PCAP.CD"
        ),
        start = year - 1,
        end = year - 1,
        extra = TRUE
    )

    # Rename year to Year to match downstream code expectations
    if ("year" %in% names(wb_data)) {
        wb_data <- wb_data |> dplyr::rename(Year = year)
    }

    departed <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coo_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, oip),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::group_by(coo_name, coo_iso) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(coo_name = stringr::str_replace(
            coo_name,
            " \\(Bolivarian Republic of\\)", ""
        )) |>
        dplyr::left_join(
            wb_data |>
                dplyr::select("SP.POP.TOTL", "iso3c", "Year") |>
                dplyr::filter(Year == year - 1),
            by = c("coo_iso" = "iso3c")
        ) |>
        dplyr::mutate(ref.part = round(value / (SP.POP.TOTL + value), 4)) |>
        dplyr::arrange(dplyr::desc(ref.part)) |>
        utils::head(10)

    p <- ggplot2::ggplot(
        departed,
        ggplot2::aes(x = ref.part, forcats::fct_reorder(coo_name, ref.part))
    ) +
        ggplot2::geom_col(fill = "#0072BC") +
        ggplot2::geom_label(ggplot2::aes(label = scales::label_percent(accuracy = .1)(ref.part)),
            hjust = 1,
            vjust = 0.5,
            colour = "white",
            fill = NA,
            linewidth = NA,
            family = "Lato",
            size = label_font_size
        ) +
        ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = .1)) +
        ggplot2::labs(
            x = NULL,
            y = NULL,
            title = stringr::str_wrap(paste0("Number of refugees, asylum seekers & displaced across borders by country of origin in ", region), 60),
            subtitle = stringr::str_wrap("Top 10 Countries, as a proportion of the national population of that country of origin (SDG indicator 10.7.4)", 80),
            caption = stringr::str_wrap("Source: UNHCR.org/refugee-statistics. \n Total count of population who have been recognized as refugees as a proportion of the total population of their country of origin, expressed per 100,000 population. Refugees refers to persons recognized by the Government and/or UNHCR, or those in a refugee-like situation.  Population refers to total resident population in a given country in a given year.", 100)
        ) +
        unhcrthemes::theme_unhcr(
            grid = "X",
            axis = "y",
            axis_title = "x",
            font_size = 14
        ) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = category_font_size)
        )

    return(p)
}

# -------------------------------------------------------------------------
# 9. Refugee Status Determination - Refactored
# -------------------------------------------------------------------------

plot_reg_rsd <- function(year = 2024,
                         region,
                         top_n_countries = 10,
                         measure = "Recognized",
                         category_font_size = 10) {
    measurelabel <- dplyr::case_when(
        measure == "Recognized" ~ "Recognized Refugee Status Decisions",
        measure == "ComplementaryProtection" ~ "Complementary Protection Decisions",
        measure == "TotalDecided" ~ "Total Decision (independently of the outcome)",
        measure == "RefugeeRecognitionRate" ~ "Refugee Recognition Rate",
        measure == "TotalRecognitionRate" ~ "Total Recognition Rate",
        TRUE ~ measure
    )

    data <- refugees::asylum_decisions |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        dplyr::mutate(DecisionsAveragePersonsPerCase = 1)

    topOrigin <- data |>
        dplyr::rename(CountryOriginName = coo_name) |>
        dplyr::group_by(CountryOriginName) |>
        dplyr::summarize(
            Recognized = sum(dec_recognized, na.rm = TRUE),
            ComplementaryProtection = sum(dec_other, na.rm = TRUE),
            TotalDecided = sum(dec_total, na.rm = TRUE)
        ) |>
        dplyr::mutate(
            RefugeeRecognitionRate = (Recognized) / TotalDecided,
            TotalRecognitionRate = (Recognized + ComplementaryProtection) / TotalDecided
        ) |>
        dplyr::mutate(CountryOriginName = stringr::str_replace(CountryOriginName, " \\(Bolivarian Republic of\\)", ""))

    topOrigin1 <- topOrigin |>
        dplyr::mutate(measured = .data[[measure]]) |>
        dplyr::arrange(dplyr::desc(measured)) |>
        utils::head(top_n_countries)

    topasyl <- data |>
        dplyr::rename(CountryAsylumName = coa_name) |>
        dplyr::group_by(CountryAsylumName) |>
        dplyr::summarize(
            Recognized = sum(dec_recognized, na.rm = TRUE),
            ComplementaryProtection = sum(dec_other, na.rm = TRUE),
            TotalDecided = sum(dec_total, na.rm = TRUE)
        ) |>
        dplyr::mutate(
            RefugeeRecognitionRate = (Recognized) / TotalDecided,
            TotalRecognitionRate = (Recognized + ComplementaryProtection) / TotalDecided
        )

    topasyl1 <- topasyl |>
        dplyr::mutate(measured = .data[[measure]]) |>
        dplyr::arrange(dplyr::desc(measured)) |>
        utils::head(top_n_countries)

    rsdasyl <- ggplot2::ggplot(topasyl1, ggplot2::aes(
        y = measured,
        x = stats::reorder(CountryAsylumName, measured)
    )) +
        ggplot2::scale_y_continuous(labels = ifelse(measure %in% c("RefugeeRecognitionRate", "TotalRecognitionRate"),
            scales::label_percent(accuracy = 0, suffix = "%"),
            scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale())
        )) +
        ggplot2::geom_bar(stat = "identity", fill = "#0072bc") +
        ggplot2::coord_flip() +
        ggplot2::labs(
            subtitle = paste0("For top ", top_n_countries, " Countries of Asylum"),
            x = " ",
            y = " "
        ) +
        unhcrthemes::theme_unhcr(
            grid = "Y",
            axis = "x", axis_title = "",
            font_size = category_font_size
        ) +
        ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank()
        )

    rsdorigin <- ggplot2::ggplot(topOrigin1, ggplot2::aes(
        y = measured,
        x = stats::reorder(CountryOriginName, measured)
    )) +
        ggplot2::scale_y_continuous(labels = ifelse(measure %in% c("RefugeeRecognitionRate", "TotalRecognitionRate"),
            scales::label_percent(accuracy = 0, suffix = "%"),
            scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale())
        )) +
        ggplot2::geom_bar(stat = "identity", fill = "#0072bc") +
        ggplot2::coord_flip() +
        ggplot2::labs(
            subtitle = paste0("For top ", top_n_countries, " Countries of Origin"),
            x = " ",
            y = " "
        ) +
        unhcrthemes::theme_unhcr(
            grid = "Y",
            axis = "x", axis_title = "",
            font_size = category_font_size
        ) +
        ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.y = ggplot2::element_blank()
        )

    patchworkRSDa <- rsdasyl + rsdorigin
    patchworkRSDa1 <- patchworkRSDa +
        ggplot2::theme(legend.position = "none") +
        patchwork::plot_annotation(
            title = paste0(measurelabel, " | ", year, ", in ", region),
            caption = "Source: UNHCR.org/refugee-statistics "
        )

    return(patchworkRSDa1)
}

# -------------------------------------------------------------------------
# 10. Evolution of Solutions - Refactored
# -------------------------------------------------------------------------

plot_reg_solution <- function(year = 2024,
                              region = "The Americas",
                              lag = 10,
                              label_font_size = 4,
                              category_font_size = 10) {
    solutions_long <- refugees::solutions |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region & year >= (!!year - lag) & year <= !!year) |>
        tidyr::pivot_longer(
            cols = c(naturalisation, resettlement, returned_refugees),
            names_to = "solution_type",
            values_to = "value"
        ) |>
        dplyr::group_by(year, solution_type) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    p <- ggplot2::ggplot(solutions_long, ggplot2::aes(x = year, y = value, colour = solution_type)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 1.1, colour = "#333333") +
        ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        ggplot2::xlim(c(year - lag, year + 2)) +
        ggplot2::scale_colour_manual(
            values = c(
                "naturalisation" = "#a6cee3",
                "resettlement" = "#1f78b4",
                "returned_refugees" = "#b2df8a"
            ),
            labels = c(
                "naturalisation" = "Naturalisation",
                "resettlement" = "Resettlement",
                "returned_refugees" = "Returns"
            )
        ) +
        # ggplot2::geom_label(
        #     data = solutions_long |> dplyr::filter(year == max(year)),
        #     ggplot2::aes(
        #         x = year + .5,
        #         y = value,
        #         label = solution_type,
        #         color = solution_type
        #     ),
        #     hjust = 0,
        #     vjust = 0.5,
        #     fill = "white",
        #     linewidth = NA,
        #     family = "Lato",
        #     size = label_font_size
        # ) +
        unhcrthemes::theme_unhcr(
            grid = "Y",
            axis = "x",
            axis_title = "y",
            font_size = 14
        ) +
        ggplot2::theme(
            legend.position = "top",
            legend.justification = "left",
            legend.title = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = category_font_size)
        ) +
        ggplot2::labs(
            title = paste0("Solution for Displacement in ", region),
            subtitle = paste0("Evolution from ", year - lag, " to ", year),
            x = "",
            y = "",
            caption = "Source: UNHCR.org/refugee-statistics"
        )

    return(p)
}

# -------------------------------------------------------------------------
# 11. Plot Regional Map - Refactored
# -------------------------------------------------------------------------

plot_reg_map <- function(year = 2024,
                         region = "The Americas",
                         topn = 5,
                         pop_type = c("REF", "ASY", "OIP"),
                         projection = "Mercator",
                         maxSymbolsize = .25) {
    # Check if package is installed
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
        stop("Package 'rnaturalearth' is needed for this function to work. Please install it.", call. = FALSE)
    }

    # 1. Prepare Data
    data <- refugees::population |>
        dplyr::filter(year == !!year) |>
        dplyr::mutate(unhcr_region = countrycode::countrycode(coa_iso, "iso3c", "unhcr.region")) |>
        dplyr::filter(unhcr_region == region) |>
        tidyr::pivot_longer(
            cols = c(refugees, asylum_seekers, idps, oip, stateless, ooc, hst),
            names_to = "population_type",
            values_to = "value"
        ) |>
        dplyr::mutate(population_type_label = dplyr::case_when(
            population_type == "refugees" ~ "REF",
            population_type == "asylum_seekers" ~ "ASY",
            population_type == "idps" ~ "IDP",
            population_type == "oip" ~ "OIP",
            population_type == "stateless" ~ "STA",
            population_type == "ooc" ~ "OOC",
            population_type == "hst" ~ "HCO",
            TRUE ~ "Other"
        )) |>
        dplyr::filter(population_type_label %in% pop_type) |>
        dplyr::group_by(coa_iso, coa_name) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    # 2. Get World Map (sf)
    world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
        dplyr::filter(iso_a3 != "ATA") |> # Exclude Antarctica
        dplyr::mutate(unhcr_region = countrycode::countrycode(iso_a3, "iso3c", "unhcr.region")) |>
        sf::st_make_valid()

    # Filter map to region if standard projection limits are needed,
    # but for "logic of plot_ctr_location" which usually shows global or relevant context,
    # we might want to show the whole region + context or just clip to region.
    # plot_reg_map usually shows the specific region.
    region_sf <- world_sf |>
        dplyr::filter(unhcr_region == region)

    # 3. Join Data
    map_data <- region_sf |>
        dplyr::left_join(data, by = c("iso_a3" = "coa_iso"))

    # 4. Plot
    p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
            data = region_sf,
            fill = "#e6e6e6",
            color = "white",
            linewidth = 0.1
        ) +
        ggplot2::geom_sf(
            data = map_data,
            ggplot2::aes(fill = value),
            color = "white",
            linewidth = 0.1
        ) +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::labs(
            title = paste0("Forced Displacement in ", region),
            subtitle = paste0("Number of people (", paste(pop_type, collapse = ", "), ") as of ", year),
            caption = "Source: UNHCR.org/refugee-statistics",
            x = NULL, y = NULL
        ) +
        unhcrthemes::scale_fill_unhcr_c(
            palette = "pal_blue",
            name = NULL,
            labels = scales::label_number(scale_cut = scales::cut_short_scale()),
            na.value = "#e6e6e6"
        ) +
        unhcrthemes::theme_unhcr(
            font_size = 14,
            grid = FALSE,
            axis = FALSE
        ) +
        ggplot2::theme(
            legend.key.width = ggplot2::unit(2, "cm"),
            legend.position = "top",
            legend.justification = "left",
            legend.title.align = 0,
            axis.text = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        )

    return(p)
}

# Example Usages
plot_reg_keyfig(
    year = 2024,
    region = "The Americas",
    population_type_font_size = 4,
    population_size_font_size = 5,
    icon_size = 5
)

plot_reg_treemap(
    year = 2024,
    region = "The Americas",
    label_font_size = 15
)

plot_reg_share(
    year = 2024,
    region = "The Americas",
    label_font_size = 15
)

plot_reg_population_type_per_year(
    year = 2024,
    region = "The Americas",
    label_font_size = 4,
    category_font_size = 10,
    legend_font_size = 10
)

plot_reg_origin_dest(
    year = 2024,
    region = "The Americas"
)

plot_reg_population_type_abs(
    year = 2024,
    region = "The Americas",
    label_font_size = 6,
    category_font_size = 10
)

plot_reg_decrease(
    year = 2024,
    region = "The Americas",
    category_font_size = 10
)

plot_reg_increase(
    year = 2024,
    region = "The Americas",
    category_font_size = 10
)

plot_reg_prop_origin(
    year = 2024,
    region = "The Americas",
    label_font_size = 6,
    category_font_size = 10
)

plot_reg_rsd(
    year = 2024,
    region = "The Americas",
    measure = "Recognized",
    category_font_size = 10
)

plot_reg_solution(
    year = 2024,
    region = "The Americas",
    label_font_size = 4,
    category_font_size = 10
)

plot_reg_map(
    year = 2024,
    region = "The Americas"
)
