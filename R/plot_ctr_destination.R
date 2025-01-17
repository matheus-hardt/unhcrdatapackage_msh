# WARNING - Generated by {fusen} from /dev/dev_plot_Country.Rmd: do not edit by hand

#' Main Destination from  one specific countr
#'
#' @param year Numeric value of the year (for instance 2020)
#' @param country_origin_iso3c Character value with the ISO-3 character code of the Country of Origin
#' @param pop_type Vector of character values. Possible population type (e.g.: REF, IDP, ASY, OIP, OOC, STA)
#' 
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual scale_colour_manual 
#'             geom_text theme_void
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme  
#' @importFrom utils  head
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace 
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty
#' @importFrom stats  reorder aggregate 
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join
#' @importFrom tidyr pivot_longer
#' @importFrom unhcrthemes theme_unhcr
#' 
#' @return a ggplot2 object
#' 
#' @export
#'

#' @examples
#' # 
#' plot_ctr_destination(year = 2021,
#'                      country_origin_iso3c = "COL",
#'                      pop_type = c("REF", "ASY")
#'          )
#' 
plot_ctr_destination <- function(year = 2021,
                                 country_origin_iso3c = country_origin_iso3c,
                                 pop_type = pop_type) {

  ctrylabel <- ForcedDisplacementStat::reference |>
    filter(iso_3 == country_origin_iso3c) |>
    select(ctryname) |>
    pull()
  
  Destination <-
    left_join(
      x = ForcedDisplacementStat::end_year_population_totals_long,
      y = ForcedDisplacementStat::reference,
      by = c("CountryAsylumCode" = "iso_3")
    )  |>
    filter(
      CountryOriginCode  == country_origin_iso3c  &
        Year == year &
        Population.type  %in% as.vector(pop_type)
    )  |>
    mutate(
      CountryAsylumName = str_replace(CountryAsylumName, " \\(Bolivarian Republic of\\)", ""),
      CountryAsylumName = str_replace(CountryAsylumName, "Iran \\(Islamic Republic of\\)", "Iran"),
      CountryAsylumName = str_replace(CountryAsylumName, "United States of America", "USA"),
      CountryAsylumName = str_replace(
        CountryAsylumName,
        "United Kingdom of Great Britain and Northern Ireland",
        "UK"
      )
    ) |>
    group_by(CountryAsylumName) |>
    summarise(DisplacedAcrossBorders = sum(Value))  |>
    mutate(DisplacedAcrossBordersRound =  ifelse(
      DisplacedAcrossBorders > 1000,
      paste(
        label_number(accuracy = .1,
                     scale_cut = cut_short_scale())(DisplacedAcrossBorders)
      ),
      as.character(DisplacedAcrossBorders)
    )) |>
    arrange(desc(DisplacedAcrossBorders)) |>
    head(10) |>
    filter(DisplacedAcrossBorders > 0)

  if(nrow(Destination) ==  0) {
    
    info <-  paste0("There\'s no recorded Countries of destination \n in ", ForcedDisplacementStat::reference |>
             dplyr::filter( iso_3 == country_origin_iso3c) |>
             dplyr::pull(ctryname) , " for ", year)
    p <- ggplot() +  annotate(stringr::str_wrap("text", 80), 
                              x = 1, y = 1, size = 11,  
                              label = info ) +  theme_void() 
    
  } else {
    #Make plot
    p <-  ggplot() +
          geom_col(data = Destination, 
                aes(
                x = reorder(CountryAsylumName, DisplacedAcrossBorders),
                ## Reordering country by Value
                y = DisplacedAcrossBorders),
               fill = unhcr_pal(n = 1, "pal_blue")) + # here we configure that it will be bar chart+
        ## Format axis number
       scale_y_continuous(expand = expansion(c(0, 0.1)),
                     labels = label_number(scale_cut = cut_short_scale())) +
        ## Position label differently in the bar in white - outside bar in black
        geom_label(
          data = subset(
            Destination,
            DisplacedAcrossBorders < max(DisplacedAcrossBorders) / 1.5
          ),
        aes(
          x = reorder(CountryAsylumName, DisplacedAcrossBorders),
          y = DisplacedAcrossBorders,
          label = DisplacedAcrossBordersRound
        ),
        hjust = -0.1 ,
        vjust = 0.5,
        colour = "black",
        fill = NA,
        label.size = NA,
        #family = "Lato",
        size = 4
      ) +
      
      geom_label(
        data = subset(
          Destination,
          DisplacedAcrossBorders >= max(DisplacedAcrossBorders) / 1.5
        ),
        aes(
          x = reorder(CountryAsylumName, DisplacedAcrossBorders),
          y = DisplacedAcrossBorders,
          label = DisplacedAcrossBordersRound
        ),
        hjust = 1.1 ,
        vjust = 0.5,
        colour = "white",
        fill = NA,
        label.size = NA,
        # family = "Lato",
        size = 4
      ) +
      # Add `coord_flip()` to make your vertical bars horizontal:
      coord_flip() +
      
      ## and the chart labels
      labs(
        title = paste0("What are the main destinations for Forcibly Displaced People?"),
        
        subtitle = paste0(
          "Top Destination Countries | as of ",
          year,
          " for population from ",
          ctrylabel
        ),
        x = "",
        y = "",
        caption = "Source: UNHCR Population Statistics Database.\n Forced Displacement includes Refugees, Asylum Seekers and Other in Need of International Protection."
      ) +
    theme_unhcr(font_size = 14,
                  grid = FALSE,
                  axis = "y",
                  axis_title = FALSE,
                  axis_text = "y"
                )
    
    
  }
  return(p) # print(p)
}

