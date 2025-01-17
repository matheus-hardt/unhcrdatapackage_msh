# WARNING - Generated by {fusen} from /dev/dev_plot_Country.Rmd: do not edit by hand

#' Tree map of Population Groups within a country
#'
#' @param year Numeric value of the year (for instance 2020)
#' @param country_asylum_iso3c Character value with the ISO-3 character code of the Country of Asylum
#' @param pop_type Vector of character values. Possible population type (e.g.: REF, IDP, ASY, OIP, OOC, STA)
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line 
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual scale_colour_manual 
#'             geom_text
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
#' plot_ctr_treemap(year = 2021,
#'                             country_asylum_iso3c = "USA",
#'                             pop_type = c("REF", "ASY")
#'          )
#' 
plot_ctr_treemap <- function(year = 2021,
                     country_asylum_iso3c = country_asylum_iso3c,
                     pop_type = pop_type) {


   ctrylabel <- ForcedDisplacementStat::reference |> 
                 filter(iso_3 == country_asylum_iso3c ) |> 
               select(ctryname) |> 
                pull()

  datatree <-  ForcedDisplacementStat::end_year_population_totals_long  |> 
    filter(Year == year,  #### Parameter
                  CountryAsylumCode == country_asylum_iso3c #### Parameter
    ) |> 
    select(-c(Year)) |> 
    group_by(CountryAsylumName,  Population.type, 
                    Population.type.label, Population.type.label.short) |> 
    summarise(across(where(is.numeric), sum)) |> 
    ungroup() 
  
  p <- ggplot() +
         treemapify::geom_treemap( data = datatree, 
                 aes(area = Value, 
                     fill = Population.type) ) +
         treemapify::geom_treemap_text(data = datatree, 
                     aes(area = Value, 
                         fill = Population.type,
                         label = paste0(round(100 * Value / sum(Value),1), 
                          "%\n", 
                          Population.type.label) ),
                     colour = "white",
                           place = "centre", size = 25) +
         scale_fill_manual( values = c(   "IDP" = "#00B398",
                                         # "VDA"="#EF4A60",
                                          "OIP"="#EF4A60",
                                          "ASY" = "#18375F",
                                          "REF" = "#0072BC",
                                          "OOC" ="#8395b9",
                                          "STA"="#E1CC0D")) +
         
      theme_unhcr(font_size = 14,
                  grid = "Y", 
                  axis = "x", 
                  axis_title = "y", 
                  legend = FALSE) +
         theme(legend.position = "none") +
         ## and the chart labels
         labs(title = paste0("Population of Concern in ",  ctrylabel),
              subtitle = paste0("As of ", year, ", a total of ", format(round(sum(datatree$Value), -3),  big.mark=","), " Individuals"),
              x = "",
              y = "",
              caption = "Source: UNHCR.org/refugee-statistics")  

   return(p)
}

