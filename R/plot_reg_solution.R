# WARNING - Generated by {fusen} from /dev/dev_plot_Region.Rmd: do not edit by hand

#' Plot Solution over time in the region
#' 
#' Description
#' @param year Numeric value of the year (for instance 2020)
#' @param region Character value with the related UNHCR bureau - when left null, it will display the whole world
#' @param lag Number of year to used as comparison base
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual scale_colour_manual 
#'             geom_text geom_smooth
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme  
#' @importFrom utils  head
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace 
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty
#' @importFrom stats  reorder aggregate 
#' @importFrom dplyr  if_else desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join 
#' @importFrom tidyr pivot_longer
#' @importFrom unhcrthemes theme_unhcr
#' @importFrom ggrepel geom_label_repel
#' 
#' @return a ggplot2 object
#' 
#' @export
#'  

#' @examples
#' plot_reg_solution(year = 2022, 
#'                             region = "Americas",
#'                             lag = 10)
plot_reg_solution <- function( year = 2022, 
                            region = "Americas",
                            lag = 10){
  
    solution <-  dplyr::left_join( x= ForcedDisplacementStat::solutions_long, 
                                                     y= ForcedDisplacementStat::reference, 
                                                     by = c("CountryOriginCode" = "iso_3")) |> 
                dplyr::filter(Solution.type %in% c("NAT","RST","RET" ) & 
                       UNHCRBureau  == region &
                       Year >= (year - lag) ) |>
                 dplyr::group_by(Year, Solution.type, Solution.type.label )|>
                 dplyr::summarise(Value = sum(Value) ) |>
                 dplyr::ungroup() |>
                 dplyr::group_by( Solution.type  )|>
                 dplyr::mutate(label = dplyr::if_else(Year == max(Year), 
                                               as.character(Solution.type.label),
                                               NA_character_))|>
                 dplyr::ungroup() 

#Make plot
p <- ggplot(solution, aes(x = Year, 
                          y = Value, 
                          colour = Solution.type)) + # Adding reference to color
    geom_smooth(se = FALSE, method = "loess", span = .2) +
    scale_x_continuous( breaks = seq(year - lag, year, by = 5) )+ 
    scale_y_continuous( labels = scales::label_number(accuracy = 1, 
                                                      scale_cut = cut_short_scale()))+
    #scale_colour_viridis_d() + 
     ## Add color for each lines based on color-blind friendly palette
    scale_colour_manual( values = c( "NAT" = "#a6cee3",
                                     "RST"  = "#1f78b4",
                                     "RET" = "#b2df8a" )) +
    ## and the chart labels
    labs(title = paste0("Solution for Displacement in ", region) ,
         subtitle = paste0("Evolution in the past ", lag," years"),
         x = "",
         y = "",
         caption = "Source: UNHCR.org/refugee-statistics ") +   
    ggrepel::geom_label_repel(aes(label = label),
                              nudge_x = 1,
                              na.rm = TRUE) +
    unhcrthemes::theme_unhcr(grid = "Y", 
              axis = "x", 
              axis_title = "y",
              font_size = 14,
              legend = FALSE)  
  

 return(p)  

}

