# WARNING - Generated by {fusen} from /dev/dev_plot_Region.Rmd: do not edit by hand

#' Plot Chart on Refugee Status Determination
#' 
#' Show the main host and origin countries based on number of decisions
#' 
#' @param year Numeric value of the year (for instance 2020)
#' @param region Character value with the related UNHCR bureau - when left null, it will display the whole world
#' @param top_n_countries Numeric value of number of main countries that the graph should display
#' @param measure this can be either:
#'            * Recognized  
#'            * ComplementaryProtection  
#'            * TotalDecided 
#'            * RefugeeRecognitionRate 
#'            * TotalRecognitionRate
#' 
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual
#'              scale_colour_manual 
#'             geom_text geom_line
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme 
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join
#' @importFrom stringr str_replace  
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty pretty_breaks
#' @importFrom patchwork plot_annotation
#' @importFrom unhcrthemes theme_unhcr unhcr_pal
#' 
#' 
#' @return a ggplot2 object
#' 
#' @export
#' @examples
#' plot_reg_rsd(year = 2022,
#'              region = "Americas" ,
#'                         top_n_countries = 10, 
#'                         measure = "Recognized")
#' 
#' 
#' plot_reg_rsd(year = 2022,
#'              region = "Americas" ,
#'                         top_n_countries = 5, 
#'                         measure = "ComplementaryProtection")
#' 
#' 
#' plot_reg_rsd(year = 2022,
#'              region = "Americas" ,
#'                         top_n_countries = 10, 
#'                         measure = "TotalDecided")
#' 
#' 
#' plot_reg_rsd(year = 2022,
#'              region = "Americas" ,
#'                         top_n_countries = 10, 
#'                         measure = "RefugeeRecognitionRate")
#' 
#' 
#' plot_reg_rsd(year = 2022,
#'              region = "Americas" ,
#'                         top_n_countries = 10, 
#'                         measure = "TotalRecognitionRate")
#' 
#' 
#' # plot_reg_rsd(year = 2022,
#' #              region = "Europe", 
#' #                         top_n_countries = 10, 
#' #                         measure = "Recognized")
plot_reg_rsd <- function(year = 2022,
                        region,
                        top_n_countries = 10, 
                        measure = "Recognized"){
  
  measurelabel <- dplyr::case_when( measure == "Recognized"  ~ "Recognized Refugee Status Decisions", 
                       measure == "ComplementaryProtection"  ~ "Complementary Protection Decisions",
                       measure == "TotalDecided"  ~ "Total Decision (independently of the outcome)", 
                       measure == "RefugeeRecognitionRate"  ~ "Refugee Recognition Rate", 
                       measure == "TotalRecognitionRate"  ~ "Total Recognition Rate")

topasyl <-  ForcedDisplacementStat::asylum_decisions |>
  ## Add reference for the filters
  dplyr::left_join( ForcedDisplacementStat::reference |> 
                      select(coa_region = `UNHCRBureau`, iso_3),
                    by = c("CountryAsylumCode" = "iso_3")) |> 
  filter(coa_region == region & Year == year) |> 
  
  ## the below is change - DecisionsAveragePersonsPerCase- is just indicative... so no need to use it to m
 # mutate(DecisionsAveragePersonsPerCase = map_dbl(DecisionsAveragePersonsPerCase, ~replace_na(max(as.numeric(.), 1), 1))) |>
  mutate(DecisionsAveragePersonsPerCase = 1 ) |>
  group_by(CountryAsylumName) |> 
  summarize(Recognized = sum(Recognized * DecisionsAveragePersonsPerCase, na.rm = TRUE),
            ComplementaryProtection = sum(ComplementaryProtection * DecisionsAveragePersonsPerCase, na.rm = TRUE),
            TotalDecided = sum(TotalDecided * DecisionsAveragePersonsPerCase, na.rm = TRUE)) |>
  mutate(RefugeeRecognitionRate = (Recognized ) / TotalDecided,
         TotalRecognitionRate = (Recognized + ComplementaryProtection) / TotalDecided ) |>
  # filter(TotalDecided  != 0) |>
  # filter(TotalDecided  > 1000)  |>
  mutate(CountryAsylumName = str_replace(CountryAsylumName, "United States of America", "USA"))
  
topasyl1 <-  topasyl  |>
  mutate( measured = .data[[measure]]) |>
  arrange(desc(measured)) |>
  head(top_n_countries)  
  
 
topOrigin <-  ForcedDisplacementStat::asylum_decisions |>
  ## Add reference for the filters
  dplyr::left_join( ForcedDisplacementStat::reference |> 
                      select(coa_region = `UNHCRBureau`, iso_3),  by = c("CountryOriginCode" = "iso_3")) |> 
  filter(coa_region == region & Year == year) |> 
  ## the below is change - DecisionsAveragePersonsPerCase- is just indicative... so no need to use it to m
 # mutate(DecisionsAveragePersonsPerCase = map_dbl(DecisionsAveragePersonsPerCase, ~replace_na(max(as.numeric(.), 1), 1))) |>
  mutate(DecisionsAveragePersonsPerCase = 1 ) |>
  group_by(CountryOriginName) |> 
  summarize(Recognized = sum(Recognized * DecisionsAveragePersonsPerCase, na.rm = TRUE),
            ComplementaryProtection = sum(ComplementaryProtection * DecisionsAveragePersonsPerCase, na.rm = TRUE),
            TotalDecided = sum(TotalDecided * DecisionsAveragePersonsPerCase, na.rm = TRUE)) |>
  mutate(RefugeeRecognitionRate = (Recognized ) / TotalDecided,
         TotalRecognitionRate = (Recognized + ComplementaryProtection) / TotalDecided) |>
  # filter(TotalDecided  != 0) |>
  # filter(TotalDecided  > 1000)  |>
  mutate(CountryOriginName = str_replace(CountryOriginName, " \\(Bolivarian Republic of\\)", ""))

topOrigin1 <-  topOrigin  |>
  mutate( measured = .data[[measure]])  |>
  arrange(desc(measured)) |>
  head(top_n_countries)   
 

rsdasyl <-  ggplot(topasyl1, aes(y = measured, 
             x = reorder(CountryAsylumName, measured))) + 
  
  
  
  scale_y_continuous( labels =   scales::label_percent(accuracy = 0, suffix = "%") ) + 
  
  
  scale_y_continuous( labels =    ifelse( measure %in% c("RefugeeRecognitionRate", "TotalRecognitionRate"), 
                        scales::label_percent(accuracy = 0, suffix = "%"),
                        scales::label_number(accuracy = 1,   scale_cut = cut_short_scale()) )
                      ) + ## Format axis number
  #facet_grid(.~ ctry_asy) +  
  geom_bar( stat ="identity", fill = "#0072bc") +
  coord_flip() +
 # geom_hline(yintercept = 0, linewidth = 1.1, colour = "#333333") +
  labs(#title = "Number of RSD application  in 2020",
       subtitle = paste0( "For top ", top_n_countries, " Countries of Asylum"),
       x = " ", 
       y = " "  ) +
  theme_unhcr( grid = "Y",
               axis = "x",  axis_title = "" ,
              font_size = 10) +
  theme(#axis.text.x = element_blank(),
    # legend.position = "none",
    
    panel.grid.major.x = element_line(color = "#cbcbcb"), 
    panel.grid.major.y = element_blank()) ### changing grid line that should appear) 
  
 
rsdorigin <- ggplot(topOrigin1, aes(y = measured, 
             x = reorder(CountryOriginName, measured))) +
  
  
  scale_y_continuous( labels =   ifelse( measure %in% c("RefugeeRecognitionRate", "TotalRecognitionRate"), 
                        scales::label_percent(accuracy = 0, suffix = "%"),
                        scales::label_number(accuracy = 1,   scale_cut = cut_short_scale()) )
                      ) + ## Format axis number
  
 # scale_y_continuous( labels = scales::label_number(accuracy = 1,   scale_cut = cut_short_scale())) + ## Format axis number
  #facet_grid(.~ ctry_asy) +  
  geom_bar( stat ="identity", fill = "#0072bc") +
  coord_flip() +
#  geom_hline(yintercept = 0, size = 1.1, colour = "#333333")   +
  labs(#title = "Number of RSD application per country of Origin in 2020",
       subtitle = paste0( "For top ", top_n_countries, " Countries of Origin"),
       x = " ", 
       y = " " ) +
  theme_unhcr( grid = "Y",
               axis = "x",  axis_title = "" ,
              font_size = 10 ) + 
  theme(#axis.text.x = element_blank(),
    # legend.position = "none",
    
    panel.grid.major.x = element_line(color = "#cbcbcb"), 
    panel.grid.major.y = element_blank()) ### changing grid line that should appear) 

 
#joining charts 
requireNamespace("patchwork")
patchworkRSDa <- rsdasyl + rsdorigin
patchworkRSDa1 <- patchworkRSDa +
  #unhcRstyle::unhcr_theme(base_size = 8)  +  ## Insert UNHCR Style
  theme(legend.position = "none") +
  patchwork::plot_annotation(
    title = paste0(measurelabel, " | ", year, ", in ",region),
   # subtitle = ' ',
    caption = 'Source: UNHCR.org/refugee-statistics '
   ## \n Because of different types of procedure, data from the US may \n includes multiple applications per applicants compared to other countries
  )  

  return(patchworkRSDa1)
}
