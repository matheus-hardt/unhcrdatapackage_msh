# WARNING - Generated by {fusen} from /dev/dev_plot_Country.Rmd: do not edit by hand

#' Asylum Applications & Decision over time
#' 
#' This charts allow to visualize trends in terms of asylum applications, decision and refugee status recognition
#' 
#' @param year Numeric value of the year (for instance 2020)
#' @param country_asylum_iso3c Character value with the ISO-3 character code of the Country of Asylum
#' @param lag Number of year to used as comparison base
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual scale_colour_manual 
#'             geom_text guide_axis facet_wrap vars
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme  
#' @importFrom utils  head
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace 
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty pretty_breaks
#' @importFrom stats  reorder aggregate 
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join
#' @importFrom tidyr pivot_longer
#' @importFrom unhcrthemes theme_unhcr  scale_fill_unhcr_d
#' 
#' @return a ggplot2 object
#' 
#' @export

#' @examples
#' plot_ctr_asylum(year = 2022,
#'                 country_asylum_iso3c = "ECU", 
#'                             lag = 10)
plot_ctr_asylum <- function(year = 2022,
                            country_asylum_iso3c = country_asylum_iso3, 
                            lag = 10){
    
     ctrylabel <- ForcedDisplacementStat::reference |> 
                 filter(iso_3 == country_asylum_iso3c ) |> 
               select(ctryname) |> 
                pull()
  
    apps <- ForcedDisplacementStat::asylum_applications|>
          filter(CountryAsylumCode == country_asylum_iso3c )|>
          group_by(Year, CountryAsylumCode) |>
         summarize (NumberApplications = sum(NumberApplications , na.rm= TRUE) )

    decs <- ForcedDisplacementStat::asylum_decisions |>
          filter(CountryAsylumCode == country_asylum_iso3c ) |>
          group_by(Year, CountryAsylumCode) |>
         summarize (Recognized = sum(Recognized , na.rm= TRUE),
                  ComplementaryProtection = sum( ComplementaryProtection , na.rm= TRUE),
                  OtherwiseClosed = sum(OtherwiseClosed , na.rm= TRUE),
                  Rejected = sum(Rejected , na.rm= TRUE),
                  TotalDecided = sum(TotalDecided , na.rm= TRUE))

    
 data <- dplyr::inner_join(apps, decs) |>   # names(data)
         dplyr::filter( Year >= (year - lag)) |>
         dplyr::select( Year,  NumberApplications, Recognized, TotalDecided ) |>
         tidyr::pivot_longer( 
              cols = NumberApplications:TotalDecided,
              names_to = "AsylumStage",
              values_to = "Total",
              values_drop_na = TRUE
            )
 # levels(as.factor(data$AsylumStage))
 data$AsylumStage <- dplyr::recode (data$AsylumStage, "NumberApplications"= "Total Number of Applications",
                                            "TotalDecided" = "Total Number of Decisions",
                                             "Recognized" = "Number of Refugee Status Recognition Decisions")
  data$AsylumStage <- factor(data$AsylumStage, levels = c("Total Number of Applications",
                                                          "Total Number of Decisions",
                                                          "Number of Refugee Status Recognition Decisions"))
 
 p <- ggplot() +
     geom_bar(data = data,
             aes(x= Year, y = Total, fill = AsylumStage),
             stat = "identity", position = "dodge", width = 0.8 ) + 
     scale_fill_unhcr_d(palette = "pal_unhcr") +
    # scale_fill_manual(values = c( "#FAAB18", "#0072bc", "#FEEB18")) +  
     scale_x_continuous(breaks = pretty_breaks(10)) +
    # scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
     scale_y_continuous( labels = scales::label_number(accuracy = 1, scale_cut = cut_short_scale()))+ ## Format axis number
     labs(title = paste0( " Asylum Applications & Decisions | ", ctrylabel, " " ,  (year-lag) ," - ", year),
       subtitle = paste0( "Note that under certain circumstance, one person may have more than one applications "), 
       x="",
       y ="",
       caption = "Source: UNHCR.org/refugee-statistics") + 
     theme_unhcr(font_size = 14, ## Insert UNHCR Style
                 grid = "X" #, 
                #  axis = "Y", 
                #  axis_title = TRUE, 
                # axis_text = "Y" 
                )  +
     theme(panel.grid.major.y = element_line(color = "#cbcbcb"), 
           panel.grid.major.x = element_blank()) ### changing grid line that should appear
  
  return(p)

  
  
      
}
