# WARNING - Generated by {fusen} from /dev/dev_plot_Country.Rmd: do not edit by hand

#' Population Pyramid
#'
#' @param year Numeric value of the year (for instance 2022). 
#'             If the data is not yet available for that year (aka still in the mid year reporting stage),
#'              it will automatically fall back on the previous year
#' @param country_asylum_iso3c Character value with the ISO-3 character code of the Country of Asylum
#' @param pop_type Vector of character values. Possible population type (e.g.: REF, IDP, ASY, OIP, OOC, STA)
#' 
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual scale_colour_manual 
#'             geom_text .pt theme_void
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme  
#' @importFrom utils  head download.file
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace str_detect
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty
#' @importFrom stats  reorder aggregate  setNames
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom unhcrthemes theme_unhcr 
#' 
#' @return a ggplot2 object
#' 
#' @export
#'

#' @examples
#' # 
#' plot_ctr_pyramid(year = 2022,
#'                  country_asylum_iso3c = "COL",
#'                  pop_type = c("ASY", "REF")
#'                  )
#' 
plot_ctr_pyramid <- function(year ,
                             country_asylum_iso3c = country_asylum_iso3c,
                             pop_type = pop_type) {
  
  
    ## FontAwesome6  includes humanitarian icon... 
    #check_font()
 
  
  ctrylabel <- ForcedDisplacementStat::reference |> 
    filter(iso_3 == country_asylum_iso3c ) |> 
    distinct(ctryname) |> 
    pull()
  
  poptype_label <- ForcedDisplacementStat::end_year_population_totals_long |> 
    filter(Population.type  %in% as.vector(pop_type)) |> 
    distinct(Population.type.label.short) |> 
    pull()
  
  demographics1 <- ForcedDisplacementStat::demographics |>
    left_join( ForcedDisplacementStat::reference |> 
                 select(UNHCRBureau, iso_3),  
               by = c("CountryAsylumCode" = "iso_3")) |> 
    filter(CountryAsylumCode  == country_asylum_iso3c &
             Year == year &
             Population.type  %in% as.vector(pop_type)) |>
    
    mutate ( totGen = FemaleTotal +MaleTotal,
             totbreak = Female04 + Female511 + Female1217 + 
               Female1859 + Female60ormore + FemaleUnknown +
               Male04 + Male511 + Male1217 + Male1859 +
               Male60ormore + MaleUnknown,
             
             hasbreak = ifelse(Total - totGen == 0, "yes", "no" ))
  
  ## Check if fall back is needed.. 
  if(nrow(demographics1) == 0) { 
    year <- year -1 
    demographics1 <- ForcedDisplacementStat::demographics |>
      left_join( ForcedDisplacementStat::reference |> 
                   select(UNHCRBureau, iso_3),  
                 by = c("CountryAsylumCode" = "iso_3")) |> 
      filter(CountryAsylumCode  == country_asylum_iso3c &
               Year == year &
               Population.type  %in% as.vector(pop_type)) |>
      
      mutate ( totGen = FemaleTotal +MaleTotal,
               totbreak = Female04 + Female511 + Female1217 + 
                 Female1859 + Female60ormore + FemaleUnknown +
                 Male04 + Male511 + Male1217 + Male1859 +
                 Male60ormore + MaleUnknown,
               
               hasbreak = ifelse(Total - totGen == 0, "yes", "no" ))
    
    }
  
  
  
  
  if ( nrow(demographics1) ==  0 ){
    info <-  paste0("There\'s no recorded Gender disaggregation \n for Forcibly Displaced People across Borders \n in ", ctrylabel, "as of", year )
    p <- ggplot() +  annotate("text",  x = 1, y = 1, size = 12,  
                              label = info ) +  theme_void() 
    
    
  } else {
    
    tot <- format( sum(demographics1$Total) ,  big.mark=",")
    totprop <-   format( round( sum(demographics1$totGen) / 
                                  sum(demographics1$Total )  *100,1),  big.mark=",")  
    
    if (totprop == 0 ) {
      info <- paste0("There\'s no recorded Gender disaggregation \n for  all of the ",tot, " persons \n in ", ctrylabel  )
      p <- ggplot() +  annotate("text",  x = 1, y = 1, size = 12,  
                                label = info ) +  theme_void() 
      
      
    } else {
      #names(demographics)
      pyramid <-  demographics1[ demographics1$Year == max(demographics1$Year),
                                 c(
                                   "Female04",
                                   "Female511",
                                   "Female1217",
                                   "Female1859",
                                   "Female60ormore",
                                   "FemaleUnknown",
                                   # "FemaleTotal",
                                   "Male04",
                                   "Male511",
                                   "Male1217",
                                   "Male1859",
                                   "Male60ormore",
                                   "MaleUnknown"#,
                                   # "MaleTotal"       
                                 )]  
      
      pyramid2 <- data.frame(lapply(pyramid, function(x) { as.numeric( gsub("NA", "0", x)) })) |>
        pivot_longer(
          cols = Female04:MaleUnknown,
          names_to = "Class",
          values_to = "Sum",
          values_drop_na = TRUE
        ) 
      
      pyramid3 <- as.data.frame(aggregate(pyramid2$Sum,
                                          by = list(pyramid2$Class#, pyramid2$REGION_UN
                                          ),
                                          sum))
      names(pyramid3)[1] <- "Class"
      names(pyramid3)[2] <- "Count"
      
      pyramid3 <- pyramid3 |> 
        mutate(gender = case_when(str_detect(Class, "Male") ~ "Male",
                                  str_detect(Class, "Female") ~ "Female")) |> 
        mutate(age = case_when(str_detect(Class, "04") ~ "0-4",
                               str_detect(Class, "511") ~ "5-11",
                               str_detect(Class, "1217") ~ "12-17",
                               str_detect(Class, "1859") ~ "18-59",
                               str_detect(Class, "60") ~ "60+",
                               str_detect(Class, "Unknown") ~ "Unknown")) 
      
      pyramid3$pc <- pyramid3$Count / sum(pyramid3$Count)
      pyramid3$age <- factor(pyramid3$age, levels = c("0-4", "5-11",  "12-17",  "18-59", "60+", "Unknown"))
      
      pyramid4 <- pyramid3 |> 
        select(gender,age, pc) |> 
        mutate(gender = tolower(gender)) |> 
        pivot_wider(names_from = gender,
                    names_sort = TRUE,
                    values_from = pc
        ) 
      
      p <-  ggplot() +
        geom_col(data = pyramid4,
                 aes(-male, age,
                     fill = "Male"),  width = 0.7 ) +
        geom_col(data = pyramid4,
                 aes(female, age,
                     fill = "Female"),width = 0.7 ) +
        geom_text(data = pyramid4,
                  aes(-male, age,
                      label = percent(abs(male), accuracy = 1)),
                   hjust = 1.25,   size = 11.5 / ggplot2::.pt ) +
        geom_text(data = pyramid4,
                  aes(female,  age,                     
                      label = percent(abs(female), accuracy = 1)  ), 
                  hjust = -0.25,  size = 11.5 / ggplot2::.pt) +
        ## Now get the icon for male and female 
        # ggtext::geom_textbox(aes(x = 0.25, 
        #                           y = "60+",  
        #                 label = "\uf182" ), 
        #                family = "Font Awesome 6 Free Solid",
        #                color = "#18375F",# "#0072BC" "#8EBEFF"
        #                box.colour = NA ,   hjust = 0,   vjust = 0,
        #                width = unit(0.4, "npc"),
        #               size = 11.5 ) +
        # ggtext::geom_textbox(aes(x = as.numeric(-0.25), 
        #                          y = "60+",  
        #                label = "\uf183" ), 
        #               family = "Font Awesome 6 Free Solid",
        #                color = "#0072BC",# "#0072BC" "#8EBEFF"
        #               box.colour = NA ,   hjust = 0,   vjust = 0,
        #                width = unit(0.4, "npc"),
        #              size = 11.5 ) +
        scale_x_continuous(expand = expansion(c(0.2, 0.2))) +
        scale_fill_manual(breaks=c('Male', 'Female'),
                          values = setNames(
                            unhcr_pal(n = 3, "pal_unhcr")[c(2, 1)],
                            c("Male" , "Female")  )) +
        labs(  title = paste0("Population Pyramid for ", 
                              sub(",\\s+([^,]+)$", " and \\1",  toString(poptype_label)), 
                              " | ", ctrylabel),
          subtitle = paste0("As of ", year, 
                            ", gender disaggregation is available for ", totprop, "% of the ",tot,
                            " individuals in ", ctrylabel),
          caption = "Note: figures do not add up to 100 per cent due to rounding\nSource: UNHCR.org/refugee-statistics."  ) +
        theme_unhcr(font_size = 14, grid = FALSE, axis = FALSE, axis_title = FALSE,  axis_text = "y") +
        theme(legend.position = "none")
    }
  }
  
  return(p)
}
