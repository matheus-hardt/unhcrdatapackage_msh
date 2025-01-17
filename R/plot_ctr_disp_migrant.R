# WARNING - Generated by {fusen} from /dev/dev_plot_Country.Rmd: do not edit by hand

#' Plotting ratio Refugee/Asylum seekers vs Migrant within a country
#' 
#' This chart provides insights on the relative weight of Forced Displacement for countries generating such type of displacement.
#' First, the size of the circle represents the number of Refugees, Asylum Seekers and other in need of International Protection
#' The vertical axis represent the Share of immigrants from the country in relation with all immigrants within the country. 
#' The higher is country in this axis, the more people from this country are the host country independently of the reason why they migrated.   
#' The horizontal axis displays the share of migration generated by forced displacement within each country of origin. 
#' The more a country is right, the more people from that country cam because of Forced Displacement.
#'  
#'
#' @param year Numeric value of the year (for instance 2020)
#' @param country_asylum_iso3c Character value with the ISO-3 character code of the Country of Asylum
#' 
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual scale_colour_manual 
#'             geom_text geom_point scale_size coord_cartesian theme_void
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme  
#' @importFrom utils  head
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace 
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty
#' @importFrom stats  reorder aggregate 
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join across 
#' @importFrom tidyr pivot_longer
#' @importFrom unhcrthemes theme_unhcr
#' @importFrom viridis scale_fill_viridis
#' @import ForcedDisplacementStat 
#' 
#' @return a ggplot2 object
#' 
#' @export
#'
#' @examples
#' plot_ctr_disp_migrant(year = 2022,
#'                     country_asylum_iso3c = "MEX" )
#' # plot_ctr_disp_migrant(year = 2022,
#' #                     country_asylum_iso3c = "FRA" )
plot_ctr_disp_migrant <- function(year = 2021,
                              country_asylum_iso3c ){
  
  ctrylabel <- ForcedDisplacementStat::reference |> 
                 filter(iso_3 == country_asylum_iso3c ) |> 
               select(ctryname) |> 
                pull()
  
  thiscodeM49 <- ForcedDisplacementStat::reference |>
                filter(iso_3 == country_asylum_iso3c) |>
                 select(M49_code) |>
                 pull()
  
  wb_data <- ForcedDisplacementStat::wb_data |>
              filter( iso_3 == country_asylum_iso3c)
  ## World bank API to retrieve total population
  # wb_data <- wbstats::wb( indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD", "NY.GNP.PCAP.CD"),
  #            
  #              country = c( country_asylum_iso3c )  ,         
  #              startdate = 1990, 
  #              enddate = year, 
  #              return_wide = TRUE)
  # 
  # # Renaming variables for further matching
  # names(wb_data)[1] <- "iso_3"
  # names(wb_data)[2] <- "Year"
  
    
    
  #   WDI::WDI(country = c(country_asylum_iso3c ) ,
  #                     indicator=c("SP.POP.TOTL", "NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD", "NY.GNP.PCAP.CD"),
  #                     start = 1990, 
  #                     end = year,
  #                     extra = TRUE)   
  # # Renaming variables for further matching
  # names(wb_data)[3] <- "iso_3"
  # names(wb_data)[4] <- "Year"
  # wb_data$Year <- as.numeric(wb_data$Year)

  ## Getting summary of forcibly displaced   ##############
  displaced <- left_join( x= ForcedDisplacementStat::end_year_population_totals_long, 
                               y= ForcedDisplacementStat::reference, 
                               by = c("CountryAsylumCode" = "iso_3")) |>
              filter(Population.type  %in% c("REF", "ASY", "OIP")) |>
              filter(CountryAsylumCode == country_asylum_iso3c) |>
              filter( Year == max(Year) ) |>
              mutate( iso_3 = CountryOriginCode) |> 
              group_by(Year, iso_3,CountryOriginName, UNHCRBureau, hcr_subregion, INCOME_GRP) |>
              summarise(Asylum_Refugee_in = sum(Value, na.rm = TRUE) )  |>
              filter( iso_3 != "UKN") |>
              arrange( desc(Asylum_Refugee_in)) |>
             # head(10)|>
              as.data.frame()
  
  
  ## Now getting migrant data  ##############
  migrant <-  left_join( x= ForcedDisplacementStat::migrants, 
                               y= ForcedDisplacementStat::reference, 
                               by = c("CountryOriginM49" = "M49_code")) |>
              filter(CountryDestinationM49 == thiscodeM49) |>
              filter( Year == 2020 ) |>
              as.data.frame() |>
              group_by(Year, iso_3,CountryOriginName, UNHCRBureau, hcr_subregion, INCOME_GRP) |>
              summarise(Immigrant = sum(Value) ) |>
              ungroup()#|>
              #mutate( shareOrgin = (Immigrant / sum(Immigrant, na.rm = TRUE))*100 ) |>
              #mutate( Year = as.numeric(Year) ) |>
              #mutate(  )


  ## Now merge everyting   #########
  thismigProfile <-  migrant  |>
    dplyr::full_join( displaced |> select(Asylum_Refugee_in, iso_3), by = c("iso_3")) |>
    dplyr::mutate(  SP.POP.TOTL = as.integer(wb_data |> filter( Year == 2020) |> pull(SP.POP.TOTL) ) ) |>
     #mutate_each(funs(replace(., which(is.na(.)), 0))) |>
        mutate(across( where(is.numeric), ~replace_na(.,0)))|>
    ## Calculate a few ration
    mutate(  totImmigrant =   Immigrant + Asylum_Refugee_in  ,
   # mutate(  totImmigrant =   rowSums(as.integer(Immigrant) , as.integer(Asylum_Refugee_in), na.rm = TRUE )  )
   #    ,
             ratioAsylum_Immigrant = round(Asylum_Refugee_in / totImmigrant, 4),
             shareOrgin =  round(totImmigrant / sum(totImmigrant, na.rm = TRUE),4),

             ratioAsylum_Refugee_in = Asylum_Refugee_in / SP.POP.TOTL,
            ratioImmigrant = Immigrant / SP.POP.TOTL )  |>
    arrange( desc(Asylum_Refugee_in)) |>
   head(10)


if( nrow(displaced) ==  0 ){
     info  <- paste0("There\'s no recorded \n Forcibly Displaced People across Borders \n in ",ctrylabel )
     p <- ggplot() +  annotate("text",  x = 1, y = 1, size = 12,  
                                        label = info ) +  theme_void() 
  
} else {

p <- ggplot(thismigProfile,
       aes(x= ratioAsylum_Immigrant, 
           y= shareOrgin, 
           size= Asylum_Refugee_in/100, 
           #fill= INCOME_GRP,
           label = iso_3)) +
  geom_point(alpha=0.5, 
             shape=21, 
             color="black",
             fill = "#0072BC") +
  scale_size(range = c(.1, 20), guide= "none", ## Do not show Legend
             name="# of Forcibly Displaced People") +
  #viridis::scale_fill_viridis(discrete=TRUE,  name="Country Income Classification") +
  scale_x_continuous(labels = scales::label_percent(accuracy = .1),
                     expand = expansion(mult = c(0.1, .2))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = .1),
                     expand = expansion(mult = c(0.1, .2))) +
  # facet_wrap( vars(Year ), ncol = 2) +
  #coord_cartesian(clip = "off") +
  ggrepel::geom_label_repel(box.padding = 0.5,
                            size = 3,
                            # max.overlaps = 2 
                            fill = "white", 
                            xlim = c(-Inf, Inf), 
                            ylim = c(-Inf, Inf)) + 
  labs(title = stringr::str_wrap( paste0("Share of Forcibly Displaced People within all Migrants in ", ctrylabel  ),90) ,
       subtitle = stringr::str_wrap("This chart provides insights on the relative weight of Forced Displacement for countries generating such type of displacement.
       First, the size of the circle represents the number of Refugees, Asylum Seekers and other in need of International Protection for the main origin of displacement.
       The vertical axis represent the Share of immigrants from each country in relation with all immigrants within the country. 
       The horizontal axis displays the share of migration generated by forced displacement.", 120), 
       y = "Share among all migrants",  #shareOrgin, 
       x ="Ratio Displaced / Immigrant",  # ratioAsylum_Immigrant, 
       caption = stringr::str_wrap("Source: UNHCR.org/refugee-statistics (includes REF +ASY+OIP in calculation) & UNDESA Migrant Stock as of 2020.\n Only main top 10 countries (or less depending on data availibility) linked to Forced Displaced across Borders are presented. Be cautious in the interpretation: data on Migrants are produced every 5 years and may have some gaps.", 120) ) +
  theme_unhcr( font_size = 12 ,
               rel_small = 6/9,
               rel_tiny = 5/9,
               rel_large = 12/7)  + ## Insert UNHCR Style
  theme(legend.position="none", 
       # plot.subtitle = element_text(size = 12),
        panel.grid.major.y  = element_line(color = "#cbcbcb"), 
        panel.grid.major.x  = element_line(color = "#cbcbcb"), 
        panel.grid.minor = element_blank())   ### changing grid line that should appear
  
}
#    The higher is country in this axis, the more people from this country are the host country independently of the reason why they migrated. \n\n  
      #    The more a country is right, the more people from that country cam because of Forced Displacement.
return(p)
    
}
