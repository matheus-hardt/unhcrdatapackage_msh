# WARNING - Generated by {fusen} from /dev/dev_plot_Country.Rmd: do not edit by hand

#' Create a map with symbol proportional to population data by location within country
#'
#' @param year Numeric value of the year  (for instance 2022). 
#'             If the data is not yet available for that year (aka still in the mid year reporting stage),
#'              it will automatically fall back on the previous year
#'              
#' @param country_asylum_iso3c Character value with the ISO-3 character code of the Country of Asylum
#' @param pop_type Vector of character values. Possible population type (e.g.: REF, IDP, ASY, OIP, OOC, STA)
#' @param  mapbackground can be "osm" (default), "stamen-toner" , "stamen-terrain","stamen-watercolor". 
#'            Other mapbackground requires an API key and were not considered 
#' 
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit stat_summary
#'             geom_label geom_text labs  position_stack  geom_point
#'             scale_color_manual scale_colour_manual 
#'             scale_size_continuous element_rect
#'             geom_text .pt theme_void
#'             scale_fill_manual scale_x_continuous scale_x_discrete  scale_y_continuous   sym theme  
#' @importFrom utils  head
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace str_detect  str_glue
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty
#' @importFrom stats  reorder aggregate  setNames
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter summarise ungroup
#'               pull distinct n arrange across slice left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggspatial geom_sf coord_sf
#' @importFrom sf st_as_sf st_set_crs st_bbox
#' @importFrom rnaturalearth ne_countries
#' @import rnaturalearthdata
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices hcl.colors
#' @importFrom OpenStreetMap openmap openproj autoplot.OpenStreetMap
#' @importFrom unhcrthemes theme_unhcr
#' 
#' @return a ggplot2 object
#' 
#' @export
#'
#' @examples
#' plot_ctr_location(year = 2022,
#'                  country_asylum_iso3c = "COL",
#'                  pop_type = c("ASY", "REF", "OIP"))
#' 
#' plot_ctr_location(year = 2021,
#'                  country_asylum_iso3c = "COL",
#'                  pop_type = c("IDP"))
#' 
#' plot_ctr_location(year = 2022,
#'                  country_asylum_iso3c = "CAN",
#'                  pop_type = c("ASY", "REF", "OIP"))
#' 
#' plot_ctr_location(year = 2021,
#'                  country_asylum_iso3c = "MEX",
#'                  pop_type = c("ASY", "REF", "OIP"))
plot_ctr_location <- function( year  ,
                             country_asylum_iso3c  ,
                             pop_type  ,
                             mapbackground = "osm" #  could be "stamen-toner" , "stamen-terrain","stamen-watercolor"
                             ) {
  
  ctrylabel <- ForcedDisplacementStat::reference |> 
    dplyr::filter(iso_3 == country_asylum_iso3c ) |> 
    dplyr::distinct(ctryname) |> 
    dplyr::pull()
  
  poptype_label <- ForcedDisplacementStat::end_year_population_totals_long |> 
    dplyr::filter(Population.type  %in% as.vector(pop_type)) |> 
    dplyr::distinct(Population.type.label.short) |> 
    dplyr::pull()
  
  # names(ForcedDisplacementStat::demographics)
  mapped1 <- ForcedDisplacementStat::demographics |>
    dplyr::filter(Year == year,  #### Parameter
           CountryAsylumCode == country_asylum_iso3c, #### Parameter
           Population.type %in% pop_type  ) |> 
    dplyr::group_by ( CountryAsylumCode, location) |>
    dplyr::summarise( Total = sum(Total, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    ## add geometry
    dplyr::left_join( ForcedDisplacementStat::locpcode ,
               by = c("location" = "location"))
  
  ## Check if fall back is needed.. 
  if(nrow(mapped1) == 0) { 
    year <- year -1 
    mapped1 <- ForcedDisplacementStat::demographics |>
    dplyr::filter(Year == year,  #### Parameter
           CountryAsylumCode == country_asylum_iso3c, #### Parameter
           Population.type %in% pop_type  ) |> 
    dplyr::group_by ( CountryAsylumCode, location) |>
    dplyr::summarise( Total = sum(Total, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    ## add geometry
    dplyr::left_join( ForcedDisplacementStat::locpcode ,
               by = c("location" = "location"))
    
    }
  
  
  nonmapped <- mapped1 |>
    dplyr::filter(   is.na( latitude) & is.na(longitude)) 
  mapped <- mapped1 |>
    dplyr::filter( ! is.na( latitude) | ! is.na(longitude))  |>
    dplyr::mutate (latitude =  as.double( latitude),
                   longitude =  as.double( longitude),
                   level = paste0( "Geographic precision level: ",location_level ),
                   location =factor(location, unique(location)) ) |>
    # Center: reorder your dataset first! Big cities appear later = on top
    dplyr::arrange(desc(Total) )

    pal <- grDevices::hcl.colors(3, "Inferno", rev = TRUE, alpha = 0.7)
    
    
    # Get Bounding box
    # plot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")  |>  dplyr::filter(iso_a3 == country_asylum_iso3c) |> dplyr::pull(geometry))
    # ctr <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    requireNamespace("rnaturalearthdata")
    box <- sf::st_bbox(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")  |>  
                         dplyr::filter(iso_a3 == country_asylum_iso3c))
    LON1 <- as.numeric( box[1] - 0.1* (box[3]-box[1])) 
    LON2 <- as.numeric(box[3] + 0.1* (box[3]-box[1]))
    LAT1 <- as.numeric(box[2]  - 0.1* (box[4]-box[2]) )
    LAT2 <- as.numeric(box[4] + 0.1* (box[4]-box[2]) )
    
    # attempts to assign an object containing “longlat” to data extending beyond
    # longitude [-180, 180] or latitude [-90, 90] will be stopped.
    if(LON1 < -180){ LON1 <- -179}
    if(LON2 > 180){ LON2 <- 179}
    ## Need to trim a lot near the poles to get the webtiles
    if(LAT1 < -80){ LAT1 <- -70} 
    if(LAT2 > 80){ LAT2 <- 70}

    map <- OpenStreetMap::openmap(c(LAT2, LON1), c( LAT1, LON2), 
                                  zoom = NULL,
                                  type =  mapbackground, #"osm" , 
                                  #type =  "stamen-toner" , 
                                  # c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo"),
                                  mergeTiles = TRUE)
    
    
    map.latlon <- OpenStreetMap::openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    # Bubble choropleth map with SOM background
    p <- OpenStreetMap::autoplot.OpenStreetMap(map.latlon) +
         ggplot2::geom_point(  data = mapped ,
            pch = 21,  # , filled circle blue
            ggplot2::aes(x= longitude, y= latitude,
                size = Total,
                fill = level),
               col = "grey20",
              alpha=0.6   ) +
        ggplot2::scale_size_continuous(
          name="Population (in Th)",
          range = c(1, 9),
          breaks=  scales::breaks_pretty(5)) +
       ggplot2::scale_fill_manual(values = pal,
                    drop = FALSE,
                    na.value = "grey80") +
      ggrepel::geom_label_repel(   data = mapped |> head(5)  ,
                mapping = ggplot2::aes(x= longitude, y= latitude,
                          label = stringr::str_glue(  "{stringr::str_wrap(location,15)}\n{ scales::label_number(scale_cut = scales::cut_short_scale())(Total)} ")  # how label displays
                               ),
                          size = 2.5,   # text size in labels
                 min.segment.length = 0)+           # show all line segments  
        ggplot2::coord_sf(xlim = c(LON1 , LON2),   ylim = c(LAT1  ,LAT2 ), expand = FALSE) +
        ggplot2::labs(title = paste0( 
          
                                 format(round(sum(mapped1$Total), 0), scientific = FALSE, big.mark=","), " ",
                                # scales::label_number(scale_cut = scales::cut_short_scale())(sum(mapped1$Total)), 
                                 sub(",\\s+([^,]+)$", " and \\1",  toString(poptype_label)), 
                                " in ", ctrylabel, " as of ", year),
             subtitle = paste0( "This is disaggregated through ",
                                 nrow(mapped),
                                 " locations at different geographic levels, \n representing a total of ",
                                 format(round(sum(mapped$Total), 0), scientific = FALSE, big.mark=","), 
                                # scales::label_number(scale_cut = scales::cut_short_scale())(sum(mapped$Total)), 
                                 " people.\n ",  format(round(nonmapped$Total, 0), scientific = FALSE, big.mark=","), 
                                 " are dispersed in the country (i.e ",
                                 round(nonmapped$Total/sum(mapped1$Total)*100, 1), "%)" ),
             caption = "The boundaries and names shown on this map do not imply \n official endorsement or acceptance by the United Nations",
             size = "Displaced Population") +
        unhcrthemes::theme_unhcr_map() +
        ggplot2::theme(legend.position = "none")
  return(p)
}
