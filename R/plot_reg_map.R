# WARNING - Generated by {fusen} from /dev/dev_plot_Region.Rmd: do not edit by hand

#' Plot a regional map
#' 
#' 
#' @param year Numeric value of the year (for instance 2020)
#' 
#' @param region Character value with the related UNHCR bureau - when left
#'                null, it will display the whole world
#'                
#' @param topn how many top countries to show..
#' @param pop_type Vector of character values. Possible population type 
#'                (e.g.: REF, IDP, ASY, OIP, OOC, STA)
#'                
#' @param projection use a projection system - default is "Mercator"
#'           for instance this can be Bertin 1953 projection - 
#'          https://visionscarto.net/bertin-projection-1953)
#'          
#' @param maxSymbolsize size in point to adjust for the maximum value
#'  to display on the map
#' 
#' @importFrom ggplot2  ggplot  aes  coord_flip   element_blank element_line
#'             element_text expansion geom_bar geom_col geom_hline unit
#'              stat_summary
#'             geom_label geom_text labs  position_stack  scale_color_manual
#'              scale_colour_manual 
#'             geom_text
#'             scale_fill_manual scale_x_continuous scale_x_discrete  
#'             scale_y_continuous   sym theme  
#' @importFrom utils  head
#' @importFrom sf st_transform st_as_sf st_drop_geometry
#' @importFrom rnaturalearth ne_countries
#' @importFrom tidyselect where
#' @importFrom stringr  str_replace 
#' @importFrom scales cut_short_scale label_percent label_number breaks_pretty
#' @importFrom stats  reorder aggregate 
#' @importFrom dplyr  desc select  case_when lag mutate group_by filter 
#'                summarise ungroup
#'               pull distinct n arrange across slice left_join
#' @importFrom tidyr pivot_longer
#' @importFrom graphics par
#' @importFrom unhcrthemes theme_unhcr
#' @importFrom WDI WDI 
#' @importFrom Hmisc cut2
#' 
#' @return a base plot
#' 
#' @export
#' 
#' @examples
#' plot_reg_map(  year = 2022,
#'                             region = "Asia",
#'                             topn = 5,
#'                             pop_type =  c("REF", "ASY", "OIP"),
#'                             projection = "Mercator",
#'                             maxSymbolsize = .25)
#' 
#' 
#' # plot_reg_map(  year = 2022,
#' #                             region = "WestAfrica",
#' #                             topn = 5,
#' #                             pop_type =  c("REF", "ASY", "OIP"),
#' #                             projection = "Mercator",
#' #                             maxSymbolsize = .25)
#' 
plot_reg_map <- function(    year = 2022, 
                            region = "Americas",
                            topn = 5,
                            pop_type =  c("REF", "ASY", "OIP"),
                            projection = "Mercator",
                            maxSymbolsize = .25   ){
 
      
 
      
   ## World bank API to retrieve total population
  # wb_data <- wbstats::wb( indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD", "NY.GNP.PCAP.CD"),
  #              startdate = 1990, 
  #              enddate = year, 
  #              return_wide = TRUE)
  # 
  # # Renaming variables for further matching
  # names(wb_data)[1] <- "CountryAsylumCode"
  # names(wb_data)[2] <- "Year"
  
  wb_data <- WDI::WDI(country='all', 
                                 ## Population total https://data.worldbank.org/indicator/SP.POP.TOTL
                      indicator=c("SP.POP.TOTL",
                                  ## GDP current
                                  # https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
                                            "NY.GDP.MKTP.CD",
                                  ## GDP per capita 
                                  # https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
                                            "NY.GDP.PCAP.CD", 
                                  ## GNI per capita, Atlas method (current US$)
                                  # https://data.worldbank.org/indicator/NY.GNP.PCAP.CD
                                            "NY.GNP.PCAP.CD" 
                                  ),
                      start = year-1,
                      end = year,
                      extra = TRUE)   
  # Renaming variables for further matching
  names(wb_data)[3] <- "CountryAsylumCode"
  names(wb_data)[4] <- "Year"
      
      
      
      wb_data <- wb_data|>
        filter(Year == year-1) 
      
      ## Get spatial data to add ##########
      mapproject <- ""
      
      listctr <-  ForcedDisplacementStat::reference |>
        filter(UNHCRBureau == region) |>
        select(iso_3) |>
        pull()
      
      
      
      ## Loading the stat tables ######
      data <- dplyr::left_join( x= ForcedDisplacementStat::end_year_population_totals_long, 
                                y= ForcedDisplacementStat::reference, 
                                by = c("CountryAsylumCode" = "iso_3")) |>
        filter(Population.type  %in% pop_type &
                 Year == year & 
                 UNHCRBureau == region ) |>
        group_by(Year, CountryAsylumName, CountryAsylumCode, UNHCRBureau,Latitude, Longitude  ) |>
        summarise(Value = sum(Value) ) |>
        ungroup()
      
      
      ## Join ########
      data2 <- data |> 
        left_join(wb_data, 
                  by = c( "CountryAsylumCode")) |>  
        mutate(ratio_disp_gdp = round( (Value / NY.GDP.MKTP.CD)*100, 4)  )  |> 
        mutate(ratio_disp_gdpcap = round(  (Value/ NY.GDP.PCAP.CD)*100, 2)  )   |> 
        mutate(ratio_disp_host = round( (Value/  SP.POP.TOTL)*100 , 2)  )  
      
      ## Get Break https://riatelab.github.io/mapsf/reference/mf_get_breaks.html
      # Discretize the variable
      data2$quintDisp <- Hmisc::cut2(data2$Value, g = 4)
      data2$quintDispGGP <- Hmisc::cut2(data2$ratio_disp_gdp, g = 4) 
      data2$quintDispHost <- Hmisc::cut2(data2$ratio_disp_host, g = 4)
        
      
      #names(data2)
      data2 <- data2 |>
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      
      
      ## Getting world map for mapping
      world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") |> 
        filter(continent != "Antarctica")  |>  
        filter(adm0_a3 %in%  listctr) 
      
      ### Need to fix specific case for Asisa... where the proj disperse the country...
      ##  filter(adm0_a3 != "VUT") 
      
      ## Manage Projection... 
      data2 <- data2 |>  
        # this is the crs from d, which has no EPSG code:
        sf::st_transform( '+init=epsg:4326')
      
      world <- world |>  
        # this is the crs from d, which has no EPSG code:
        sf::st_transform('+init=epsg:4326')
      #   # this is the crs from d, which has no EPSG code:
      #   #sf::st_transform(., '+init=epsg:4326')
      #   #sf::st_transform(., '+proj=bertin1953 +R=1 0.72 0.73')
      #   #sf::st_transform(., '+proj=bertin1953 +x_0=1000')
      #   sf::st_transform(., '+proj=bertin1953')
      
      
      PopMap <- data2 |> 
                          sf::st_drop_geometry()|> 
                          ungroup() |>
                          arrange( desc (Value)) |> 
                          head(topn) |> 
                          select(Value) 
      
      minPopMap <- PopMap |>   min()
      maxPopMap <- PopMap |>  max()             
      
      
     regionname <- dplyr::case_when( region == "Americas"  ~  "Americas",
                                  region == "Asia"  ~  "Asia & the Pacific",
                                  region == "EastAfrica"  ~  "Eastern Africa",
                                  region =="Europe"  ~  "Europe",
                                  region == "MENA"  ~  "Middle East & North Africa",
                                  region == "SouthAfrica"  ~  "Southern Africa",
                                  region == "WestAfrica"  ~  "Western Africa")          
      
      ## Generate Map   ##################
      #Maps is created here with [MapSF package](https://riatelab.github.io/mapsf/index.html)
      # Select a font already installed on your system !!
      par(family="Lato")
      # set a theme
      mapsfunhcr <- mapsf::mf_theme(bg ="#d4dff2", #  "#E2E7EB",  ## background color --> Used country 
                      # bg = "#cdd2d4", "#faebd7ff",  "#cdd2d4",
                      mar = c(0, 0, 2, 0), ## margins
                      tab = FALSE,  # if TRUE the title is displayed as a 'tab'
                      fg = "#0072BC",  ## foreground color --> for the top title - use UNHCR Blue..
                      pos = "left", # position, one of 'left', 'center', 'right'
                      inner = FALSE, # if TRUE the title is displayed inside the plot area.
                      line = 2, #number of lines used for the title
                      cex = 2.5, #cex of the title
                      #font = "Lato",
                      font = 1 ) #font of the title
      
      
           # map_fun <- function(data, init, theme){
     #      # Initiate a base map
     #      mf_init(x = init, theme = theme)
     #      # always use add = TRUE after mf_init()
     #      mf_shadow(data, col = "grey50", cex = 0.2 , add = TRUE)
     #      mf_map(data, add = TRUE)
     #      mf_title(txt = "Martinique ", fg = "#FFFFFF")
     #      mf_credits(txt = "Credit", bg = "#ffffff80") 
     #      return(invisible(NULL)) 
     #      }
      
      
      ##Initialise the map with a background
      mapsf::mf_init( x = world, theme = mapsfunhcr) 
      # always use add = TRUE after mf_init()
      
      # Plot a shadow
      mapsf::mf_shadow(world, col = "grey50", cex = 0.2 , 
                       add = TRUE)
      
      mapsf::mf_map(world, 
                    lwd = 0.5, 
                    border = "#93A3AB", 
                    col = "#FFFFFF", 
                    add = TRUE)
      
      # Set a layout
      mapsf::mf_title(txt = paste0("Forced Displacement in ",
                                   regionname, " | ", year), fg = "#FFFFFF")
      mapsf::mf_credits(txt = "Source: UNHCR.org/refugee-statistics - A Country is name if it features among the five largest population.\n The boundaries and names shown and the designations used on this map do not imply official endorsment or acceptance by the inited nations", bg = "#ffffff80" ) 
      
      
      ## Proportional Symbol with color based on other
      mapsf::mf_prop_typo( 
        x = data2, # frame to use.. 
        var = c("Value", "quintDispHost"),  ## First value for size, second for color
        inches = maxSymbolsize, # size of the biggest symbol (radius for circles, half width for squares) in inches
        val_max = maxPopMap,  # maximum value used for proportional symbols
        symbol = "circle", ## type of symbol- 'circle' or 'square'
        border = "grey25", ## border color of symbol
        lwd = 1.5, #  border width of symbol
        pal = "Inferno",  ## Color palette  - https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/
        alpha = .8,  ## if pal is a hcl.colors palette name, the alpha-transparency level
        leg_no_data = "No data", ## When no data
        col_na = "grey",  ## When no data
        leg_pos = c("topright", "bottomright"),  # position of the legend
        leg_title = c("Number of Individuals", "Ratio with Host Community"), # title of the legend
        leg_title_cex = c(.7, .7),  # title font size of the legend
        leg_val_cex = c(.5, .5), # content font size of the legend
        leg_val_rnd = .2, # number of decimal places of the values in the legend
        leg_frame = c(FALSE, FALSE), # add frame around the legend
        add = TRUE
      )
    # labels for a few  countries - https://riatelab.github.io/mapsf/reference/mf_label.html
     mapsf::mf_label(x = data2[data2$Value >= minPopMap,], 
                      var = "CountryAsylumName",  # name(s) of the variable(s) to plot
                      cex = 0.9, #  labels cex
                      col = "black", 
                      font = 3.5,
                      halo = TRUE, # add halo
                      bg = "white",  # halo color
                      r = 0.2,  # width of the halo
                      overlap = FALSE,  # if FALSE, labels are moved so they do not overlap.
                      lines = TRUE) 
     
     return(invisible(NULL))
}
