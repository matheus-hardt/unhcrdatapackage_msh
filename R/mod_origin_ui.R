# WARNING - Generated by {fusen} from /dev/dev_golem_module.Rmd: do not edit by hand

#' origin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @keywords internal
mod_origin_ui <- function(id){
  ns <- NS(id)
  tagList(
   
   tabsetPanel(type = "tabs",
         tabPanel(title= "Main Country of Origin",
                  mod_plotviz_ui(ns("origin1"),
                  thisPlot = "plot_ctr_population_type_abs" ) ),
         
         tabPanel(title= "Main Country of Origin as %",
                  mod_plotviz_ui(ns("origin2"),
                  thisPlot = "plot_ctr_population_type_perc" ) ),
         
         tabPanel(title= "Increases and Decreases ",
                  mod_plotviz_ui(ns("origin3"),
                  thisPlot = "plot_ctr_diff_in_pop_groups" ) ),
         
         tabPanel(title= "Origin History",
                  mod_plotviz_ui(ns("origin4"), 
                  thisPlot = "plot_ctr_origin_history" ) ) 
      ) ## End Tabset 
  )
}
    
#' origin Server Functions
#'
#' @param reactiveParameters  Main app filters defined through mod_input
#' @noRd 
#' @import ggplot2
#' @import shiny
#' @keywords internal
mod_origin_server <- function(id, reactiveParameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_plotviz_server("origin1", 
                       thisPlot = "plot_ctr_population_type_abs", 
                       reactiveParameters )
    mod_plotviz_server("origin2", 
                       thisPlot = "plot_ctr_population_type_perc", 
                       reactiveParameters )
    mod_plotviz_server("origin3", 
                       thisPlot = "plot_ctr_diff_in_pop_groups", 
                       reactiveParameters )
    mod_plotviz_server("origin4",
                       thisPlot = "plot_ctr_origin_history", 
                       reactiveParameters )
 
  })
}
    
## To be copied in the UI
# mod_origin_ui("origin_1")
    
## To be copied in the server
# mod_origin_server("origin_1")

