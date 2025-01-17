# WARNING - Generated by {fusen} from /dev/dev_golem_module.Rmd: do not edit by hand

#' solution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @keywords internal
mod_solution_ui <- function(id){
  ns <- NS(id)
  tagList(
   tabsetPanel(type = "tabs",
         tabPanel(title= "Solutions Trend",
                  mod_plotviz_ui(ns("solution1"),
                  thisPlot = "plot_ctr_solution" ) ) 
      ) ## End Tabset
  )
}
    
#' solution Server Functions
#' @param reactiveParameters  Main app filters defined through mod_input
#'
#' @noRd 
#' @import ggplot2
#' @import shiny
#' @keywords internal
mod_solution_server <- function(id, reactiveParameters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   mod_plotviz_server("solution1", 
                      thisPlot = "plot_ctr_solution", 
                      reactiveParameters ) 
 
  })
}
    
## To be copied in the UI
# mod_solution_ui("solution_1")
    
## To be copied in the server
# mod_solution_server("solution_1")

