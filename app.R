# First attempt at a Shiny App

library(shiny)
library(shinythemes)
library(shinyjs)

# Define User Interface

User_Interface <- fluidPage(theme = shinytheme("cerulean"),
                    useShinyjs(),
                    navbarPage("HBC Training",
                      tabPanel("DGE",
                        tags$h1("Differential Gene Expression R Package Check"),
                        sidebarPanel( 
                          width = 6,
                          div(
                          id = "side-panel",
                          h3("Paste your sessionInfo() below"),                        
                          textAreaInput("session_info", "", height = 250)),
                          actionButton("run", "Check my sessionInfo()"), 
                          actionButton("reset", "Clear Input")
                        ),
                        mainPanel(
                          width = 6,
                          h3("Missing Packages"),
                          textOutput("txtout"),
                          )
                        )
                      )
                    )

Server <- function(input, output){
  session_info_check <- eventReactive(
    eventExpr = input$run, 
    valueExpr = {input$session_info}
  )
  observeEvent(input$reset, {
    reset("side-panel")})
  failed_packages <- c()
  output$txtout <- renderText({
    deseq2_logical <- any(grepl("DESeq2", session_info_check()))
    tidyverse_logical <- any(grepl("tidyverse", session_info_check()))
    rcolorbrewer_logical <- any(grepl("RColorBrewer", session_info_check()))
    pheatmap_logical <- any(grepl("pheatmap", session_info_check()))
    ggrepel_logical <- any(grepl("ggrepel", session_info_check()))
    cowplot_logical <- any(grepl("cowplot", session_info_check()))
    clusterprofiler_logical <- any(grepl("clusterProfiler", session_info_check()))
    degreport_logical <- any(grepl("DEGreport", session_info_check()))
    orgHsegdb_logical <- any(grepl("org.Hs.eg.db", session_info_check()))
    dose_logical <- any(grepl("DOSE", session_info_check()))
    pathview_logical <- any(grepl("pathview", session_info_check()))
    tximport_logical <- any(grepl("tximport", session_info_check()))
    annotationhub_logical <- any(grepl("AnnotationHub", session_info_check()))
    ensembldb_logical <- any(grepl("ensembldb", session_info_check()))
    if ( all(deseq2_logical, tidyverse_logical, rcolorbrewer_logical, pheatmap_logical, ggrepel_logical, cowplot_logical, clusterprofiler_logical, degreport_logical, orgHsegdb_logical, dose_logical, pathview_logical, tximport_logical, annotationhub_logical, ensembldb_logical)){
      print("You are not missing any packages and you are ready for the course! Please take a screenshot or picture of this message and e-mail it to hbctraining@hsph.harvard.edu.")
    } 
    else {
      if ( deseq2_logical == FALSE){
        failed_packages <- c(failed_packages, "DESeq2")
      }
      if ( tidyverse_logical == FALSE){
        failed_packages <- c(failed_packages, "tidyverse")
      }
      if ( rcolorbrewer_logical == FALSE){
        failed_packages <- c(failed_packages, "RColorBrewer")
      }
      if ( pheatmap_logical == FALSE){
        failed_packages <- c(failed_packages, "pheatmap")
      }
      if ( ggrepel_logical == FALSE){
        failed_packages <- c(failed_packages, "ggrepel")
      }
      if ( cowplot_logical == FALSE){
        failed_packages <- c(failed_packages, "cowplot")
      }
      if ( clusterprofiler_logical == FALSE){
        failed_packages <- c(failed_packages, "clusterProfiler")
      }
      if ( degreport_logical == FALSE){
        failed_packages <- c(failed_packages, "DEGreport")
      }
      if ( orgHsegdb_logical == FALSE){
        failed_packages <- c(failed_packages, "org.Hs.eg.db")
      }
      if ( dose_logical == FALSE){
        failed_packages <- c(failed_packages, "DOSE")
      }
      if ( pathview_logical == FALSE){
        failed_packages <- c(failed_packages, "pathview")
      }
      if ( tximport_logical == FALSE){
        failed_packages <- c(failed_packages, "tximport")
      }
      if ( annotationhub_logical == FALSE){
        failed_packages <- c(failed_packages, "AnnotationHub")
      }
      if ( ensembldb_logical == FALSE){
        failed_packages <- c(failed_packages, "ensembldb")
      }
      failed_packages
    }
  })
}


shinyApp(ui = User_Interface, server = Server)
