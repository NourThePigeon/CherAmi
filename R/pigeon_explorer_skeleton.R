library(ggplot2)
library(tidyverse)
library(shiny)
library(devtools)

### UI ####
ui <- fluidPage(

  titlePanel(
    "Welcome to pigeon_explore"
  ),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data"),
        tabPanel("Layers",
                 sliderInput("nTabs", 'No of Layers', 1,5,2),
                 uiOutput('layertabs')),
        tabPanel("Theme")
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data"),
        tabPanel("Plot"),
        tabPanel("Code")
      )
    )
  )

)

### Server ####
server <- function(input, output) {
  output$layertabs = renderUI({
    nTabs = input$nTabs
    myTabs = lapply(paste0('Layer', 1: nTabs), tabPanel)
    do.call(tabsetPanel, myTabs)
  })

  output$layeroptions = renderUI({
    textInput("bleh","blah")
  })

}

### Shiny App####
shinyApp(ui = ui, server = server)
