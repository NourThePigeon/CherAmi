# library(ggplot2)
# library(tidyverse)
# library(shiny)
# library(devtools)

ui <- fluidPage(

  titlePanel(
    "Welcome to pigeon_explore"
  ),

  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1",
                label = "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput(inputId = "header",
                    label = "Header",
                    value = TRUE),
      selectInput(inputId = "graph_1",
                  label = "Choose a graph",
                  choices = c("density", "histogram", "box", "jitter", "violin"))
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
