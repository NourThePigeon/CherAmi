library(ggplot2)
library(tidyverse)
library(shiny)
library(devtools)

ui <- fluidPage(

  titlePanel(
    "Welcome to pigeon_explore"
  ),

  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file",
                label = "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),

      selectInput(inputId = "plotchoice",
                  label = "Choose your plot",
                  choices = c("Density", "Histogram", "Rug")),
      selectInput(inputId = "xchoice",
                  label = "Choose your x variable",
                  choices = c("names()")),

      checkboxInput(inputId = "header",
                    label = "Header",
                    value = TRUE),
      checkboxInput(inputId = "head",
                    label = "Head",
                    value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {

  output$table <- renderTable({

    req(input$file)

    rawdf <- read.csv(input$file$datapath,
                       header = input$header)

    if(input$head){
      return(head(rawdf))
    }else{
      return(rawdf)
    }

  })

  output$plot <- renderPlot({
    req(input$file)

    rawdf <- read.csv(input$file$datapath,
                      header = input$header)

    if(input$plotchoice == "Density"){
      geomgraph <- geom_density()
    } else if(input$plotchoice == "Histogram"){
      geomgraph <- geom_histogram()
    } else if (input$plotchoice == "Rug"){
      geomgraph <- geom_rug()
    }


    dfplot <- ggplot(rawdf, aes(x = rawdf[,1])) +
    geomgraph

    return(dfplot)

  })

}

shinyApp(ui = ui, server = server)
