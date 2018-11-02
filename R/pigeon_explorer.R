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
      checkboxInput(inputId = "header",
                    label = "Header",
                    value = TRUE),
      checkboxInput(inputId = "head",
                    label = "Head",
                    value = FALSE),
      tags$hr(),


      selectInput(inputId = "var_quant",
                  label = "How many variables?",
                  choices = 1:3),
      conditionalPanel(
        condition = "input.var_quant == 1",
        selectInput(inputId = "q1_plot",
                    label = "Choose your plot",
                    choices = c("Density", "Histogram", "Rug"))
      ),
      conditionalPanel(
        condition = "input.var_quant == 2",
        selectInput(inputId = "q2_plot",
                    label = "Choose your plot",
                    choices = c("Box", "Scatter", "Bar", "Violin"))
      ),


      uiOutput("xchoice"),
      conditionalPanel(
        condition = "input.var_quant == 2",
        uiOutput("ychoice")
      ),
      radioButtons(inputId = "aes_include",
                   label = "Grouping?",
                   choices = c("Yes", "No"),
                   selected = "No"),
      conditionalPanel(
        condition = "input.aes_include == 'Yes'",
        selectInput(inputId = "aestype",
                    label = "What type of grouping?",
                    choices = c("Color", "Fill", "Size", "Shape", "Linetype", "Facet")),
        uiOutput("aeschoice")
      )

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

    rawdf <- as.data.frame(read.csv(input$file$datapath,
                                    header = input$header))

    if(input$head){
      return(head(rawdf))
    }else{
      return(rawdf)
    }

  })

  output$xchoice <- renderUI({
    req(input$file)

    rawdf <- as.data.frame(read.csv(input$file$datapath,
                                    header = input$header))

    var_names <- names(rawdf)

    selectInput(inputId = "xchoice",
                label = "Choose x variable",
                choices = var_names)
  })

  output$ychoice <- renderUI({
    req(input$file)

    rawdf <- as.data.frame(read.csv(input$file$datapath,
                                    header = input$header))

    var_names <- names(rawdf)

    selectInput(inputId = "ychoice",
                label = "Choose y variable",
                choices = var_names)
  })

  output$aeschoice <- renderUI({
    req(input$file)

    rawdf <- as.data.frame(read.csv(input$file$datapath,
                                    header = input$header))

    var_names <- names(rawdf)

    selectInput(inputId = "aeschoice",
                label = "Choose grouping variable",
                choices = var_names)
  })

  output$plot <- renderPlot({
    req(input$file)

    rawdf <- as.data.frame(read.csv(input$file$datapath,
                      header = input$header))

    if (input$var_quant == 1){
      if (input$q1_plot == "Density"){
        geomgraph <- geom_density()
      } else if (input$q1_plot == "Histogram"){
        geomgraph <- geom_histogram()
      } else if (input$q1_plot == "Rug"){
        geomgraph <- geom_rug()
      }

      if (input$aes_include == "Yes"){
        if (input$aestype == "Color"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], color = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Fill"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], fill = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Size"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], size = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Shape"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], shape = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Linetype"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], linetype = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Facet"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice])) +
            geomgraph +
            facet_grid(row = input$aeschoice)
        }

      } else {
        dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice])) +
          geomgraph
      }


    } else if (input$var_quant == 2){
      if(input$q2_plot == "Box"){
        geomgraph <- geom_boxplot()
      } else if(input$q2_plot == "Scatter"){
        geomgraph <- geom_jitter()
      } else if (input$q2_plot == "Bar"){
        geomgraph <- geom_bar()
      } else if (input$q2_plot == "Violin"){
        geomgraph <- geom_violin()
      }

      if(input$aes_include == "Yes"){
        if (input$aestype == "Color"){
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice], color = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Fill") {
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice], fill = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Size") {
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice], size = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Shape") {
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice], shape = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Linetype") {
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice], linetype = rawdf[,input$aeschoice])) +
            geomgraph
        } else if (input$aestype == "Facet") {
          dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice])) +
            geomgraph +
            facet_grid(row = input$aeschoice)
        }
      } else {
        dfplot <- ggplot(rawdf, aes(x = rawdf[,input$xchoice], y = rawdf[,input$ychoice])) +
          geomgraph
      }
    }

    return(dfplot)

  })

}

shinyApp(ui = ui, server = server)
