library(shiny)
library(riverplot)
library(ggalluvial)
library(htmlwidgets)
library(rhandsontable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sankey demo"),
  
  # Sidebar with a slider input for number of bins 
   fluidRow( 
     column(5, rHandsontableOutput("hot1")), 
     column(6, rHandsontableOutput("hot"))
   ),
  tabsetPanel(
    tabPanel("riverplot", h1("Riverplot package"),
             plotOutput("river", width = "600px")
    ),
    tabPanel("ggalluvial", h1("ggalluvial package"),
             #verbatimTextOutput("table"),
             plotOutput("ggalluvial", width="800px"))
      )          
    )
  )
