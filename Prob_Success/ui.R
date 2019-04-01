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
  sidebarLayout(
    sidebarPanel(
       rHandsontableOutput("hot"),
       br()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("riverplot", h1("Riverplot package"),
                 plotOutput("river")
        ),
        tabPanel("ggalluvial", h1("ggalluvial package"),
                 #verbatimTextOutput("table"),
                 plotOutput("ggalluvial"))
      )          
    )
  )
))
