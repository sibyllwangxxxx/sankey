#Nate Mockler - Sample GoogleVis Sankey Plot

library(readr)
library(plotly)

sample_data <- read_csv("C:/Users/nmockler/Desktop/sample_Plotly.csv")

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("Effective", "Not Effective", "Ph1B-Pos", "Ph1B-Neg", "Ph1b-Neg", "Ph2-Pos"),
    color = c("blue", "Purple", "Blue", "blue", "purple", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = sample_data$Source,
    target = sample_data$Target,
    value =  sample_data$Value
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )

p
