#Nate Mockler - Sample GoogleVis Sankey Plot

library(readr)
library(googleVis)

sample_data <- read_csv("C:/Users/nmockler/Desktop/Sample_GoogleVis.csv")

Sankey <- gvisSankey(sample_data, from="From", to="To", weight="Weight",
                     options=list(
                       sankey="{link: {color: { fill: '#d799ae' } },
                       node: { color: { fill: '#a61d4c' },
                       label: { color: '#871b47' } }}"))
plot(Sankey)