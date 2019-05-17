
# Library
library(networkD3)
library(tidyverse)


# CONNECTION DATA FRAME - Usually what you have is a connection data frame: a list of flows with intensity for each flow
links=data.frame(source=c("Effective","Effective", "NotEffective","NotEffective", "Ph1b+Y", "Ph1b+Y","Ph1b+N", "Ph1b+N",  "Ph2+Y", "Ph2+Y",  "Ph2+N", "Ph2+N"), target=c("Ph1b+Y","Ph1b-Y", "Ph1b+N","Ph1b-N", "Ph2+Y","Ph2-Y","Ph2+N","Ph2-N", "Ph3+Y","Ph3-Y", "Ph3+N","Ph3-N"), value=c(180,20, 40, 760, 162, 18,2,38,146,16,1,1 ))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1


# Add a 'group' column to each connection:
links$group=as.factor(c("type_1","type_2","type_3","type_4","type_5","type_6", "type_7","type_8","type_9","type_10","type_11","type_12" ))

# Add a 'group' column to each node. 
nodes$group=as.factor(c("Effective","NotEffective","Ph1b+Y","Ph1b+N","Ph2+Y","Ph2+N","Ph1b-Y","Ph1b-N","Ph2-Y","Ph2-N","Ph3+Y","Ph3-Y","Ph3+N","Ph3-N"))

# Give a color for each group and flow
my_color <- 'd3.scaleOrdinal() .domain(["type_1","type_2","type_3","type_4","type_5","type_6", "type_7","type_8","type_9","type_10","type_11","type_12", "Effective","NotEffective", "Ph1b+Y", "Ph1b-Y", "Ph1b+N", "Ph1b-N", "Ph2+Y", "Ph2-Y", "Ph2+N", "Ph2-N","Ph3+Y", "Ph3-Y", "Ph3+N", "Ph3-N"]) 
.range(["#ADD8E6", "#ADD8E6", "#D8BFD8", "#D8BFD8", "#D8BFD8", "#D8BFD8", "#D8BFD8", "#D8BFD8","orange", "yellow","orange", "yellow",
"teal", "violet" ,"blue" ,"violet", "pink", "brown","orange" , "green" , "green","teal", "teal" ,"purple" ,"purple", "teal" ])'

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", fontSize=15,
              colourScale=my_color, LinkGroup="group", NodeGroup="group", sinksRight=FALSE)




