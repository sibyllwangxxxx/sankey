library(shiny)
library(riverplot)
library(ggalluvial)
library(htmlwidgets)
library(rhandsontable)
library(RColorBrewer)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   output$hot <- renderRHandsontable(rhandsontable(data.frame(X=c("TP","FP","FN","TN"),
                                                    Portfolio=c(200,800,NA,NA),
                                                    Phase1b = c(180,40,20,760),
                                                    Phase2 = c(162,2,18,38),
                                                    Phase3 = c(145.8,0.05,16.2,1.95), 
                                                    stringsAsFactors = F, row.names = 1:4), useTypes = FALSE))
  #output$txt <- renderPrint(str(hot_to_r(input$hot)))
  output$river <- renderPlot({
   df <- hot_to_r(input$hot)
   nodes <- reshape2::melt(df)
   nodes <- nodes[!is.na(nodes$value), ]
   nodes$ID <- apply(nodes[, 2:1], 1, paste, collapse=" ")
   nodes$x = as.integer(as.factor(nodes$variable))
   nodes$y = as.numeric(as.character(factor(nodes$X, labels = c(1,4,2,8))))
   edges <- expand.grid(N1 = 1:nrow(nodes), N2 = 1:nrow(nodes))
   edges <- edges[edges$N1 != edges$N2, ]
   edges <- edges[nodes$x[edges$N2] - nodes$x[edges$N1] == 1, ]
   edges <- edges[!nodes$X[edges$N1] %in% c("FN","TN"), ]
   edges <- edges[(nodes$y[edges$N2] == nodes$y[edges$N1]) | 
                    ((nodes$y[edges$N2] + nodes$y[edges$N1]) %in% c(9, 6)), ]
   edges$Value <- nodes$value[edges$N2]
   edges$N1 <- nodes$ID[edges$N1]
   edges$N2 <- nodes$ID[edges$N2]
   
   table(nodes$y, nodes$X)
   
   # rp <- list(nodes= nodes[, c("ID","x","y")], edges = edges)
   # class(rp) <- c(class(rp), "riverplot")
   # plot(rp, plot_area = 0.95, yscale=0.06)
   
   palette = paste0(brewer.pal(length(unique(nodes$X)), "Set1"))
   names(palette) <- unique(nodes$X)
   # styles = lapply(nodes$y, function(n) {
   #   list(col = palette[n], lty = 1, textcol = "black", srt=0, edgecol="col")
   # })
   # names(styles) = nodes$ID
   
   nodes$col <- palette[nodes$X]
   nodes1 <- nodes[, c("ID","x","y", "col")]
   nodes1$y <- nodes$y/2
   nodes1$srt <- 0
   nodes1$label <- nodes$value
   nodes1$textpos <- 4
   edges1 <- edges
   edges1$col <- paste0(palette[nodes$X[match(edges1$N1, nodes$ID)]], "60")
   edges1$edgecol <- "col"
   
   rp1 <- makeRiver(nodes1, edges1, node_labels = as.character(nodes$value) )
   #class(rp1) <- c(class(rp1), "riverplot")
   #plot(rp1, plot_area = 0.95)
   (coords <- plot(rp1, plot_area = 0.85, autoy=F))
   text(
     #x = range(coords["x",]),
     x = seq(min(coords["x",]), max(coords["x", ]), length.out = 4),
     y = max(coords["top",]),
     labels = unique(nodes$variable),
     pos = 1, offset = -1, font = 2
   )
   
 })
  
})
