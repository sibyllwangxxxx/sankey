library(shiny)
library(riverplot)
library(ggalluvial)
library(htmlwidgets)
library(rhandsontable)
library(RColorBrewer)
library(igraph)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   output$hot <- renderRHandsontable(rhandsontable(data.frame(X=c("TP","FP","FN","TN"),
                                                    Portfolio = c(200,800,NA,NA),
                                                    Phase1b = c(180,40,20,760),
                                                    Phase2 = c(162,2,18,38),
                                                    Phase3 = c(145.8,0.05,16.2,1.95), 
                                                    stringsAsFactors = F, row.names = 1:4), useTypes = FALSE))
   
  output$hot1 <- renderRHandsontable(rhandsontable(data.frame(X=c("Effective","Not Effective"),
                                                                  Portfolio=c(200,800),
                                                                  stringsAsFactors = F, row.names = 1:2), useTypes = FALSE))
  edges <- reactive({   
    edges <- expand.grid(N1 = 1:nrow(nodes()), N2 = 1:nrow(nodes()))
    edges <- edges[edges$N1 != edges$N2, ]
    edges <- edges[nodes()$x[edges$N2] - nodes()$x[edges$N1] == 1, ]
    edges <- edges[!nodes()$X[edges$N1] %in% c("FN","TN"), ]
    edges <- edges[(nodes()$y[edges$N2] == nodes()$y[edges$N1]) | 
                   ((nodes()$y[edges$N2] + nodes()$y[edges$N1]) %in% c(9, 6)), ]
    edges$Value <- nodes()$value[edges$N2]
    edges$N1 <- nodes()$ID[edges$N1]
    edges$N2 <- nodes()$ID[edges$N2]
    return(edges)
    })
  
  nodes <- reactive({
    df <- hot_to_r(input$hot)
    nodes <- reshape2::melt(df)
    nodes <- nodes[!is.na(nodes$value), ]
    nodes$ID <- apply(nodes[, 2:1], 1, paste, collapse=" ")
    nodes$x = as.integer(as.factor(nodes$variable))
    nodes$y = as.numeric(as.character(factor(nodes$X, labels = c(1,4,2,8))))
    palette = paste0(brewer.pal(length(unique(nodes$X)), "Set1"))
    names(palette) <- unique(nodes$X)
    nodes$col <- palette[nodes$X]
    
    return(nodes)
  })
  
  output$river <- renderPlot({
   palette = paste0(brewer.pal(length(unique(nodes()$X)), "Set1"))
   names(palette) <- unique(nodes()$X)
   edges1 <- edges()
   edges1$col <- paste0(palette[nodes()$X[match(edges1$N1, nodes()$ID)]], "60")
   edges1$edgecol <- "col"
   
   nodes1 <- nodes()[, c("ID","x","y", "col")]
   nodes1$srt <- 0
   nodes1$label <- nodes()$value
   nodes1$textpos <- 4
   
   rp1 <- makeRiver(nodes1, edges1, node_labels = as.character(nodes()$value) )
   (coords <- plot(rp1, plot_area = 0.85, autoy=F))
   text(
     #x = range(coords["x",]),
     x = seq(min(coords["x",]), max(coords["x", ]), length.out = 4),
     y = max(coords["top",]),
     labels = unique(nodes()$variable),
     pos = 1, offset = -1, font = 2
   )
   
 })
  output$table <- renderPrint({edges()})
  
  output$ggalluvial <- renderPlot({
    g <- graph_from_data_frame(edges())
    p <- unlist(lapply(1:2, 
                       function(x) all_simple_paths(g, x, to =V(g)[grepl("N$|Phase3", V(g)$name)], 
                                                    mode = "out")), recursive = F)
    p1 <- do.call(rbind, lapply(seq_along(p), function(x){
      eid <- get.edge.ids(g, p[[x]][length(p[[x]])+c(-1,0)])
      data.frame(lode = p[[x]]$name, 
                 alluvial=x, 
                 value=E(g)$Value[eid], stringsAsFactors = F )
    })
    )
    
    p1$Stratum <- unlist(sapply(strsplit(p1$lode, " "), "[[", 2))
    p1$x <- unlist(sapply(strsplit(p1$lode, " "), "[[", 1))
    p1$x <- factor(p1$x, levels = c("Portfolio","Phase1b","Phase2","Phase3"))
    p1$Stratum <- factor(p1$Stratum,levels = c("TP","FP","FN","TN"))
    
    ggplot(p1, aes(x=x, stratum=Stratum, y = value, label=Stratum, alluvium = alluvial)) + 
      geom_flow(aes(fill=Stratum)) + 
      geom_stratum(aes(fill=Stratum)) +
      geom_text_repel(aes(label=value), stat = "stratum",box.padding = 0.25, nudge_x = 0.25) +
      # geom_text(data = p1[grepl("Portforlio", p1$lode), ], 
      #           aes(label = rep(c("Effective","Not Effective"), each = 4), 
      #               hjust="right"), stat = "stratum", nudge_y = -50, nudge_x=-0.05) +
      labs(y = "", x="") + 
      theme_minimal()
    
    
    
    
  })
  
})
