library(riverplot)
library(ggalluvial)
library(RColorBrewer)

df <- read.csv("sample_data.csv", stringsAsFactors = F)
df

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

# styles = lapply(as.numeric(nodes$X), function(n) {
#   list(col = palette[n], lty = 1, textcol = "black", srt=0, edgecol="col")
#   })
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
(coords <- plot(rp1, plot_area = 0.85))
text(
  #x = range(coords["x",]),
  x = seq(min(coords["x",]), max(coords["x", ]), length.out = 4),
  y = max(coords["top",]),
  labels = unique(nodes$variable),
  pos = 1, offset = -1, font = 2
)

# edgesAll <- as.data.frame(t(expand.grid(split(nodes$ID, nodes$variable))), stringsAsFactors = F)
# edgesAll2 <- as.data.frame(lapply(edgesAll, function(x) {
#   if(any(grepl("N$", x[-length(x)]))){
#   x[(min(grep("N$", x))+1):length(x)] <- NA
#   return(x)
#   } else return(x)
#   }), stringsAsFactors = FALSE
# )

ggplot(nodes, aes(x=variable, stratum=X, y = value, label=X, alluvium = ID)) + 
  geom_alluvium(aes(fill=X))

edgesLode <- edges
edgesLode$alluvium2 <- apply(cbind(unlist(sapply(strsplit(edgesLode$N1, " "), "[[", 2)), 
                             unlist(sapply(strsplit(edgesLode$N2, " "), "[[", 2))), 1, paste, collapse="")

edgesLode$alluvium2[grepl("N$", edgesLode$alluvium2)] <- make.unique(edgesLode$alluvium2[grepl("N$", edgesLode$alluvium2)])
#edgesLode$alluvium <- as.numeric(as.factor(edgesLode$alluvium2))                            
#edgesLode$alluvium <- c(1,2,3,4,1,2,5,6,1,2,7,8)
#edgesLode$alluvium <- c(1,2,1,2,1,2,1,2,1,2,1,2)

edgesLode <- data.frame(mapply(c, edgesLode[1:12,c(1,3,4)], 
                               edgesLode[grepl("N$|Phase3", edgesLode$N2),c(2,3,4)], SIMPLIFY = FALSE), 
                        stringsAsFactors = FALSE)
edgesLode$Stratum <- unlist(sapply(strsplit(edgesLode$N1, " "), "[[", 2))
#edgesLode$alluvium <- paste0(edgesLode$Stratum, edgesLode$alluvium2)
#edgesLode$alluvium <- c(1,2,3,4,1,2)

edgesLode$x <- unlist(sapply(strsplit(edgesLode$N1, " "), "[[", 1))
edgesLode$x[edgesLode$x=="Portforlio"] <- "aPortfolio"

#lodes <- split(edgesLode, edgesLode$N1)
edgesLode2 <- edgesLode
#edgesLode2$alluvium2 <- as.numeric(as.factor(edgesLode$alluvium2))
ggplot(edgesLode2, aes(x=x, stratum=Stratum, y = Value, label=Stratum, alluvium = alluvium2)) + 
  geom_flow(aes(fill=Stratum)) + geom_stratum(aes(fill=Stratum)) +
  geom_text(stat = "",check_overlap = T, size = 3)

g <- graph_from_data_frame(edges)
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
p1$x[p1$x=="Portforlio"] <- "aPortfolio"

ggplot(p1, aes(x=x, stratum=Stratum, y = value, label=Stratum, alluvium = alluvial)) + 
  geom_flow(aes(fill=Stratum)) + geom_stratum(aes(fill=Stratum)) 



