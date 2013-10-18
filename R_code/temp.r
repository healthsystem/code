# temp
rm(list=ls())
setwd('/home/robert/workspace/Projects/health_fraud/code/R_code')
library('igraph', lib.loc = '/home/robert/workspace/Projects/health_fraud/lib')
library(d3Network)
g <- barabasi.game(100, power = 0.6, m = 10)
V(g)$name <- seq(vcount(g))
# g <- simplify(g)
l <- layout.fruchterman.reingold(g)
l <-layout.norm(l, -1,1,-1,1)

# fcs <- fastgreedy.community(g)
# Q <- round(modularity(fcs), 3)


### different community detection methods #####################
leading.eigenvector.community(g)
label.propagation.community(g)
fcs <-walktrap.community(g,modularity = TRUE)
sping <-spinglass.community(g,spins = 10)
Q <- round(modularity(fcs), 3)

plot(g, layout=l, vertex.size=3, vertex.label=NA, vertex.color="#ff000033",
     vertex.frame.color="#ff000033", edge.color="#55555533", edge.arrow.size=0.3,
     rescale=FALSE, xlim=range(l[,1]), ylim=range(l[,2]),
     main=paste(sep="", "walk trap community detection,\nQ=", Q))

g2 <- induced.subgraph(g, communities(fcs)[[1]])
l2 <- l[ communities(fcs)[[1]], ]

plot(g2, layout=l2, vertex.size=3, vertex.label=V(g2)$name,
     vertex.color="#ff0000", vertex.frame.color="#ff0000", edge.color="#555555",
     vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.font=2,
     edge.arrow.size=0.3, add=TRUE, rescale=FALSE)

nodes <- communities(fcs)[[1]]
nodes <- paste(collapse=", ", nodes)
text(0,-1.3, nodes, col="blue")
