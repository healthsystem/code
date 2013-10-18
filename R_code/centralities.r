rm(list=ls())
setwd('/home/robert/workspace/Projects/health_fraud/code/R_code')
library('igraph', lib.loc = '/home/robert/workspace/Projects/health_fraud/lib')

g <-read.graph("../../data/edge_col.txt", format = "ncol")
# print(g, g=T, v=T, e=T)
# plot(g)
#1. Graph density

den = graph.density(g, loops = T)

# different centralities

V(g)$page.rank <- page.rank(g,V(g),directed = TRUE)$vector
V(g)$out.degree<-degree(g,V(g),mode = c("out"),loops = TRUE)
V(g)$in.degree<-degree(g,V(g),mode = c("in"),loops = TRUE)
V(g)$total.degree<-degree(g,V(g),mode = c("total"),loops = TRUE)

V(g)$out.closeness<-closeness(g, v=V(g), mode = c("out"))
V(g)$in.closeness<-closeness(g, v=V(g), mode = c("in"))
V(g)$all.closeness<-closeness(g, v=V(g), mode = c("all"))

V(g)$betweenness<-betweenness(g, v=V(g), directed = TRUE)
V(g)$eigenvector.centrality<-evcent(g, V(g),scale = TRUE, weights = NULL)$vector


scores <- data.frame(name=V(g)$name, page.rank=V(g)$page.rank,out.degree=V(g)$out.degree,
                     in.degree=V(g)$in.degree, total.degree=V(g)$total.degree,
                     eigenvector.centrality=V(g)$eigenvector.centrality)

output<- data.frame(scores[order(-scores[,"page.rank"]),c("name", "page.rank")],
                      scores[order(-scores[,"out.degree"]),c("name","out.degree")],
                      scores[order(-scores[,"in.degree"]),c("name","in.degree")],
                      scores[order(-scores[,"total.degree"]),c("name","total.degree")],
                      scores[order(-scores[,"eigenvector.centrality"]),c("name","eigenvector.centrality")]
                      )
out_file <- "../../data/result/prac_centralities.csv"
write.table(output,out_file, col.names = TRUE, row.names = FALSE, sep = ",")