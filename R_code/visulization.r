rm(list=ls())
setwd('/home/robert/workspace/Projects/health_fraud/code/R_code')

library(digest)
library(devtools)
library(d3Network)
library('igraph', lib.loc = '/home/robert/workspace/Projects/health_fraud/lib')
#source_gist("5734624")

# g <- read.graph("../../data/edge_col.txt", format = "ncol", directed= TRUE)
# g <- read.graph("../../data/patient_pairs_15.csv", format = "ncol", directed = TRUE)
# g <- read.graph("../../data/provider_pairs_10.csv", format = "ncol", directed = TRUE)
g <-read.graph("../../data/san.csv", format = "ncol", directed = TRUE)

#print(g, g=T, v=T, e=T)
#mydata<-read.table("../../data/edge_col.txt")
#mydata

links <-get.data.frame(g, what="both")
MisLinks <-data.frame(links[2])
colnames(MisLinks) <- c("s","t","v")
nodes = unique(MisLinks[1])

############### select the sub graph #####################

d <- subset(MisLinks, MisLinks[3] > 20)
g <- graph.data.frame(d, directed= T, vertices = NULL)
links <-get.data.frame(g, what="both")
MisLinks <-data.frame(links[2])
colnames(MisLinks) <- c("s","t","v")
nodes = unique(MisLinks[1])

# g2 = graph.neighborhood(g, 2, nodes = "118351009")  # node is selected from patient_pairs_10.csv
# g2 = graph.neighborhood(g, 2, nodes = "112823191")  # node is selected from patient_pairs_15.csv
#g2 = graph.neighborhood(g,2, nodes = "4868323576")
#g3 <-g2[[1]]

################### simple layout and display ###################
# set.seed(3952)
layout1 <- layout.fruchterman.reingold(g2)
plot(g2, layout = layout1)
##################################################################


fcs <-walktrap.community(g,modularity = TRUE)
fcs <-leading.eigenvector.community(g)
fcs <-label.propagation.community(g)
Q <- round(modularity(fcs), 3)
g2 <- induced.subgraph(g, communities(fcs)[[3]])
# eb2 <- edge.betweenness.community(g, directed = TRUE)
# eb3 <-cutat(eb2,100)

#eb2 <-walktrap.community(g,modularity = TRUE)

MisNodes <- data.frame(V(g)$name, eb3)
colnames(MisNodes) <- c("name", "group")

links <-get.data.frame(g3, what="both")
MisLinks <-data.frame(links[2])
colnames(MisLinks) <- c("s","t","v")

Source <-match(MisLinks$s, MisNodes$name)
Target <-match(MisLinks$t, MisNodes$name)
MisLinks<-data.frame(cbind(Source-1,Target-1,MisLinks$v))
colnames(MisLinks) <- c("source", "target","value")

###################################testing ############################
library(RCurl)

# Gather raw JSON formatted data
URL <- "https://raw.github.com/christophergandrud/d3Network/master/JSONdata/miserables.json"
MisJson <- getURL(URL, ssl.verifypeer = FALSE)

# Convert JSON arrays into data frames
MisLinks <- JSONtoDF(jsonStr = MisJson, array = "links")
MisNodes <- JSONtoDF(jsonStr = MisJson, array = "nodes")
###################################testing ############################

d3ForceNetwork(Links = MisLinks, Nodes = MisNodes, 
               Source = "source", Target = "target", 
               Value = "value", NodeID = "name", 
               Group = "group", width = 900, height = 900, opacity = 0.9,
               file = "../../data/ClusteringGraph_example.html")

x<-eb
plot(x,g3,
     colbar=rainbow(length(x)),
     col=colbar[membership(x)],
     mark.groups=communities(x),
     edge.color=c("black", "red")[crossing(x,g3)+1])


y <-get.data.frame(g3, what="both")
#source_gist("5734624")
Source<- y$edges$from
Target<- y$edges$to

NetworkData <- data.frame(Source, Target)
d3SimpleNetwork(NetworkData, width = 1000, height = 1000,file= "../../data/test2.html")

# source_gist("5734624")
# Source <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
# Target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
# NetworkData <- data.frame(Source, Target)
# d3SimpleNetwork(NetworkData, width = 1000, height = 1000,file= "../../data/test.html")

