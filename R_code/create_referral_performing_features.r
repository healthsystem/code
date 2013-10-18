# create features for referal-performing network
#!/usr/local/bin/Rscript
rm(list=ls())
setwd('/Users/lshi/Documents/Projects/health_fraud/code/R_code')
library('igraph')
  

g <- read.graph("../../data/referal_08_perform.csv", format="ncol", directed = TRUE)

links <-get.data.frame(g, what="both")
MisLinks <-data.frame(links[2])
colnames(MisLinks) <- c("s","t","v")
nodes = unique(MisLinks[1])

out_file_all <- "../../data/result/group_features_referal_performing_08_all_new.csv"

for (i in 1:nrow(nodes)){
  print(i)
  item = nodes[i,1]
  g2 = graph.neighborhood(g, 2, nodes = item)
  g2 <- g2[[1]]
  #   if (length(V(g2)) > 300){
  #     next
  #   }
  
  print(length(V(g2)))
  den = graph.density(g2, loops = T)
  
  #2. Degree Statistics: mean, variance, median, maximum degree
  
  degrees = degree(g2)
  mean_degree = mean(degrees)
  variance_degree = var(degrees)
  median_degree = median(degrees)
  maximum_degree = max(degrees)
  
  #3. Edge weight statistics: mean and variance of weights (currently has some problem with weight.)
  sum_weights = graph.strength(g2, mode = 'all')
  mean_weights = mean(sum_weights)
  variance_weights = var(sum_weights)
  median_weights = median(sum_weights)
  maximum_weights = max(sum_weights)
  
  #4. Diameter
  diameter = diameter(g2)
  
  print ('Diameter')
  #5. Topological change.
  cutoffs = c(0, 1, 2)
  top_chgs = cutoffs*0
  for (i in 1:(length(cutoffs)-1)) {
    E_cur = length(which(E(g2)$weight>cutoffs[i]))
    E_new = length(which(E(g2)$weight>cutoffs[i+1]))
    
    top_chgs[i] = (E_cur - E_new)/E_cur
  }
  top_chgs[length(top_chgs)] = 1
  print ("done with topological change")
  
  #6. Clustering coefficient. mean, variance and maximum
  clustering_coff_mean = transitivity(g2, type = "globalundirected")
  clustering_coff_local = transitivity(g2, type = "localundirected")
  clustering_coff_variance = var(clustering_coff_local)
  clustering_coff_max = max(clustering_coff_local)
  
  print ('done with #6 Clustering coefficient ')
  
  #7. Topological coefficient

  V0 = as.numeric(V(g2))
  for (v0 in V0) {
    V1 = neighbors(g2, v0, 'all')
    kn = length(V1)
    
    J = 0
    cnt = 0
    for (v1 in V1) {
      V2 = neighbors(g2, v1, 'all')
      
      for (v2 in V2) {
        if (v2 != v0) {
          J = J + length(which(neighbors(g2, v2, 'all') %in% c(neighbors(g2, v0, 'all'), v0)))
          cnt = cnt + 1
        }
      }
    }
    V(g2)[v0]$topological_coef = J/cnt/kn
  }
  
  topological_coef_mean = mean(V(g2)$topological_coef)
  topological_coef_variance = var(V(g2)$topological_coef)
  topological_coef_median = median(V(g2)$topological_coef)
  topological_coef_max = max(V(g2)$topological_coef)      
    
  print ('done with #7 Topological coefficient')
  
  features <- c(item, length(V(g2)),den, mean_degree, variance_degree, median_degree, maximum_degree,   
                mean_weights, variance_weights, median_weights, maximum_weights,
                diameter, clustering_coff_mean,clustering_coff_variance,clustering_coff_max                
  )
  #result<- rbind(result, features)
  FF <- as.matrix(t(features))
  write.table(FF, file = out_file_all, sep = ",", 
              col.names = FALSE, append=TRUE)
}

# colnames(result) <- c("ID","size", "density", "mean_degree", "variance_degree", "median_degree", "maximum_degree",   
#                       "mean_weights", "variance_weights", "median_weights", "maximum_weights",
#                       "diameter", "clustering_coff_mean", "clustering_coff_variance", "clustering_coff_max")
# 
# out_file_all <- "../../data/result/group_features_studio.csv"
# write.table(result, out_file_all, col.names = TRUE, row.names = FALSE, sep = ",")
