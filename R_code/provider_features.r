# provider-provider network analysis
g <- read.graph("../../data/provider_pairs_10.csv", format = "ncol", directed = TRUE)

links <-get.data.frame(g, what="both")
MisLinks <-data.frame(links[2])
colnames(MisLinks) <- c("s","t","v")
nodes = unique(MisLinks[1])


for (i in 1:nrow(nodes)){
  item = nodes[i,1]
  g2 = graph.neighborhood(g, 2, nodes = item)
  g2 <- g2[[1]]
  #print(length(V(g2)))
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
  
  #5. Topological change.
  cutoffs = c(0, 1, 2)
  top_chgs = cutoffs*0
  for (i in 1:(length(cutoffs)-1)) {
    E_cur = length(which(E(g2)$weight>cutoffs[i]))
    E_new = length(which(E(g2)$weight>cutoffs[i+1]))
    
    top_chgs[i] = (E_cur - E_new)/E_cur
  }
  top_chgs[length(top_chgs)] = 1
  
  #6. Clustering coefficient. mean, variance and maximum
  clustering_coff_mean = transitivity(g2, type = "globalundirected")
  clustering_coff_local = transitivity(g2, type = "localundirected")
  clustering_coff_variance = var(clustering_coff_local)
  clustering_coff_max = max(clustering_coff_local)
  
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
  
  features <- c(item, length(V(g2)),den, mean_degree, variance_degree, median_degree, maximum_degree,   
                mean_weights, variance_weights, median_weights, maximum_weights,
                diameter, clustering_coff_mean,clustering_coff_variance,clustering_coff_max                
  )
  result<- rbind(result, features)
}



colnames(result) <- c("ID","size", "density", "mean_degree", "variance_degree", "median_degree", "maximum_degree",   
                      "mean_weights", "variance_weights", "median_weights", "maximum_weights",
                      "diameter", "clustering_coff_mean", "clustering_coff_variance", "clustering_coff_max")

for (i in 1:nrow(nodes)){
  item = nodes[i,1]
  g2 = graph.neighborhood(g, 2, nodes = item)
  g2 <- g2[[1]]
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
  
  #5. Topological change.
  cutoffs = c(0, 1, 2)
  top_chgs = cutoffs*0
  for (i in 1:(length(cutoffs)-1)) {
    E_cur = length(which(E(g2)$weight>cutoffs[i]))
    E_new = length(which(E(g2)$weight>cutoffs[i+1]))
    
    top_chgs[i] = (E_cur - E_new)/E_cur
  }
  top_chgs[length(top_chgs)] = 1
  
  #6. Clustering coefficient. mean, variance and maximum
  clustering_coff_mean = transitivity(g2, type = "globalundirected")
  clustering_coff_local = transitivity(g2, type = "localundirected")
  clustering_coff_variance = var(clustering_coff_local)
  clustering_coff_max = max(clustering_coff_local)
  
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
  
  features <- c(item, length(V(g2)),den, mean_degree, variance_degree, median_degree, maximum_degree,   
                mean_weights, variance_weights, median_weights, maximum_weights,
                diameter, clustering_coff_mean,clustering_coff_variance,clustering_coff_max                
  )
  result<- rbind(result, features)
}



colnames(result) <- c("ID","size", "density", "mean_degree", "variance_degree", "median_degree", "maximum_degree",   
                      "mean_weights", "variance_weights", "median_weights", "maximum_weights",
                      "diameter", "clustering_coff_mean", "clustering_coff_variance", "clustering_coff_max")
