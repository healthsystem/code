rm(list=ls())
setwd('/home/robert/workspace/Projects/health_fraud/code/R_code')
library('igraph', lib.loc = '/home/robert/workspace/Projects/health_fraud/lib')

g <-read.graph("../../data/edge_col.txt", format = "ncol", directed= T)
#neighborhood.size# print(g, g=T, v=T, e=T)


g2 = graph.neighborhood(g, 2, nodes = "A50373")
g = g2[[1]]
# print(g2, g2=T, v=T, e=T)
# plot(g2)
#1. Graph density
# g = g2
den = graph.density(g, loops = T)

#2. Degree Statistics: mean, variance, median, maximum degree

degrees = degree(g)
mean_degree = mean(degrees)
variance_degree = var(degrees)
median_degree = median(degrees)
maximum_degree = max(degrees)

#3. Edge weight statistics: mean and variance of weights (currently has some problem with weight.)
sum_weights = graph.strength(g, mode = 'all')
mean_weights = mean(sum_weights)
variance_weights = var(sum_weights)
median_weights = median(sum_weights)
maximum_weights = max(sum_weights)


#4. Diameter
diameter = diameter(g)


#5. Topological change.
cutoffs = c(0, 1, 2)
top_chgs = cutoffs*0
for (i in 1:(length(cutoffs)-1)) {
    E_cur = length(which(E(g)$weight>cutoffs[i]))
    E_new = length(which(E(g)$weight>cutoffs[i+1]))

    top_chgs[i] = (E_cur - E_new)/E_cur
}
top_chgs[length(top_chgs)] = 1

#6. Clustering coefficient. mean, variance and maximum
clustering_coff_mean = transitivity(g, type = "globalundirected")
clustering_coff_local = transitivity(g, type = "localundirected")
clustering_coff_variance = var(clustering_coff_local)
clustering_coff_max = max(clustering_coff_local)

#7. Topological coefficient

V0 = as.numeric(V(g))
for (v0 in V0) {
    V1 = neighbors(g, v0, 'all')
    kn = length(V1)

    J = 0
    cnt = 0
    for (v1 in V1) {
        V2 = neighbors(g, v1, 'all')

        for (v2 in V2) {
            if (v2 != v0) {
                J = J + length(which(neighbors(g, v2, 'all') %in% c(neighbors(g, v0, 'all'), v0)))
                cnt = cnt + 1
            }
        }
    }
    V(g)[v0]$topological_coef = J/cnt/kn
}

topological_coef_mean = mean(V(g)$topological_coef)
topological_coef_variance = var(V(g)$topological_coef)
topological_coef_median = median(V(g)$topological_coef)
topological_coef_max = max(V(g)$topological_coef)                                                       

item = "A50373"
features <- c(item, length(V(g)),den, mean_degree, variance_degree, median_degree, maximum_degree,   
              mean_weights, variance_weights, median_weights, maximum_weights,
              diameter, clustering_coff_mean,clustering_coff_variance,clustering_coff_max                
)
