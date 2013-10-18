# outlier_detection for family practioners

# outlier detection
rm(list=ls())
setwd('/Users/lshi/Documents/Projects/health_fraud/code/R_code')
library(DMwR)
# outlier detection

# file = "../../data/group_features_studio.csv"
# file = "../../data/result/group_features_claim_npi_thr_50.csv"
# file = "../../data/result/group_features_referal_performing_08.csv"
file = "../../data/result/group_features_referal_performing_08_all_small.csv"
#features <- read.csv(file)

features <- read.table(file, header = FALSE, sep=",")
numOfCol  = ncol(features)
list <- c(1,2,numOfCol-1,numOfCol)
features2 <- features[,-list]
features3 <- na.omit(features2)

scores <- lofactor(features3, k=5)
#plot(density(scores))
outliers <- order(scores, decreasing = T)
# TopNodes_K_5 <-features[outliers[1:200],2]
TopNodes_K_5 <- row.names(features)[outliers[1:200]]

scores2 <- lofactor(features3, k=10)
outliers2 <- order(scores2, decreasing = T)
TopNodes_K_10 <-row.names(features)[outliers2[1:200]]

scores3 <- lofactor(features3, k=15)
outliers3 <- order(scores3, decreasing = T)
TopNodes_K_15 <-row.names(features)[outliers3[1:200]]

outfile <- "../../data/result/TopNodes_K_5_family_prac_small.csv"
temp <-as.vector(features[TopNodes_K_5,2])
write(temp, outfile, sep="\n")