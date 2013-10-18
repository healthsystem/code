# outlier detection
rm(list=ls())
setwd('/Users/lshi/Documents/Projects/health_fraud/code/R_code')
library(DMwR)
# outlier detection

# library(DMwR)
# # remove "Species", which is a categorical column
# iris2 <- iris[,1:4]
# outlier.scores <- lofactor(iris2, k=5)

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

# filtering some nan values in the matrix
infList <- is.infinite(scores)
scores2 <- scores[!infList]
nanList <- is.nan(scores2)
scores3 <- scores2[!nanList]

infIndex <- which(infList == TRUE)
InfNodes_K_13 <-features[infIndex,2]
outliers <-order(scores3, decreasing = T)
TopNodes_K_13 <-features[outliers[1:200],2]

scores <- lofactor(features3, k=5)
#plot(density(scores))

infList <- is.infinite(scores)
scores2 <- scores[!infList]
nanList <- is.nan(scores2)
scores3 <- scores2[!nanList]

infIndex <- which(infList == TRUE)
InfNodes_K_5 <-features[infIndex,2]
outliers <-order(scores3, decreasing = T)
TopNodes_K_5 <-features[outliers[1:200],2]

scores <- lofactor(features3, k=10)
#plot(density(scores))

infList <- is.infinite(scores)
scores2 <- scores[!infList]
nanList <- is.nan(scores2)
scores3 <- scores2[!nanList]

infIndex <- which(infList == TRUE)
InfNodes_K_10 <-features[infIndex,2]
outliers <-order(scores3, decreasing = T)
TopNodes_K_10 <-features[outliers[1:200],2]

n <- nrow(features3)
labels <- 1:n
labels[-infIndex] <- "."
biplot(prcomp(features3), cex=.8, xlabs=labels)
biplot(prcomp(features3), scale = TRUE)

fit <-princomp(features3, cor = TRUE)
summary(fit)

outfile <- "../../data/result/TopNodes_K_5_family_prac.csv"
temp <-as.vector(features[TopNodes_K_5,2])
write(temp, outfile, sep="\n")