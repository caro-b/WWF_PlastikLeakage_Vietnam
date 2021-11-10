#################### Statistical Analysis ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##

library(psych)
library(corrplot)
library(ggplot2)


#### I. Cluster Analysis ####

## 0. Data Preparation

# convert to standard dataframe & only keep plastic leakage factors
leakage_factors <- (landfills_factors %>% st_drop_geometry())[,c(5,7:15)]

str(leakage_factors)

# change character to integer values
from <- c("10-30.000","30-50.000","50-70.000","70-200.000","200-300.000","300-2.500.000")
to <- c(10000,30000,50000,70000,200000,300000)
leakage_factors$waste <- plyr::mapvalues(leakage_factors$waste, from, to)
leakage_factors$waste <- as.integer(leakage_factors$waste)

## Scatter Plots of all possible factor combinations
pairs(leakage_factors)

## remove rows with NA data
leakage_factors_clean <- na.omit(leakage_factors)

# data must be numeric (K-means does not work with categorical data)
## remove categorical data
leakage_factors_km <- leakage_factors_clean[,c(1:9)]

## Normalize Data
# to mean= 0 & standard deviaion = 1
leakage_factors_norm <- scale(leakage_factors_km)

head(leakage_factors_norm)
hist(leakage_factors_norm)



#### I. Factor Analysis ####

describe(leakage_factors_clean)
dim(leakage_factors_clean)

## correlation matrix
datamatrix <- cor(leakage_factors_norm)
corrplot(datamatrix, method="number")

X <- leakage_factors_norm

KMO(r=cor(X)) # data not factorable?

cortest.bartlett(X) # significance level < 0.05 indicates factor analysis might be useful

## parallel analysis
parallel <- fa.parallel(X)

fa.none <- fa(r=X, 
              nfactors = 2, 
              # covar = FALSE, SMC = TRUE,
              fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax") # none rotation
print(fa.none)

## graph factor loadings
fa.diagram(fa.none)



#### II. K-means Clustering (Unsupervised Machine Learning)

#### Clustering via distance measure
# similar objects are close to one another
distance <- get_dist(leakage_factors_norm)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#### K-means Clustering

## first find optimal numbers of clusters (elbow method)
# minimize total intra-cluster variation
fviz_nbclust(leakage_factors_norm, kmeans, method = "wss")

## Silhouette Method
# how well each object lies within its cluster
fviz_nbclust(leakage_factors_norm, kmeans, method = "silhouette")

# gap statistic
gap_stat <- clusGap(leakage_factors_norm,
                    FUN = kmeans,
                    K.max = 9,
                    nstart = 25,
                    B = 50)

# Plotten der Anzahl der Cluster vs. Lückenstatistik
fviz_gap_stat(gap_stat)

best <- FitKMeans(leakage_factors_norm, max.clusters=9, nstart=25, seed=123) > best


# set seed to make clustering reproducible
set.seed(123)
km <- kmeans(leakage_factors_norm, centers = 3, nstart=25) # 3 clusters (low, medium & high risk), random starting condition

km 

# plot clustering results
fviz_cluster(km, data = leakage_factors_norm)


# mean factor values per Cluster
aggregate(leakage_factors_km, by=list(cluster=norm$cluster), mean)


# add clusters to original data
final_data <- cbind(leakage_factors_km, cluster = km$cluster)

# Enddaten anzeigen
head(final_data)



#### III. Herarchical Clustering

# Compute dissimilarity matrix
res.dist <- dist(leakage_factors_norm, method = "euclidean")

# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")

# Visualize
plot(res.hc, cex = 0.5)

fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
          gith)



#### Enhanced hierarchical clustering
res.ec <- eclust(leakage_factors_norm, "hclust", k = 3) # compute hclust

## Dendogram
fviz_dend(res.ec, rect = TRUE) # Add rectangle around groups
fviz_cluster(res.ec)


#### TODO: function: create risk per landfill ####



