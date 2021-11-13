#################### Statistical Analysis ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##


#### 0. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("corrplot","DataExplorer","dplyr","psych","readr","sf","sp","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'D:/Documents/WWF_PlastikLeakage_Vietnam/data'


## import dataframe as CSV
filename <- paste(dir,"/landfill_variables.csv", sep= "")
landfills_factors <- read.csv(filename, sep= ";")



#### I. Data Preparation

#### TODO: assign high, medium & low risk to landfills ####
## -> to account for different loadings/directions of variables (e.g. low distances to ocean - high risk, but low rain = low risk)


## account for NA values
landfills_factors[is.na(landfills_factors$waste),]

## Con Do belongs to Bà Rịa–Vũng Tàu province (=70-200.000 waste)
landfills_factors$waste[is.na(landfills_factors$waste)] <- "70-200.000"

## remove dependent variable (leakage)
leakage_factors_indep <- leakage_factors[,-11]

# convert to standard dataframe & only keep plastic leakage factors
leakage_factors <- landfills_factors[,c(5,8:17)]

str(leakage_factors)

# change character to integer values
from <- c("10-30.000","30-50.000","50-70.000","70-200.000","200-300.000","300-2.500.000")
to <- c(10000,30000,50000,70000,200000,300000)
leakage_factors$waste <- plyr::mapvalues(leakage_factors$waste, from, to)
leakage_factors$waste <- as.integer(leakage_factors$waste)

str(leakage_factors)


## download dataframe as CSV
filename <- paste(dir,"/landfill_variables.csv", sep= "")
write.table(landfills_factors, file = filename, row.names = F, fileEncoding = "UTF-8", sep = ";")



#### II. Exploratory Data Analysis ####

## Density Plot
leakage_factors %>% plot_density()
## rain has different distribution

## Histogram
leakage_factors %>% plot_histogram()

## Scatter Plots of all possible factor combinations
pairs(leakage_factors)

## Correlation Matrix
leakage_factors %>% plot_correlation() 
## high correlation between waste & area, rel. high between rain & no. storms

#### TODO: try dropping rain & waste ####



#### III. Factor Analysis ####

describe(leakage_factors_clean)
dim(leakage_factors_clean)

X <- leakage_factors_clean[-8] # drop storms as value 0

KMO(r=cor(X)) # data not factorable?

cortest.bartlett(X) # significance level < 0.05 indicates factor analysis might be useful

## parallel analysis
parallel <- fa.parallel(X) # 2 factors

fa.none <- fa(r=X, 
              nfactors = 2, 
              # covar = FALSE, SMC = TRUE,
              fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax") # none rotation
print(fa.none)

## graph factor loadings
fa.diagram(fa.none)



#### IV. Cluster Analysis ####

#### 0) Normalize

## remove categorical data
# data must be numeric (K-means does not work with categorical data)
leakage_factors_km <- leakage_factors_clean[,c(1:7,9)]


## Normalize Data to mean= 0 & standard deviaion = 1
leakage_factors_norm <- scale(leakage_factors_km)

head(leakage_factors_norm)
hist(leakage_factors_norm)



#### a) Distance Measures

# similar objects are close to one another
distance <- get_dist(leakage_factors_clean, "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



#### b) K-Means Clustering

## Unsupervised - no response variable

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

#best <- FitKMeans(leakage_factors_norm, max.clusters=9, nstart=25, seed=123) > best


# set seed to make clustering reproducible
set.seed(123)
km <- kmeans(leakage_factors_norm, centers = 3, nstart=25) # 3 clusters (low, medium & high risk), random starting condition

km 

# plot clustering results
fviz_cluster(km, data = leakage_factors_norm)


# mean factor values per Cluster
aggregate(leakage_factors_km, by=list(cluster=km$cluster), mean)


# add clusters to original data
final_data <- cbind(leakage_factors_km, cluster = km$cluster)

# Enddaten anzeigen
head(final_data)


#### TODO: try clustering with only numeric (not categorical data) ####



#### c) Herarchical Clustering

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
)

aggregate(leakage_factors_km, by=list(cluster=res.hc$cluster), mean)



#### d) Enhanced Hierarchical Clustering
res.ec <- eclust(leakage_factors_norm, "hclust", k = 3) # compute hclust

## Dendogram
fviz_dend(res.ec, rect = TRUE) # Add rectangle around groups
fviz_cluster(res.ec)

