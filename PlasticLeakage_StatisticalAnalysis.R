#################### Statistical Analysis ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##


#### 0. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("corrplot","cluster","DataExplorer","dbscan","dplyr","factoextra","fpc","ggplot","psych","readr","sf","sp","tidyverse")
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

## take smallest water distance
landfills_factors$watermin <- with(landfills_factors, 
                                   pmin(landfills_factors$dist_water, landfills_factors$dist_permwater, landfills_factors$dist_ocean))

## account for NA values
landfills_factors[is.na(landfills_factors$waste),]

## Con Do belongs to Bà Rịa–Vũng Tàu province (=70-200.000 waste)
landfills_factors$waste[is.na(landfills_factors$waste)] <- "70-200.000"

str(landfills_factors)

# change character to integer values
from <- c("10-30.000","30-50.000","50-70.000","70-200.000","200-300.000","300-2.500.000")
to <- c(10000,30000,50000,70000,200000,300000)
landfills_factors$waste <- plyr::mapvalues(landfills_factors$waste, from, to)
landfills_factors$waste <- as.integer(landfills_factors$waste)

str(landfills_factors)

# only keep plastic leakage factors
leakage_factors <- landfills_factors[,c(5,8:18)]

## remove dependent variable (leakage)
leakage_factors_indep <- leakage_factors[,-11]

## main variables
leakage_factors_main <- leakage_factors_indep[,c(2:3,6,9,11)]


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

## main variables
# dropping waste as higher correlation
leakage_factors_indep[,c(1:3,6,9,11)] %>% plot_correlation() 
## positive correlation: high correlation between waste & area, rel. high between rain & no. storms
## negative correlation: rel. high between waste & rain, waste & distance water

## Boxplots
boxplot(leakage_factors_main)

par(mfrow=c(1,6))
boxplot(leakage_factors_main[,1])
boxplot(leakage_factors_main[,2])
boxplot(leakage_factors_main[,3])
boxplot(leakage_factors_main[,4])
boxplot(leakage_factors_main[,5])
boxplot(leakage_factors_main[,6])



#### III. Factor Analysis ####

describe(leakage_factors_indep)
X <- leakage_factors_indep

KMO(r=cor(X)) # data not factorable?

cortest.bartlett(X) # significance level < 0.05 indicates factor analysis might be useful

## parallel analysis
parallel <- fa.parallel(X) # 2 factors

fa.none <- fa(r=X, 
              nfactors = 4, 
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
leakage_factors_km <- leakage_factors_main


## Normalize Data to mean= 0 & standard deviation = 1
leakage_factors_norm <- scale(leakage_factors_km)

head(leakage_factors_norm)
hist(leakage_factors_norm)



#### a) Distance Measures

# similar objects are close to one another
distance <- get_dist(leakage_factors_indep, "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



#### b) K-Means Clustering

## Unsupervised - no response variable

## first find optimal numbers of clusters (elbow method)
# minimize total intra-cluster variation
fviz_nbclust(leakage_factors_norm, kmeans, method = "wss")

## Silhouette Method
# how well each object lies within its cluster
fviz_nbclust(leakage_factors_norm, kmeans, method = "silhouette")
# 3 (water aggregated to watermin, dropping nothing)


# gap statistic
gap_stat <- clusGap(leakage_factors_norm,
                    FUN = kmeans,
                    K.max = 9,
                    nstart = 25,
                    B = 50)

# Plotten der Anzahl der Cluster vs. Lückenstatistik
fviz_gap_stat(gap_stat)


# set seed to make clustering reproducible
set.seed(123)
km <- kmeans(leakage_factors_norm, centers = 3, nstart=25) # 3 clusters (low, medium & high risk), random starting condition

km 

# plot clustering results
fviz_cluster(km, data = leakage_factors_norm)


# mean variable values per Cluster
aggregate(leakage_factors_km, by=list(cluster=km$cluster), mean)


# add clusters to original data
leakage_factors_main <- cbind(leakage_factors_main, km_cluster = km$cluster)
# cluster2: dropping waste & storms

head(leakage_factors_main)


#### TODO: try clustering with only numeric (not categorical data) ####



#### c) Herarchical Clustering

# Compute dissimilarity matrix
res.dist <- dist(leakage_factors_norm, method = "euclidean")

# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")

# Visualize
plot(res.hc, cex = 0.5)

fviz_dend(res.hc, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)



#### d) Enhanced Hierarchical Clustering
res.ec <- eclust(leakage_factors_norm, "hclust", k = 3) # compute hclust

## Dendogram
fviz_dend(res.ec, rect = TRUE) # Add rectangle around groups
fviz_cluster(res.ec)

fviz_cluster(res.ec, data = leakage_factors_norm)

aggregate(leakage_factors_km, by=list(cluster=res.ec$cluster), mean)

leakage_factors_main <- cbind(leakage_factors_main, ec_cluster = res.ec$cluster)




#### e) Fuzzy Clustering
fc <- fanny(leakage_factors_km, 3, metric = "euclidean")

# low value of Dunn’s coefficient indicates a very fuzzy clustering, a value close to 1 indicates a near-crisp clustering
fc$coeff

fviz_cluster(fc, repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")

fviz_silhouette(fc, palette = "jco",
                ggtheme = theme_minimal())



#### f) Density-Based Clustering 
## as kmeans & hierarchical clustering severely affected by noise and outliers in data

# find optimum eps value
kNNdistplot(leakage_factors_norm, k = 3)
abline(h = 2.4, lty = 2)

# Compute DBSCAN using fpc package
set.seed(123)
db <- dbscan(leakage_factors_norm, eps = 2.5, MinPts = 3)

# Plot DBSCAN results
fviz_cluster(db, data = leakage_factors_norm, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point", palette = "jco", ggtheme = theme_classic())



#### Risk Assessment ####

describe(leakage_factors_main)

## attribute low, medium & high risk to landfills according to literature values & variable quantiles 
# low: 0, medium: 1, high: 2

## rain
# < 3 : low risk 
# < 15: medium risk
quantile(leakage_factors_main$rain)
quantile(leakage_factors_main$rain, probs = c(1/3, 2/3))

## wind
# 0: low risk
# < 11: medium risk
quantile(leakage_factors_main$windspeed)
quantile(leakage_factors_main$windspeed, probs = c(1/3, 2/3))

## water distance
# > 1km: low risk
# < 1km & > 500m: medium risk
# < 500m: high risk
quantile(leakage_factors_main$watermin)
quantile(leakage_factors_main$watermin, probs = c(1/3, 2/3))

## flooding (%)
# < 10: low risk
# < 20: medium risk
quantile(leakage_factors_main$flood_risk)
quantile(leakage_factors_main$flood_risk, probs = c(1/3, 2/3))

## slope
# < 0.01: low risk
# < 0.05: medium risk
quantile(leakage_factors_main$slope)
quantile(leakage_factors_main$slope, probs = c(1/3, 2/3))

## (landfill area)
#
quantile(leakage_factors_main$area_ha)



leakage_factors_main$risk <- 0
## compute risk value per landfill
# 0: low, 1: medium, 2: high

i <- 1
while (i <= nrow(leakage_factors_main)) {
  ## rain risk
  ifelse(leakage_factors_main$rain[i] <= 3, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$rain[i] < 15, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))

  ## windspeed
  ifelse(leakage_factors_main$windspeed[i] == 0, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$windspeed[i] < 11, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  ## water
  ifelse(leakage_factors_main$watermin[i] > 1000, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$watermin[i] > 500, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))

  ## flooding
  ifelse(leakage_factors_main$flood_risk[i] <= 10, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$flood_risk[i] < 20, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  ## slope
  ifelse(leakage_factors_main$slope[i] < 0.01, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$slope[i] < 0.05, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  ## increment i
  i <- i+1
}

## add low, medium or high risk to risk value
leakage_factors_main$risk_label <- "medium"
leakage_factors_main$risk_label[leakage_factors_main$risk <= 3] <- "low"
leakage_factors_main$risk_label[leakage_factors_main$risk >= 7] <- "high"

