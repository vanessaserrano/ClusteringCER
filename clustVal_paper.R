##=====================================================##
##  CLUSTERING VALIDATION - Paper version              ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##

#### 00 SYSTEM PREPARATION ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "cluster",
         "GGally", "ggalt")
# cluster: "Finding Groups in Data": Cluster Analysis Extended, Rousseeuw et al. 
# ggalt::geom_encircle

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

# Assumptions: 
#   Factors/subscales: SPR, MT, EI, CC
#       SPR = cQ01 + cQ02 + cQ06 + cQ07 + cQ10 + cQ14 + cQ18 + cQ19,
#       MT = cQ05 + cQ08 + cQ12 + cQ15,
#       EI = cQ03 + cQ11 + cQ16 + cQ20,
#       CC = cQ04 + cQ09 + cQ13 + cQ17
#   Number of clusters: 3-4
#   Distance: euclidean
#   Method: kmeans (centroids: means by default), hclust (aggregation method: ward)
# Goal: 
#   Validation of the clusters + ICI example
#   Validation of the classification of individual observations + ICI example

ksel <- 4

#### 01 DATA PREPARATION ####
dfData <- read.table("data/ICI.tsv",sep="\t",header=T)

dfDataF1m <- dfData[,22:41]
dfDataF1m <- dfDataF1m %>% mutate(
  SPR = cQ01 + cQ02 + cQ06 + cQ07 + cQ10 + cQ14 + cQ18 + cQ19,
  MT = cQ05 + cQ08 + cQ12 + cQ15,
  EI = cQ03 + cQ11 + cQ16 + cQ20,
  CC = cQ04 + cQ09 + cQ13 + cQ17) %>% select(SPR,MT,EI,CC)
dfDataF1m$SPR <- dfDataF1m$SPR / 8
dfDataF1m[,c(2:4)] <- dfDataF1m[,c(2:4)] / 4

#### 02 CLUSTERINGS IMPORT ####
### 02.1 kmeans ####
# kmF1m_ns25: kmeans on 'a priori' factors taken as means,
#             25 internal repetitions (starting centers-partition, nstart=25)
#             1000 external repetitions for k = 2:8
load("km_clust/kmF1m_ns25.rda")
clusteringsKm <- clusterings[clusterings$k==ksel,]

### 02.2 hclust ####
# hF1m_W_euc: hierarchical clustering (AGNES), euclidean distance,
#             Ward's aggregation method on 'a priori' factors taken as means,
#             1000 repetitions
load("km_clust/hF1m_W_euc.rda")
clusteringsHc <- clusterings

lstPartitions <- as.list(rep(NA,1000))
lstCenters <- as.list(rep(NA,1000))
for(i in clusterings$i) {
    cl1 <- cutree(clusterings$hc[[i]],ksel)
    cl1 <- cl1[order(clusterings$ordering[[i]])]
    lstPartitions[[i]] <- as.numeric(factor(cl1,levels=unique(cl1)))
    lstCenters[[i]] <- data.frame(dfDataF1m,cluster=cl1) %>% 
      group_by(cluster) %>% 
      summarise(across(1:4,mean))
}
clusteringsHc$cluster <- lstPartitions
clusteringsHc$centers <- lstCenters

str(clusteringsHc)

#### 03 SELECT REFERENCE FOR EACH METHOD ####

### 03.1 Visualize reference ####


#### 04 VALIDATION OF THE CLUSTERS ####

### 04.1 Split halves ####

### 04.2 Compare repetitions ####
# AMONG THEM OR WITH REFERENCE

# ARI


### 04.3 Bootstrap ####

### 04.4 Silhouette analysis ####

#### 05 VALIDATION OF INDIVIDUAL CLASSIFICATION ####

