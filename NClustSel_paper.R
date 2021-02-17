##=====================================================##
##  NUMBER OF CLUSTERS - Paper version                 ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##

#### 00 SYSTEM PREPARATION ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "cluster",
         "GGally", "ggalt", "mclust", "factoextra", "NbClust")
# factoextra: Extract and Visualize the Results of Multivariate Data Analyses
# cluster: "Finding Groups in Data": Cluster Analysis Extended, Rousseeuw et al. 
# ggalt::geom_encircle
# mclust: Gaussian Mixture Modelling for Model-Based Clustering, 
#       Classification, and Density Estimation (contains ARI)

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
#   Determination of the number of clusters

k_range <- 2:8

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

disF1m <- dist(dfDataF1m, method="euclidean")

#### 02 NUMBER OF CLUSTERS ####
### 02.1 kmeans ####
# kmF1m_ns25: kmeans on 'a priori' factors taken as means,
#             25 internal repetitions (starting centers-partition, nstart=25)
#             1000 external repetitions for k = 2:8

NbClust(dfDataF1m, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "kmeans", index="all")

load("km_clust/kmF1m_ns25.rda")

dfNClust <- data.frame(k=k_range, sil=NA, totwss=NA)
for(ksel in k_range) {
  clusteringsKm <- clusterings[clusterings$k==ksel,]

  clusteringsKm$sil <- apply(clusteringsKm, 1,
                             function(x) {
                               sil <- silhouette(x$kmeans$cluster,
                                                 dist=disF1m,method="complete")
                               mean(sil[,3])
                             })
  dfNClust$sil[dfNClust$k==ksel] <- max(clusteringsKm$sil)
  dfNClust$totwss[dfNClust$k==ksel] <- min(apply(clusteringsKm, 1, 
                                                     function(x) x$totwss))
}

ggplot(dfNClust, aes(x=k, y=sil)) + geom_point(shape=21) +
  geom_line(color="grey") + theme_classic()


ggplot(dfNClust, aes(x=k, y=totwss)) + geom_point(shape=21) +
  geom_line(color="grey") + theme_classic()


### 02.2 hclust ####
# hF1m_W_euc: hierarchical clustering (AGNES), euclidean distance,
#             Ward's aggregation method on 'a priori' factors taken as means,
#             1000 repetitions
NbClust(dfDataF1m, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "ward.D2", index="all")

load("km_clust/hF1m_W_euc.rda")
clusteringsHc <- clusterings

dfNClust <- data.frame(k=k_range, sil=NA)
for(ksel in k_range) {
  vecSil <- numeric(1000)
  
  for(i in clusteringsHc$i) {
    cl1 <- cutree(clusteringsHc$hc[[i]],ksel)
    cl1 <- cl1[order(clusteringsHc$ordering[[i]])]
    vecPartition <- as.numeric(factor(cl1,levels=unique(cl1)))
    vecSil[i] <- mean(silhouette(vecPartition,
                            dist=disF1m,method="complete")[,3])
  }
  dfNClust$sil[dfNClust$k==ksel] <- max(vecSil)
}  

ggplot(dfNClust, aes(x=k, y=sil)) + geom_point(shape=21) +
  geom_line(color="grey") + theme_classic()




