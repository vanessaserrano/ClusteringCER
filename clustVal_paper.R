##=====================================================##
##  CLUSTERING VALIDATION - Paper version              ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##

#### 00 SYSTEM PREPARATION ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "cluster",
         "GGally", "ggalt", "mclust")
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

for(i in clusteringsHc$i) {
    cl1 <- cutree(clusteringsHc$hc[[i]],ksel)
    cl1 <- cl1[order(clusteringsHc$ordering[[i]])]
    lstPartitions[[i]] <- as.numeric(factor(cl1,levels=unique(cl1)))
    lstCenters[[i]] <- data.frame(dfDataF1m,cluster=lstPartitions[[i]]) %>% 
      group_by(cluster) %>% 
      summarise(across(1:4,mean),.groups="drop_last")
}
clusteringsHc$cluster <- lstPartitions
clusteringsHc$centers <- lstCenters

str(clusteringsHc)


#### 03 REFERENCE ATTEMPT FOR EACH METHOD ####
# Select 1 from the set of repetitions (1000)
disF1m <- dist(dfDataF1m, method="euclidean")

### 03.1 kmeans ####
## ... select reference (optimum) attempt ####
# As minimum total within
refKm_tw <- clusteringsKm[which.min(apply(clusteringsKm, 1, 
                               function(x) x$totwss)),]

# As maximum silhouette
clusteringsKm$sil <- apply(clusteringsKm, 1,
                   function(x) {
                     sil <- silhouette(x$kmeans$cluster,
                               dist=disF1m,method="complete")
                     mean(sil[,3])
                   })
refKm_sil <- clusteringsKm[which.max(clusteringsKm$sil),]

# ARI goes from 0 to 1. 1 means identical partitions. 0 means random 
adjustedRandIndex(refKm_tw$kmeans[[1]]$cluster,refKm_sil$kmeans[[1]]$cluster)

ggplot(NULL, aes(x=clusteringsKm$sil)) +
  geom_histogram(fill="lightgrey", color="black", binwidth=0.002) +
  theme_classic()

## ... visualize reference ####
# ... ... centers ####
dfCenters <- as.data.frame(refKm_sil$kmeans[[1]]$centers)
centersAve <- apply(dfCenters,1,mean)

dfCenters$cluster <- factor(as.numeric(factor(1:4,levels=order(centersAve)))) 
dfCentersL <- pivot_longer(dfCenters,1:4,names_to="question",
                           values_to = "mean")

ggplot(dfCentersL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + 
  scale_fill_viridis_b(option="magma", direction=-1) +
  theme_bw()

# ... ... observations ####
dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(as.numeric(factor(refKm_sil$kmeans[[1]]$cluster,
                                              levels=order(centersAve))))
)

# ggpairs(dfClusterBest[,1:4],
#         diag="blankDiag",
#         mapping=ggplot2::aes(color=dfClusterBest$cluster),
#         lower=list(continuous=
#                      wrap("points",alpha=.2, position=position_jitter())))+
#   scale_fill_brewer(type="qual")+
#   theme_classic()
# 
# for(i in seq(1,ncol(dfClusterBest)-2)) {
#   for(j in seq(i+1,ncol(dfClusterBest)-1)) {
#     print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
#                                           y=colnames(dfClusterBest)[j],
#                                           color="cluster"))+
#             geom_encircle() +
#             geom_jitter(shape=21, alpha=.8)+
#             geom_point(data=dfCenters, size=20, shape="+")+
#             scale_color_brewer(type="qual", palette="Dark2")+
#             theme_classic())
#   }
# }

for(i in seq(1,ncol(dfClusterBest)-2, by=2)) {
  j <- i + 1
  print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
                                        y=colnames(dfClusterBest)[j],
                                        color="cluster")) +
          geom_encircle() +
          geom_jitter(shape=21, alpha=.2)+
          geom_point(data=dfCenters, size=20, shape="+") +
          geom_point(data=dfCenters, size=2, color="black") +
          scale_color_brewer(type="qual", palette="Dark2") +
          facet_wrap(~cluster) + 
          scale_x_continuous(breaks=seq(0,1,by=.25)) + 
          scale_y_continuous(breaks=seq(0,1,by=.25)) + 
          theme_classic())
}

# for(i in seq(1,ncol(dfClusterBest)-2, by=2)) {
#   j <- i + 1
#   print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
#                                         y=colnames(dfClusterBest)[j],
#                                         color="cluster")) +
#           geom_encircle()+
#           geom_point(data=dfCenters, size=20, shape="+") +
#           geom_point(data=dfCenters, size=2, color="black") +
#           scale_color_brewer(type="qual", palette="Dark2") +
#           facet_wrap(~cluster) + 
#           scale_x_continuous(breaks=seq(0,1,by=.25)) + 
#           scale_y_continuous(breaks=seq(0,1,by=.25)) + 
#           theme_classic())
# }


### 03.2 hclust ####
## ... select reference (optimum) attempt ####
# As minimum total within

# As maximum silhouette
clusteringsHc$sil <- apply(clusteringsHc, 1,
                           function(x) {
                             sil <- silhouette(x$cluster,
                                               dist=disF1m,method="complete")
                             mean(sil[,3])
                           })
refHc_sil <- clusteringsHc[which.max(clusteringsHc$sil),]

adjustedRandIndex(refHc_sil$cluster[[1]],refKm_sil$kmeans[[1]]$cluster)

ggplot(NULL, aes(x=clusteringsHc$sil)) +
  geom_histogram(fill="lightgrey", color="black", binwidth=0.002) +
  theme_classic()

## ... visualize reference ####
# ... ... centers ####
dfCenters <- as.data.frame(refHc_sil$centers[[1]])[,-1]
centersAve <- apply(dfCenters,1,mean)

dfCenters$cluster <- factor(as.numeric(factor(1:4,levels=order(centersAve)))) 
dfCentersL <- pivot_longer(dfCenters,1:4,names_to="question",
                           values_to = "mean")
dfCentersL <- as.data.frame(dfCentersL)
dfCentersL <- dfCentersL[order(dfCentersL$cluster),]


ggplot(dfCentersL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + 
  scale_fill_viridis_b(option="magma", direction=-1) +
  theme_bw()

# ... ... observations ####

dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(as.numeric(factor(refHc_sil$cluster[[1]],
                                              levels=order(centersAve))))
)

# ggpairs(dfClusterBest[,1:4],
#         diag="blankDiag",
#         mapping=ggplot2::aes(color=dfClusterBest$cluster),
#         lower=list(continuous=
#                      wrap("points",alpha=.2, position=position_jitter())))+
#   scale_fill_brewer(type="qual")+
#   theme_classic()
# 
# for(i in seq(1,ncol(dfClusterBest)-2)) {
#   for(j in seq(i+1,ncol(dfClusterBest)-1)) {
#     print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
#                                           y=colnames(dfClusterBest)[j],
#                                           color="cluster"))+
#             geom_encircle() +
#             geom_jitter(shape=21, alpha=.8)+
#             geom_point(data=dfCenters, size=20, shape="+")+
#             scale_color_brewer(type="qual", palette="Dark2")+
#             theme_classic())
#   }
# }

for(i in seq(1,ncol(dfClusterBest)-2, by=2)) {
  j <- i + 1
  print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
                                        y=colnames(dfClusterBest)[j],
                                        color="cluster")) +
          geom_encircle() +
          geom_jitter(shape=21, alpha=.2)+
          geom_point(data=dfCenters, size=20, shape="+") +
          geom_point(data=dfCenters, size=2, color="black") +
          scale_color_brewer(type="qual", palette="Dark2") +
          facet_wrap(~cluster) + 
          scale_x_continuous(breaks=seq(0,1,by=.25)) + 
          scale_y_continuous(breaks=seq(0,1,by=.25)) + 
          theme_classic())
}

# for(i in seq(1,ncol(dfClusterBest)-2, by=2)) {
#   j <- i + 1
#   print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
#                                         y=colnames(dfClusterBest)[j],
#                                         color="cluster")) +
#           geom_encircle()+
#           geom_point(data=dfCenters, size=20, shape="+") +
#           geom_point(data=dfCenters, size=2, color="black") +
#           scale_color_brewer(type="qual", palette="Dark2") +
#           facet_wrap(~cluster) + 
#           scale_x_continuous(breaks=seq(0,1,by=.25)) + 
#           scale_y_continuous(breaks=seq(0,1,by=.25)) + 
#           theme_classic())
# }



#### 04 VALIDATION OF THE CLUSTERS ####

### 04.1 Split halves ####

### 04.2 Compare repetitions ####
# AMONG THEM OR WITH REFERENCE

# ARI


### 04.3 Bootstrap ####

### 04.4 Silhouette analysis ####

#### 05 VALIDATION OF INDIVIDUAL CLASSIFICATION ####

