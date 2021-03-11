##=====================================================##
##  CLUSTERING VALIDATION                              ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##

# This script include the different approaches to clustering validation.
# All of them are run for kmeans (with 25 random starting points) and 
# hierarchical clustering using euclidean distance and Ward's method
# for aggregation


#### 00 SYSTEM PREPARATION ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "cluster",
         "GGally", "ggalt", "mclust", "factoextra", "gtools")
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

# Cluster indexes are ordered following the order of the data in the 
# initial data set


#### 03 REFERENCE ATTEMPT FOR EACH METHOD ####
# Select 1 from the set of repetitions (1000)
disF1m <- dist(dfDataF1m, method="euclidean")

### 03.1 kmeans ####
## ... select reference (optimum) attempt ####
# As maximum silhouette
clusteringsKm$sil <- apply(clusteringsKm, 1,
                           function(x) {
                             sil <- silhouette(x$kmeans$cluster,
                                               dist=disF1m,method="complete")
                             mean(sil[,3])
                           })
refKm_sil <- clusteringsKm[which.max(clusteringsKm$sil),]

# As minimum WSS
refKm_tw <- clusteringsKm[which.min(apply(clusteringsKm, 1, 
                               function(x) x$totwss)),]
# The reference solution will be the optimum one according to the
# minimum total within sum-of-squares (WSS) criterion, as it maps better
# to the internal optimization of the kmeans method.


## ... visualize reference (total within) ####
# ... ... centers ####
dfCenters <- as.data.frame(refKm_tw$kmeans[[1]]$centers)
centersAve <- apply(dfCenters,1,mean)

dfCenters$cluster <- factor(as.numeric(factor(1:ksel,levels=order(centersAve)))) 
dfCentersL <- pivot_longer(dfCenters,1:4,names_to="question",
                           values_to = "mean")

ggplot(dfCentersL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + 
  scale_fill_viridis_b(option="magma", direction=-1) +
  theme_bw()

# ... ... observations ####
# Facetted scatterplots
dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(as.numeric(factor(refKm_tw$kmeans[[1]]$cluster,
                                              levels=order(centersAve))))
)

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


# Boxplots
dfClusterBestL <- dfClusterBest %>% 
  pivot_longer(1:4,names_to="factor",values_to="value")

ggplot(dfClusterBestL,aes(x=factor, fill=cluster, color=cluster, y=value)) +
  geom_boxplot(alpha=0.4, position = position_dodge2(padding=.2), width=.4) +
  geom_boxplot(fill=NA, size=.6, position = position_dodge2(padding=.2), width=.4) +
  scale_fill_brewer(type="qual", palette="Dark2") +
  scale_color_brewer(type="qual", palette="Dark2") +
  labs(x="", y="")+
  theme_classic()

# Facetted boxplots
ggplot(dfClusterBestL,aes(x=factor, fill=cluster, color=cluster, y=value)) +
  geom_boxplot(alpha=0.4, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  geom_boxplot(fill=NA, size=.6, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  scale_fill_brewer(type="qual", palette="Dark2") +
  scale_color_brewer(type="qual", palette="Dark2") +
  facet_wrap(~cluster)+
  labs(x="", y="")+
  theme_classic()

# Cluster sizes
table(dfClusterBest$cluster)


### 03.2 hclust ####
## ... select reference (optimum) attempt ####
# As maximum silhouette
clusteringsHc$sil <- apply(clusteringsHc, 1,
                           function(x) {
                             sil <- silhouette(x$cluster,
                                               dist=disF1m,method="complete")
                             mean(sil[,3])
                           })
refHc_sil <- clusteringsHc[which.max(clusteringsHc$sil),]

# As minimum WSS
# Should we do it? Not done because its calculation is not readily
# available

## ... visualize reference ####
# ... ... centers ####
dfCenters <- as.data.frame(refHc_sil$centers[[1]])[,-1]
centersAve <- apply(dfCenters,1,mean)

dfCenters$cluster <- factor(as.numeric(factor(1:ksel,levels=order(centersAve)))) 
dfCentersL <- pivot_longer(dfCenters,1:4,names_to="question",
                           values_to = "mean")
dfCentersL <- as.data.frame(dfCentersL)
dfCentersL <- dfCentersL[order(dfCentersL$cluster),]

ggplot(dfCentersL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + 
  scale_fill_viridis_b(option="magma", direction=-1) +
  theme_bw()

# ... ... observations ####

# Facetted scatterplots
dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(
    as.numeric(factor(refHc_sil$cluster[[1]],
                      levels=order(centersAve))))
)


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

# Boxplots
dfClusterBestL <- dfClusterBest %>% 
  pivot_longer(1:4,names_to="factor",values_to="value")

ggplot(dfClusterBestL,aes(x=factor, fill=cluster, color=cluster, y=value)) +
  geom_boxplot(alpha=0.4, position = position_dodge2(padding=.2), width=.4) +
  geom_boxplot(fill=NA, size=.6, position = position_dodge2(padding=.2), width=.4) +
  scale_fill_brewer(type="qual", palette="Dark2") +
  scale_color_brewer(type="qual", palette="Dark2") +
  labs(x="", y="")+
  theme_classic()

# Facetted boxplots
ggplot(dfClusterBestL,aes(x=factor, fill=cluster, color=cluster, y=value)) +
  geom_boxplot(alpha=0.4, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  geom_boxplot(fill=NA, size=.6, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  scale_fill_brewer(type="qual", palette="Dark2") +
  scale_color_brewer(type="qual", palette="Dark2") +
  facet_wrap(~cluster)+
  labs(x="", y="")+
  theme_classic()

# Cluster sizes
table(dfClusterBest$cluster)


#### 04 VALIDATION OF THE CLUSTERS ####

### 04.1 Split halves ####
# There is a significant risk of missing clusters.
# In case this approach is followed many different halves should be
# tested and presented.

### 04.2 Compare repetitions ####
## ... kmeans ####
# ... ... centers distribution ####
sel <- clusteringsKm$kmeans

for(i in seq(length(sel))) {
  if(i == 1) {
    dfCenters <- as.data.frame(sel[[i]]$centers)
    dfCenters$i <- i
  } else {
    dfCenters1 <- as.data.frame(sel[[i]]$centers)
    dfCenters1$i <- i
    dfCenters <- rbind(dfCenters,dfCenters1)
  }
}

dfCentersL <- dfCenters %>% 
  pivot_longer(.,1:(ncol(.)-1), names_to = "factor", values_to = "mean")

dfBest <- as.data.frame(refKm_tw$kmeans[[1]]$centers)
centersAve <- apply(dfBest,1,mean)

dfBest$cluster <- factor(as.numeric(factor(1:ksel,levels=order(centersAve)))) 

dfBestL <- dfBest %>% 
  pivot_longer(.,1:(ncol(.)-1), names_to = "factor", values_to = "mean")

ggplot(dfCentersL, aes(x=mean)) + 
  geom_vline(aes(color=cluster,xintercept=mean),
             data=dfBestL, size=1.5) +
  geom_density() +
  scale_x_continuous(breaks=seq(0,1,by=.25), limits=c(0,1)) + 
  facet_wrap(~factor, scales="free_y") +
  scale_color_brewer(type="qual", palette="Dark2") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())


# ... ... visualize ARI distribution ####
partBest <- refKm_tw$kmeans[[1]]$cluster

ari <- numeric(length(sel))
for(i in seq(length(sel))) {
  part1 <- sel[[i]]$cluster
  cluOrder <- unique(part1)
  part1 <- as.numeric(factor(part1,
                             levels=cluOrder))
  ari[i] <- adjustedRandIndex(part1,partBest)
}

ggplot(NULL, aes(x=ari, y=..density..)) +
  geom_histogram(color="black", fill="lightgrey", boundary=1) +
  geom_density() +
  scale_x_continuous(limits=c(0,1.2),
                     breaks=seq(0,1,by=.2))+
  labs(y="") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

table(cut(ari,breaks=seq(0,1,by=.2)))


## ... hclust ####
# ... ... centers distribution ####
sel <- clusteringsHc

for(i in seq(length(sel))) {
  if(i == 1) {
    dfCenters <- as.data.frame(sel[i,]$centers)[,-1]
    dfCenters$i <- i
  } else {
    dfCenters1 <- as.data.frame(sel[i,]$centers)[,-1]
    dfCenters1$i <- i
    dfCenters <- rbind(dfCenters,dfCenters1)
  }
}

dfCentersL <- dfCenters %>% 
  pivot_longer(.,1:(ncol(.)-1), names_to = "factor", values_to = "mean")

dfBest <- as.data.frame(refHc_sil$centers)[,-1]
centersAve <- apply(dfBest,1,mean)

dfBest$cluster <- factor(as.numeric(factor(1:ksel,levels=order(centersAve)))) 

dfBestL <- dfBest %>% 
  pivot_longer(.,1:(ncol(.)-1), names_to = "factor", values_to = "mean")

ggplot(dfCentersL, aes(x=mean)) + 
  geom_vline(aes(color=cluster,xintercept=mean),
             data=dfBestL, size=1.5) +
  geom_density() +
  scale_x_continuous(breaks=seq(0,1,by=.25), limits=c(0,1)) + 
  facet_wrap(~factor, scales="free_y") +
  scale_color_brewer(type="qual", palette="Dark2") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())


# ... ... visualize ARI distribution ####
partBest <- unlist(refHc_sil$cluster)

ari <- numeric(nrow(sel))
for(i in seq(nrow(sel))) {
  part1 <- unlist(sel$cluster[i])
  cluOrder <- unique(part1)
  part1 <- as.numeric(factor(part1,
                             levels=cluOrder))
  ari[i] <- adjustedRandIndex(part1,partBest)
}

ggplot(NULL, aes(x=ari, y=..density..)) +
  geom_histogram(color="black", fill="lightgrey", boundary=1) +
  geom_density() +
  scale_x_continuous(limits=c(0,1.2),
                     breaks=seq(0,1,by=.2))+
  labs(y="") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

table(cut(ari,breaks=seq(0,1,by=.2)))


### 04.3 Bootstrap ####
## ... kmeans ####
load("km_clust/kmF1m_ns25_bs.rda")
clusteringsKmBs <- clusterings[clusterings$k==ksel,]

lstCenters <- lapply(clusteringsKmBs$kmeans, function(x) as.data.frame(x$centers))
lstClSizes <- lapply(clusteringsKmBs$kmeans, function(x) as.data.frame(table(x$cluster)))

dfCentersBest <- as.data.frame(refKm_tw$kmeans[[1]]$centers)
clSize <- as.data.frame(table(refKm_tw$kmeans[[1]]$cluster))
dfCentersBest$n <- clSize$Freq 

centersAve <- apply(dfCentersBest[,1:4],1,mean)
dfCentersBest$cluster <- factor(as.numeric(factor(1:ksel,levels=order(centersAve)))) 
dfCentersBest <- dfCentersBest[order(dfCentersBest$cluster),]

dfPermutations <- permutations(6,6)

lstClMatches <- as.list(rep(NA,1000))
i <- 1
for(i in 1:length(lstCenters)) {
  dfDistances <- as.data.frame(dfPermutations)
  dfDistances$dist <- NA
  centersDist <- 0
  j <- 1
  for(j in seq(nrow(dfDistances))) {
    centerOrder <- unlist(dfDistances[j,])
    dfCentersActual <- lstCenters[[i]]
    dfCentersActual$n <- lstClSizes[[i]]$Freq 
    k <- 1
    centersDist <- 0
    for(k in seq(nrow(dfCentersBest))) {
      centersDist <- sum(c(centersDist,
                           dist(matrix(c(unlist(dfCentersBest[k,1:4]),
                                       unlist(dfCentersActual[centerOrder[k],1:4])),
                                       nrow=2, byrow=T))))  
    }
    dfDistances$dist[j] <- centersDist
  }
  lstClMatches[[i]] <- dfDistances[which.min(dfDistances$dist),]
}

sumDist <- lapply(lstClMatches, function(x) x$dist)
sumDist <- unlist(sumDist)

ggplot(NULL, aes(x=sumDist)) + 
  geom_histogram(fill="lightgray", color="black") +
  theme_classic()

data.frame(dfCentersBest[,1:5], ave=apply(dfCentersBest[,1:4],1,mean))
i <- 2
cbind(lstCenters[[i]],n=lstClSizes[[i]][,2])[as.numeric(lstClMatches[[i]][1:ksel]),]

for(k in seq(nrow(dfCentersBest))) {
  dfClCenterDist <- as.data.frame(matrix(rep(0,length(lstCenters)*5),
                                         ncol=5))
  colnames(dfClCenterDist) <- colnames(dfCentersBest[,1:5])
  for(i in 1:length(lstCenters)) {
    dfClCenterDist[i,] <- cbind(lstCenters[[i]],
                                n=lstClSizes[[i]][,2])[
                                  as.numeric(lstClMatches[[i]][1:ksel]),][k,]
  }
  if(k==1) {
    dfCentersDist <- cbind(cluster=k, dfClCenterDist)
  } else {
    dfCentersDist <- rbind(dfCentersDist,
      cbind(cluster=k, dfClCenterDist))
  }
}

dfCentersDistL <- dfCentersDist[,1:5] %>% 
  pivot_longer(2:5, names_to="factor") 

dfCentersDistL$cluster <- factor(dfCentersDistL$cluster)
dfCentersDistL$factor <- factor(dfCentersDistL$factor)

dfCentersBest[,1:4]

dfCentersBestL <- cbind(cluster=1:ksel, dfCentersBest[,1:4]) %>% 
  pivot_longer(2:5, names_to="factor")

dfCentersBestL$cluster <- factor(dfCentersBestL$cluster)
dfCentersBestL$factor <- factor(dfCentersBestL$factor)

ggplot(dfCentersDistL,aes(x=factor, fill=cluster, color=cluster, y=value)) +
  geom_boxplot(alpha=0.4, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  geom_boxplot(fill=NA, size=.6, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  geom_point(data=dfCentersBestL, shape=3, size=2, color="black")+
  scale_fill_brewer(type="qual", palette="Dark2") +
  scale_color_brewer(type="qual", palette="Dark2") +
  facet_wrap(~cluster)+
  labs(x="", y="")+
  theme_classic()

# Bootstraped intervals 
# (+- 1.96 s, alfa = 0.05, assumed normality)

dfCentersIsd <- dfCentersDist %>% 
  pivot_longer(2:6, names_to="factor") %>%
  group_by(cluster, factor) %>% 
  summarise(CI = qnorm(.975) * sd(value), .groups = "drop") %>% 
  arrange(cluster,factor)

dfCentersIm <- dfCentersDist %>% 
  pivot_longer(2:6, names_to="factor") %>%
  group_by(cluster, factor) %>% 
  summarise(mean(value), .groups = "drop") %>% 
  arrange(cluster,factor)

(dfCentersI <- data.frame(dfCentersIm[,1:2],
                          CI=paste(unlist(dfCentersIm[,3]),"\u00b1",
                                   unlist(dfCentersIsd[,3]))))


# Bias-corrected bootstraped intervals 
# (+- 1.96 s, alfa = 0.05, assumed normality)

dfCentersIsd <- dfCentersDist %>% 
  pivot_longer(2:6, names_to="factor") %>%
  group_by(cluster, factor) %>% 
  summarise(CI = qnorm(.975) * sd(value), .groups = "drop") %>% 
  arrange(cluster,factor)

dfCentersIm <- data.frame(cluster=1:ksel, dfCentersBest[1:5]) %>% 
  pivot_longer(2:6, names_to="factor") %>% 
  arrange(cluster,factor)
  
(dfCentersI <- data.frame(dfCentersIm[,1:2],
                         CI=paste(unlist(dfCentersIm[,3]),"\u00b1",
                                  unlist(dfCentersIsd[,3]))))

## ... hclust ####
load("km_clust/hF1m_W_euc_bs.rda")
clusteringsHcBs <- clusterings

lstPartitions <- as.list(rep(NA,1000))
lstCenters <- as.list(rep(NA,1000))

i<-100
for(i in clusteringsHcBs$i) {
  cl1 <- cutree(clusteringsHcBs$hc[[i]],ksel)
  lstPartitions[[i]] <- as.numeric(factor(cl1,levels=unique(cl1)))
  lstCenters[[i]] <- data.frame(
    dfDataF1m[clusteringsHcBs$ordering[[i]],],
    cluster=lstPartitions[[i]]) %>% 
    group_by(cluster) %>% 
    summarise(across(1:4,mean),.groups="drop_last")
}
clusteringsHcBs$cluster <- lstPartitions
clusteringsHcBs$centers <- lstCenters

lstClSizes <- lapply(clusteringsHcBs$cluster, function(x) as.data.frame(table(x)))

dfCentersBest <- as.data.frame(refHc_sil$centers)[,2:5]
clSize <- as.data.frame(table(refHc_sil$cluster))
dfCentersBest$n <- clSize$Freq 

centersAve <- apply(dfCentersBest[,1:4],1,mean)
dfCentersBest$cluster <- factor(as.numeric(factor(1:ksel,levels=order(centersAve)))) 
dfCentersBest <- dfCentersBest[order(dfCentersBest$cluster),]

dfPermutations <- permutations(6,6)

lstClMatches <- as.list(rep(NA,1000))
i <- 1
for(i in 1:length(lstCenters)) {
  dfDistances <- as.data.frame(dfPermutations)
  dfDistances$dist <- NA
  centersDist <- 0
  for(j in seq(nrow(dfDistances))) {
    centerOrder <- unlist(dfDistances[j,])
    dfCentersActual <- lstCenters[[i]][,2:5]
    dfCentersActual$n <- lstClSizes[[i]]$Freq 
    k <- 1
    centersDist <- 0
    for(k in seq(nrow(dfCentersBest))) {
      centersDist <- sum(c(centersDist,
                           dist(matrix(c(unlist(dfCentersBest[k,1:4]),
                                         unlist(dfCentersActual[centerOrder[k],
                                                                colnames(dfCentersBest)[1:4]])),
                                       nrow=2, byrow=T))))  
    }
    dfDistances$dist[j] <- centersDist
  }
  lstClMatches[[i]] <- dfDistances[which.min(dfDistances$dist),]
}

sumDist <- lapply(lstClMatches, function(x) x$dist)
sumDist <- unlist(sumDist)

ggplot(NULL, aes(x=sumDist)) + 
  geom_histogram(fill="lightgray", color="black") +
  theme_classic()

data.frame(dfCentersBest[,1:5], ave=apply(dfCentersBest[,1:4],1,mean))
i <- 2
cbind(lstCenters[[i]],n=lstClSizes[[i]][,2])[as.numeric(lstClMatches[[i]][1:ksel]),]

for(k in seq(nrow(dfCentersBest))) {
  dfClCenterDist <- as.data.frame(matrix(rep(0,length(lstCenters)*5),
                                         ncol=5))
  colnames(dfClCenterDist) <- colnames(dfCentersBest[,1:5])
  for(i in 1:length(lstCenters)) {
    dfClCenterDist[i,] <- cbind(lstCenters[[i]][,2:5],
                                n=lstClSizes[[i]][,2])[
                                  as.numeric(lstClMatches[[i]][1:ksel]),][k,]
  }
  if(k==1) {
    dfCentersDist <- cbind(cluster=k, dfClCenterDist)
  } else {
    dfCentersDist <- rbind(dfCentersDist,
                           cbind(cluster=k, dfClCenterDist))
  }
}

dfCentersDistL <- dfCentersDist[,1:5] %>% 
  pivot_longer(2:5, names_to="factor") 

dfCentersDistL$cluster <- factor(dfCentersDistL$cluster)
dfCentersDistL$factor <- factor(dfCentersDistL$factor)

dfCentersBest[,1:4]

dfCentersBestL <- cbind(cluster=1:ksel, dfCentersBest[,1:4]) %>% 
  pivot_longer(2:5, names_to="factor")

dfCentersBestL$cluster <- factor(dfCentersBestL$cluster)
dfCentersBestL$factor <- factor(dfCentersBestL$factor)

ggplot(dfCentersDistL,aes(x=factor, fill=cluster, color=cluster, y=value)) +
  geom_boxplot(alpha=0.4, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  geom_boxplot(fill=NA, size=.6, position = position_dodge2(padding=.2), width=.4, outlier.color = NA) +
  geom_point(data=dfCentersBestL, shape=3, size=2, color="black")+
  scale_fill_brewer(type="qual", palette="Dark2") +
  scale_color_brewer(type="qual", palette="Dark2") +
  facet_wrap(~cluster)+
  labs(x="", y="")+
  theme_classic()

# Bootstraped intervals 
# (+- 1.96 s, alfa = 0.05, assumed normality)

dfCentersIsd <- dfCentersDist %>% 
  pivot_longer(2:6, names_to="factor") %>%
  group_by(cluster, factor) %>% 
  summarise(CI = qnorm(.975) * sd(value), .groups = "drop") %>% 
  arrange(cluster,factor)

dfCentersIm <- dfCentersDist %>% 
  pivot_longer(2:6, names_to="factor") %>%
  group_by(cluster, factor) %>% 
  summarise(mean(value), .groups = "drop") %>% 
  arrange(cluster,factor)

(dfCentersI <- data.frame(dfCentersIm[,1:2],
                          CI=paste(unlist(dfCentersIm[,3]),"\u00b1",
                                   unlist(dfCentersIsd[,3]))))


# Bias-corrected bootstraped intervals 
# (+- 1.96 s, alfa = 0.05, assumed normality)

dfCentersIsd <- dfCentersDist %>% 
  pivot_longer(2:6, names_to="factor") %>%
  group_by(cluster, factor) %>% 
  summarise(CI = qnorm(.975) * sd(value), .groups = "drop") %>% 
  arrange(cluster,factor)

dfCentersIm <- data.frame(cluster=1:ksel, dfCentersBest[1:5]) %>% 
  pivot_longer(2:6, names_to="factor") %>% 
  arrange(cluster,factor)

(dfCentersI <- data.frame(dfCentersIm[,1:2],
                          CI=paste(unlist(dfCentersIm[,3]),"\u00b1",
                                   unlist(dfCentersIsd[,3]))))

### 04.4 Silhouette analysis ####
## ... kmeans ###
dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(as.numeric(factor(refKm_tw$kmeans[[1]]$cluster,
                                              levels=order(centersAve))))
)

sil <- silhouette(as.numeric(dfClusterBest$cluster),
                  dist=dist(dfDataF1m),method="complete")
fviz_silhouette(sil) +
  scale_color_brewer(type="qual", palette="Dark2") +
  theme(legend.position = "none")

## ... hclust ###
dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(as.numeric(factor(refHc_sil$cluster[[1]],
                                              levels=order(centersAve))))
)

sil <- silhouette(as.numeric(dfClusterBest$cluster),
                  dist=dist(dfDataF1m),method="complete")
fviz_silhouette(sil) +
  scale_color_brewer(type="qual", palette="Dark2") +
  theme(legend.position = "none")


#### 05 VALIDATION OF INDIVIDUAL CLASSIFICATION ####
# Not to be detailed
