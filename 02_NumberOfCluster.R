##=====================================================##
##  ANALYSIS OF THE OPTIMUM NUMBER OF CLUSTERS         ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##

# This script includes the methods that can used
# to choose the best number of clusters.
# Functions in the NbClust package and in the factoextra
# are directly used on the data set and additional 
# graphical methods are applied to the repetitions of
# the clusterings (kmeans and agnes).


#### 00 SYSTEM PREPARATION ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "cluster", "factoextra", "NbClust")
# factoextra: Extract and Visualize the Results of Multivariate Data Analyses
# cluster: "Finding Groups in Data": Cluster Analysis Extended, Rousseeuw et al. 
# NbClust: Determining the Best Number of Clusters in a Data Set

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
NbClust(dfDataF1m, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "kmeans", index="all")

factoextra::fviz_nbclust(dfDataF1m, kmeans, method = "wss", 
                         k.max = max(k_range), nstart = 10) 
factoextra::fviz_nbclust(dfDataF1m, kmeans, method = "silhouette", 
                         k.max = max(k_range), nstart = 10) 
factoextra::fviz_nbclust(dfDataF1m, kmeans, method = "gap_stat", 
                         k.max = max(k_range), nstart = 10, iter.max = 20,
                         nboot = 50)

# kmF1m_ns25: kmeans on 'a priori' factors taken as means,
#             25 internal repetitions (starting centers-partition, nstart=25)
#             1000 external repetitions for k = 2:8

load("km_clust/kmF1m_ns25.rda")

clusterings$sil <- apply(clusterings, 1,
                           function(x) {
                             sil <- silhouette(x$kmeans$cluster,
                                               dist=disF1m,method="complete")
                             mean(sil[,3])
                           })

clust_summ <- clusterings %>% group_by(k) %>% 
  summarise(mintotwss=min(totwss),mediantotwss=median(totwss),
            maxbss=max(bss), medianbss=median(bss),
            maxsil=max(sil),mediansil=median(sil)) 

# Plotting the minimum WSS for each number of clusters
ggplot(clusterings, aes(x=k, y=totwss)) +
  geom_jitter(size=1, shape=21, width=.2, alpha=0.2) +
  geom_line(aes(y=mintotwss), data=clust_summ, color="blue") +
  geom_point(aes(y=mintotwss), data=clust_summ, color="blue") +
  theme_classic() +
  scale_x_continuous(breaks=min(k_range):max(k_range))

# Plotting the maximum silhouette for each number of clusters
ggplot(clusterings, aes(x=k, y=sil)) +
  geom_jitter(size=1, shape=21, width=.2, alpha=0.2) +
  geom_line(aes(y=maxsil), data=clust_summ, color="blue") +
  geom_point(aes(y=maxsil), data=clust_summ, color="blue") +
  geom_line(aes(y=mediansil), data=clust_summ, color="red") +
  geom_point(aes(y=mediansil), data=clust_summ, color="red") +
  theme_classic() +
  scale_x_continuous(breaks=min(k_range):max(k_range))


### 02.2 Hierarchical clustering ####
NbClust(dfDataF1m, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "ward.D2", index="all")

factoextra::fviz_nbclust(dfDataF1m, hcut, method = "wss", 
                         k.max = max(k_range), nstart = 10) 
factoextra::fviz_nbclust(dfDataF1m, hcut, method = "silhouette", 
                         k.max = max(k_range), nstart = 10) 
factoextra::fviz_nbclust(dfDataF1m, hcut, method = "gap_stat", 
                         k.max = max(k_range), nstart = 10, iter.max = 20,
                         nboot = 50)

# hF1m_W_euc: hierarchical clustering (AGNES), euclidean distance,
#             Ward's aggregation method on 'a priori' factors taken as means,
#             1000 repetitions

load("km_clust/hF1m_W_euc.rda")

for(ksel in k_range) {
  vecSil <- numeric(1000)
  
  for(i in clusterings$i) {
    cl1 <- cutree(clusterings$hc[[i]],ksel)
    cl1 <- cl1[order(clusterings$ordering[[i]])]
    vecPartition <- as.numeric(factor(cl1,levels=unique(cl1)))
    vecSil[i] <- mean(silhouette(vecPartition,
                            dist=disF1m,method="complete")[,3])
  }
  clusteringsK <- clusterings
  clusteringsK$k <- ksel
  clusteringsK$sil <- vecSil
  clusteringsK$ordering <- NULL
  clusteringsK$hc <- NULL
  
  if(ksel==min(k_range)){
    clusteringsT <- clusteringsK
  } else {
    clusteringsT <- rbind(clusteringsT, clusteringsK)
  }
}

clust_summ <- clusteringsT %>% group_by(k) %>% 
  summarise(maxsil=max(sil),mediansil=median(sil)) 

# Plotting the maximum silhouette for each number of clusters
ggplot(clusteringsT, aes(x=k, y=sil)) +
  geom_jitter(size=1, shape=21, width=.2, alpha=0.2) +
  geom_line(aes(y=maxsil), data=clust_summ, color="blue") +
  geom_point(aes(y=maxsil), data=clust_summ, color="blue") +
  geom_line(aes(y=mediansil), data=clust_summ, color="red") +
  geom_point(aes(y=mediansil), data=clust_summ, color="red") +
  theme_classic() +
  scale_x_continuous(breaks=min(k_range):max(k_range))

# The plot for minimum WSS has not been produced because WSS is not
# readily available in the agnes/cutree results. It could be obtained
# through the factoextra package.
 
