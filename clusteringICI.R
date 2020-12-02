#### ** CLUSTERING IN CER ** ####

#### 00 DATA PREPARATION ####
### 00.1 Package management ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "factoextra",
         "NbClust", "mclust", "cluster", "flexclust")
# factoextra: Extract and Visualize the Results of Multivariate Data Analyses
# NbClust: determining the best number of clusters
# mclust: Gaussian Mixture Modelling for Model-Based Clustering, 
#       Classification, and Density Estimation 
# cluster: "Finding Groups in Data": Cluster Analysis Extended, Rousseeuw et al. 
# flexclust: Flexible Cluster Algorithms (includes kcca function)

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

### 00.2 Data import ####
dfData <- read.table("data/ICI.tsv",sep="\t",header=T)

# 1-tier concept inventory
# Gender: 1-Female, 2-Male, 3-Other
# Q01-Q20: Nominal response to the question (1-4), 
#     multiple-choice (select one), 4 answers per question
# cQ01-cQ20: Grading (0, incorrect or 1, correct)

# Only complete response. No missing data

dim(dfData)
summary(dfData)



#### 01 PREVIOUS CONSIDERATIONS ####
### 01.1 Variables: selection and transformation ####

## 01.1.1 Students by responses ####
# Similarity for nominal.
# https://www.researchgate.net/publication/286927854_Similarity_Measures_for_Nominal_Variable_Clustering

dfDataR <- dfData[,2:21]

## 01.1.2 Students by gradings ####
# Data is dichotomous (0;1)
# Euclidean distance
# Manhattan distance: Is it different to euclidean in this case? 

dfDataG <- dfData[,22:41]


## 01.1.3 Factor analysis ####


### 01.2 Distances ####
## 01.2.1 Distance calculation ####
diss <- factoextra::get_dist(dfDataG, method="euclidean")

# stats::dist includes euclidean", "maximum", "manhattan",
#     "canberra", "binary" or "minkowski"
# factoextra::get_dist includes "euclidean", "maximum",
#     "manhattan", "canberra", "binary", "minkowski", 
#     "pearson", "spearman" or "kendall"

## 01.2.2 Comparing distances ####
# visualizing distance matrices 
distance1 <- get_dist(dfDataG[1:50,1:20], method = "euclidean")
fviz_dist(distance1, gradient = list(low = "yellow", high = "darkblue"))
fviz_dist(distance1^2, gradient = list(low = "yellow", high = "darkblue"))

max(c(distance1))
median(c(distance1))

distance2 <- get_dist(dfDataG[1:50,1:20], method = "manhattan")
fviz_dist(distance2, gradient = list(low = "yellow", high = "darkblue"))

max(c(distance2))
median(c(distance2))

# Manhattan distance is equal squared Euclidean distance 
# Only euclidean will be used


### 01.3 Grouping criteria ####
### 01.4 Outliers ####
#### 02 INTERNAL VALIDATION ####
### 02.1 Sense-making ####
## 02.1.1 For the research team. Ability to explain ####
## 02.1.2 Experts based ####
### 02.2 Validation of groups ####
## 02.2.1 Repeatability: Stochastic methods, Deterministic methods, Ensemble methods, Choosing an optimal solution ####
## 02.2.2 Silhouette analysis ####
### 02.3 Validation of an individual classification ####
## 02.3.1 Silhouette index ####
## 02.3.2 Probability of being in a group ####
#### 03 EXTERNAL VALIDATION ####
### 03.1 Classification coherence indices ####


################################################
####  TO ORGANIZE ####
################################################



### kmeans, euclidean distance ####
## .Example (with 4 clusters) ####
# stats::kmeans uses euclidean distance by default
clusterG <- kmeans(dfDataG, centers=4)
dfDataG$cluster <- clusterG$cluster
table(dfDataG$cluster)
clusterG$centers

dfMeans <- as.data.frame(clusterG$centers)
dfMeans$cluster <- rownames(dfMeans)
dfMeansL <- pivot_longer(dfMeans,1:20,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile()

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_gradient(low ="lightblue",high ="red") + theme_bw()

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_viridis_b(option="magma") + theme_bw()

ggplot(dfMeansL,aes(x=question,y=mean, color=cluster, group=cluster)) +
  geom_line(size=1) + theme_bw() + theme(axis.text.x = element_text(angle = 90, size=7))

ggplot(dfMeansL,aes(x=question,y=mean, color=cluster, group=cluster)) +
  facet_grid(cluster ~ .) + geom_line(size=1) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size=7))

## .Optimal number of clusters ####
NbClust(dfDataG, distance="euclidean", min.nc = 2, max.nc = 20,
        method = "kmeans", index="all")

# * Among all indices:                                                
# * 8 proposed 2 as the best number of clusters 
# * 8 proposed 3 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 2 proposed 11 as the best number of clusters 
# * 1 proposed 12 as the best number of clusters 
# * 1 proposed 14 as the best number of clusters 
# * 1 proposed 17 as the best number of clusters 
# * 1 proposed 18 as the best number of clusters 

# fviz_nbclust uses euclidean distance by default
factoextra::fviz_nbclust(dfDataG, kmeans, method = "wss", 
                         k.max = 20, nstart = 10) 
# >>> optimal ~ 5
factoextra::fviz_nbclust(dfDataG, kmeans, method = "silhouette", 
                         k.max = 20, nstart = 10) 
# >>> optimal ~ 5-6
# factoextra::fviz_nbclust(dfDataG, kmeans, method = "gap_stat", 
#                          k.max = 20, nstart = 10)
# >>> optimal ~ 4-6
# >>> Range of k for further inspection k = 2:10

# Adding repetitions
clusterings <- data.frame(k=sort(rep(2:10,10)),
                          i=rep(1:10,9),
                          kmeans=NA)
lstKmeans<-as.list(rep(NA,90))
for(cl in 1:nrow(clusterings)) {
  lstKmeans[[cl]] <- 
    kmeans(dfDataG, centers= clusterings$k[cl], nstart=1)
  print(cl)
}

clusterings$kmeans <- lstKmeans
str(clusterings$kmeans[1])



clusterings$totwss <- sapply(clusterings$kmeans,
                             function(x) x$tot.withinss)

ggplot(clusterings, aes(x=k, y=totwss)) +
  geom_point(size=3, shape=21) + theme_classic() +
  scale_x_continuous(breaks=2:10)
# >>> results seem to converge for k=5-6
# tot.withinss should be between point and centers of their cluster  
# betweenss should be ss between centers and grand average

clusterings$bss <- sapply(clusterings$kmeans,
                             function(x) x$betweenss)

ggplot(clusterings, aes(x=k, y=bss)) +
  geom_point(size=3, shape=21) + theme_classic() +
  scale_x_continuous(breaks=2:10)

summary(clusterings$totwss+clusterings$bss)



# L-method
# silhouette with repetitions



# stats::kmeans uses euclidean distance
clusterG <- kmeans(dfDataG, centers=5, nstart = 10)
dfDataG$cluster <- clusterG$cluster
table(dfDataG$cluster)
clusterG$centers

dfMeans <- as.data.frame(clusterG$centers)
dfMeans$cluster <- rownames(dfMeans)
dfMeansL <- pivot_longer(dfMeans,1:20,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile()

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_gradient(low ="lightblue",high ="red") + theme_bw()

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_viridis_b(option="magma") + theme_bw()

ggplot(dfMeansL,aes(x=question,y=mean, color=cluster, group=cluster)) +
  geom_line(size=1) + theme_bw() + theme(axis.text.x = element_text(angle = 90, size=7))

ggplot(dfMeansL,aes(x=question,y=mean, color=cluster, group=cluster)) +
  facet_grid(cluster ~ .) + geom_line(size=1) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size=7))


#### ANALYSIS OF CENTERS DISTRIBUTION (02.2.1?) ####
### Kmeans, gradings ####
clusterings <- data.frame(k=sort(rep(2:8,1000)),
                          i=rep(1:1000,7),
                          kmeans=NA)
lstKmeans<-as.list(rep(NA,7000))
for(cl in 1:nrow(clusterings)) {
  lstKmeans[[cl]] <- 
    kmeans(dfDataG, centers= clusterings$k[cl], nstart=1,
           iter.max=50)
  # print(cl)
}

clusterings$kmeans <- lstKmeans
str(clusterings$kmeans[1])

# selk <- 3
for(selk in 2:8) {
  sel <- clusterings$kmeans[clusterings$k==selk]
  
  for(i in seq(length(sel))) {
    if(i == 1) {
      dfCenters <- as.data.frame(sel[[i]]$centers)
    } else {
      dfCenters <- rbind(dfCenters,
                         as.data.frame(sel[[i]]$centers))
    }
  }
  
  dfCentersL <- dfCenters %>% 
    pivot_longer(.,1:ncol(.), names_to = "question", values_to = "mean")
  
  dfBest <- as.data.frame(sel[[which.min(sapply(sel,
                                                function(x) x$tot.withinss))]]$centers)
  
  dfBest$cluster <- row.names(dfBest) 
  
  dfBestL <- dfBest %>% 
    pivot_longer(.,1:(ncol(.)-1), names_to = "question", values_to = "mean")
  
  dfBestL$cluster <- as.factor(dfBestL$cluster)
  
  print(paste("===",selk,"==="))
  print(ggplot(dfCentersL, aes(x=mean, y=1)) + 
          geom_vline(aes(color=cluster,xintercept=mean),
                     data=dfBestL, size=1.5) +
          geom_jitter(width=0, height=0.4, shape=21,
                      alpha=0.1, size=1) +
          scale_y_continuous(limits=c(.5,1.5))+
          facet_wrap(~question) +
          theme_classic() +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank()))
  
  print(ggplot(dfCentersL, aes(x=mean)) + 
          geom_vline(aes(color=cluster,xintercept=mean),
                     data=dfBestL, size=1.5) +
          geom_density() +
          facet_wrap(~question, scales="free_y") +
          theme_classic() +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank()))
  
}


#### TO DOs ####
# TO DO: Validation of the clustering
# A low dispersion of tot.withinss among repetitions could be a
# validation criterium for the clustering

# TO DO: Error estimation for individual classifications

# TO DO: Run EFA and choose subscores from that
#        Recover from mail to Vicente

# TO DO: Add hierarchical

# TO DO: Look for additional criteria to select the optimal
#        number of clusters

# TO DO: Analyze kcca with different distances (dist matrix
# is not valid for kmeans, distance to centroid can not be
# calculated)
#  - https://stackoverflow.com/questions/7524042/how-to-specify-distance-metric-while-for-kmeans-in-r
#  - https://www.rdocumentation.org/packages/flexclust/versions/1.4-0/topics/kcca
