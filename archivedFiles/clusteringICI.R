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
# stats::kmeans uses euclidean distance by default

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

### 01.5 Optimal number of clusters ####
k_range <- c(2,8)

# Externally decided to be between 2 and 8
NbClust(dfDataG, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "kmeans", index="all")

# * Among all indices:                                                
# * 9 proposed 2 as the best number of clusters 
# * 9 proposed 3 as the best number of clusters 
# * 1 proposed 4 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 1 proposed 7 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 


# fviz_nbclust uses euclidean distance by default
factoextra::fviz_nbclust(dfDataG, kmeans, method = "wss", 
                         k.max = max(k_range), nstart = 10) 
# >>> optimal ~ 4-5
factoextra::fviz_nbclust(dfDataG, kmeans, method = "silhouette", 
                         k.max = max(k_range), nstart = 10) 
# >>> optimal ~ 2-4
factoextra::fviz_nbclust(dfDataG, kmeans, method = "gap_stat", 
                          k.max = max(k_range), nstart = 10, iter.max = 20,
                          nboot = 50)
# gap statistic method uses bootstrap, iter.max is increased
# to avoid no-convergence warnings
# >>> optimal ~ 5-6


# Adding repetitions
clusterings <- data.frame(k=sort(rep(k_range,1000)),
                          i=rep(1:1000,length(k_range)),
                          kmeans=NA)
lstKmeans<-as.list(rep(NA,1000 * length(k_range)))
for(cl in 1:nrow(clusterings)) {
  lstKmeans[[cl]] <- 
    kmeans(dfDataG, centers= clusterings$k[cl], nstart=1,
           iter.max=20)
  # print(cl)
}

clusterings$kmeans <- lstKmeans
str(clusterings$kmeans[1])

clusterings$totwss <- sapply(clusterings$kmeans,
                             function(x) x$tot.withinss)
clusterings$bss <- sapply(clusterings$kmeans,
                          function(x) x$betweenss)

clust_summ <- clusterings %>% group_by(k) %>% 
  summarise(mintotwss=min(totwss),mediantotwss=median(totwss),
            maxbss=max(bss), medianbss=median(bss)) 

ggplot(clusterings, aes(x=k, y=totwss)) +
  geom_jitter(size=1, shape=21, width=.2, alpha=0.2) +
  geom_line(aes(y=mintotwss), data=clust_summ, color="blue") +
  geom_point(aes(y=mintotwss), data=clust_summ, color="blue") +
  geom_line(aes(y=mediantotwss), data=clust_summ, color="red") +
  geom_point(aes(y=mediantotwss), data=clust_summ, color="red") +
  theme_classic() +
  scale_x_continuous(breaks=min(k_range):max(k_range))
# >>> results seem to converge for k=5-6
# tot.withinss should be between point and centers of their cluster  
# betweenss should be ss between centers and grand average


ggplot(clusterings, aes(x=k, y=bss)) +
  geom_jitter(size=1, shape=21, width=.2, alpha=0.2) +
  geom_line(aes(y=maxbss), data=clust_summ, color="blue") +
  geom_point(aes(y=maxbss), data=clust_summ, color="blue") +
  geom_line(aes(y=medianbss), data=clust_summ, color="red") +
  geom_point(aes(y=medianbss), data=clust_summ, color="red") +
  theme_classic() +
  scale_x_continuous(breaks=min(k_range):max(k_range))

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







#### 02 INTERNAL VALIDATION ####
### 02.1 Sense-making ####
## 02.1.1 For the research team. Ability to explain ####

# ... kmeans (4 clusters), euclidean distance ####
# Used an arbitrary number of clusters
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


## 02.1.2 Experts based ####

### 02.2 Validation of groups ####
## 02.2.1 Repeatability: Stochastic methods, Deterministic methods, Ensemble methods, Choosing an optimal solution ####
# ... Stochastic methods: kmeans, gradings ####
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



## 02.2.2 Silhouette analysis ####
### 02.3 Validation of an individual classification ####
## 02.3.1 Silhouette index ####
## 02.3.2 Probability of being in a group ####
#### 03 EXTERNAL VALIDATION ####
### 03.1 Classification coherence indices ####


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
