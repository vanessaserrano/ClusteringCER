##=====================================================##
##  CLUSTERING IN CER                                  ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##

#### 00 DATA PREPARATION ####
### 00.1 Package management ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "factoextra",
         "NbClust", "mclust", "cluster", "flexclust", "psych",
         "corrplot", "polycor", "philentropy", "arules",
         "GGally", "ggExtra", "ggalt")
# factoextra: Extract and Visualize the Results of Multivariate Data Analyses
# NbClust: determining the best number of clusters
# mclust: Gaussian Mixture Modelling for Model-Based Clustering, 
#       Classification, and Density Estimation 
# cluster: "Finding Groups in Data": Cluster Analysis Extended, Rousseeuw et al. 
# flexclust: Flexible Cluster Algorithms (includes kcca function)
# philentropy: Similarity and Distance Quantification Between Probability Functions

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
vecPatternG <- apply(dfDataG,1,paste, collapse="")

tabPatternG <- table(vecPatternG)
tabPatternG <- sort(tabPatternG,decreasing = T)

tabPatternG[1:10]

## 01.1.3 Students by factors ####
# ... a priori factors ####
dfDataF1s <- dfDataG
dfDataF1s <- dfDataF1s %>% mutate(
  SPR = cQ01 + cQ02 + cQ06 + cQ07 + cQ10 + cQ14 + cQ18 + cQ19,
  MT = cQ05 + cQ08 + cQ12 + cQ15,
  EI = cQ03 + cQ11 + cQ16 + cQ20,
  CC = cQ04 + cQ09 + cQ13 + cQ17) %>% select(SPR,MT,EI,CC)
summary(dfDataF1s)

dfDataF1m <- dfDataF1s
dfDataF1m$SPR <- dfDataF1m$SPR / 8
dfDataF1m[,c(2:4)] <- dfDataF1m[,c(2:4)] / 4

summary(dfDataF1m)

alpha(dfDataG)
alpha(dfDataG[,c("cQ01","cQ02","cQ06","cQ07",
                 "cQ10","cQ14","cQ18","cQ19")])
alpha(dfDataG[,c("cQ05","cQ08","cQ12","cQ15")])
alpha(dfDataG[,c("cQ03","cQ11","cQ16","cQ20")])
alpha(dfDataG[,c("cQ04","cQ09","cQ13","cQ17")])

omega(dfDataG, nfactors=4, flip=FALSE, covar=F)

# ... EFA (corr) ####
corrplot(cor(dfDataG,
             use="complete.obs"),
         order="hclust",addrect=2)
corrplot(cor(dfDataG,
             use="complete.obs"),
         order="hclust",addrect=3)
corrplot(cor(dfDataG,
             use="complete.obs"),
         order="hclust",addrect=4)
corrplot(cor(dfDataG,
             use="complete.obs"),
         order="hclust",addrect=5)
corrplot(cor(dfDataG,
             use="complete.obs"),
         order="hclust",addrect=6)
corrplot(cor(dfDataG,
             use="complete.obs"),
         order="hclust",addrect=7)

# ... a posteriori factors, by PCA + Varimax ####
# https://it.unt.edu/sites/default/files/binaryfa_l_jds_sep2014_0.pdf

dfDataG_f <- sapply(dfDataG, as.factor)

# polychoric correlations
corDataG <- hetcor(dfDataG_f)$cor
round(corDataG,2)

pcDataG <- princomp(covmat = corDataG)
screeplot(pcDataG, type="lines")

faDataG <- factanal(covmat = corDataG, factors = 4,
                    rotation = "varimax")
l <- faDataG$loadings
dfLoadings <- data.frame(matrix(as.numeric(l), 
                                attributes(l)$dim,
                                dimnames=attributes(l)$dimnames))
dfLoadings[dfLoadings<.3] <- NA 
maxLoadings <- apply(dfLoadings, 1, max, na.rm=T)

dfLoadings[dfLoadings!=maxLoadings] <- NA

(f1names <- rownames(dfLoadings)[!is.na(dfLoadings[,1])])
(f2names <- rownames(dfLoadings)[!is.na(dfLoadings[,2])])
(f3names <- rownames(dfLoadings)[!is.na(dfLoadings[,3])])
(f4names <- rownames(dfLoadings)[!is.na(dfLoadings[,4])])

corDataG_selF2 <- hetcor(
  dfDataG_f[,c(f1names,f2names,f3names,f4names)])
corDataG_selF2 <- corDataG_selF2$cor

corrplot(corDataG_selF2, order="hclust", addrect = 4)

dfDataF2s <- data.frame(
  F1 = apply(dfDataG[,f1names],1,sum),
  F2 = apply(dfDataG[,f2names],1,sum),
  F3 = apply(dfDataG[,f3names],1,sum),
  F4 = apply(dfDataG[,f4names],1,sum)
)

dfDataF2m <- data.frame(
  F1 = apply(dfDataG[,f1names],1,mean),
  F2 = apply(dfDataG[,f2names],1,mean),
  F3 = apply(dfDataG[,f3names],1,mean),
  F4 = apply(dfDataG[,f4names],1,mean)
)

alpha(dfDataG[,c(f1names,f2names,f3names,f4names)])  

alpha(dfDataG[,f1names])  
alpha(dfDataG[,f2names])  
alpha(dfDataG[,f3names])  
alpha(dfDataG[,f4names])  

omega(dfDataG[,c(f1names,f2names,f3names,f4names)],
      nfactors=4, flip=FALSE, covar=F)


### 01.2 Distances ####
# stats::kmeans uses euclidean distance by default

## 01.2.1 Distance calculation ####
diss <- factoextra::get_dist(dfDataG, method="euclidean")

# stats::dist includes euclidean", "maximum", "manhattan",
#     "canberra", "binary" or "minkowski"
# factoextra::get_dist includes "euclidean", "maximum",
#     "manhattan", "canberra", "binary", "minkowski", 
#     "pearson", "spearman" or "kendall"
# cluster::daisy includes "euclidean", "manhattan", "gower"
# philentropy::distance includes "additive_symm", "avg", 
#     "bhattacharyya", "canberra", "chebyshev", "clark", 
#     "cosine", "czekanowski", "dice", "divergence", "euclidean",
#     "fidelity", "gower", "harmonic_mean", "hassebrook", 
#     "hellinger", "inner_product", "intersection", "jaccard", 
#     "jeffreys", "jensen_difference", "jensen-shannon", 
#     "k_divergence", "kulczynski_d", "kulczynski_s", 
#     "kullback-leibler", "kumar-johnson", "lorentzian", 
#     "manhattan", "matusita", "minkowski", "motyka", "neyman", 
#     "non-intersection", "pearson", "prob_symm", "ruzicka", 
#     "soergel", "sorensen", "squared_chi", "squared_chord", 
#     "squared_euclidean", "taneja", "tanimoto", "topsoe", "wavehedges"

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


#### 02 CLUSTERING ####
### 02.1 Clustering repetitions ####
# Let's make 1000 repetitions of selected methods
# and save them to avoid  excessive recalculations

k_range <- 2:8
# Externally decided to be between 2 and 8

## 02.1.1 Gradings, kmeans (nstart = 1) ####
if(!file.exists("km_clust/kmG_ns1.rda")) {
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
  save(clusterings, file="km_clust/kmG_ns1.rda")
}

## 02.1.2 Gradings, kmeans (nstart = 25) ####
if(!file.exists("km_clust/kmG_ns25.rda")) {
  clusterings <- data.frame(k=sort(rep(k_range,1000)),
                            i=rep(1:1000,length(k_range)),
                            kmeans=NA)
  lstKmeans<-as.list(rep(NA,1000 * length(k_range)))
  for(cl in 1:nrow(clusterings)) {
    lstKmeans[[cl]] <- 
      kmeans(dfDataG, centers= clusterings$k[cl], nstart=25,
             iter.max=20)
    # print(cl)
  }
  
  clusterings$kmeans <- lstKmeans
  str(clusterings$kmeans[1])
  
  clusterings$totwss <- sapply(clusterings$kmeans,
                               function(x) x$tot.withinss)
  clusterings$bss <- sapply(clusterings$kmeans,
                            function(x) x$betweenss)
  save(clusterings, file="km_clust/kmG_ns25.rda")
}

## 02.1.3 Gradings, hclust (Ward, dice) ####
if(!file.exists("km_clust/hG_W_dice.rda")) {
  clusterings <- data.frame(i=1:1000,
                            ordering = NA,
                            hc=NA)
  lstOrder <- as.list(rep(NA,1000))
  lstHC<-as.list(rep(NA,1000))
  for(cl in 1:nrow(clusterings)) {
    print(cl)
    lstOrder[[cl]] <- sample(1:nrow(dfDataG),nrow(dfDataG)) 
    df1 <- dfDataG[lstOrder[[cl]],]
    distData <- dissimilarity(as.matrix(df1), method="dice")
    lstHC[[cl]] <- 
      agnes(distData, diss=T, method="ward")
  }
  
  clusterings$ordering <- lstOrder
  clusterings$hc <- lstHC
  str(clusterings$hc[1])
  
  save(clusterings, file="km_clust/hG_W_dice.rda")
}

## 02.1.4 Apriori factors, kmeans (nstart = 1) ####
if(!file.exists("km_clust/kmF1m_ns1.rda")) {
  clusterings <- data.frame(k=sort(rep(k_range,1000)),
                            i=rep(1:1000,length(k_range)),
                            kmeans=NA)
  lstKmeans<-as.list(rep(NA,1000 * length(k_range)))
  for(cl in 1:nrow(clusterings)) {
    lstKmeans[[cl]] <- 
      kmeans(dfDataF1m, centers= clusterings$k[cl], nstart=1,
             iter.max=20)
    # print(cl)
  }
  
  clusterings$kmeans <- lstKmeans
  str(clusterings$kmeans[1])
  
  clusterings$totwss <- sapply(clusterings$kmeans,
                               function(x) x$tot.withinss)
  clusterings$bss <- sapply(clusterings$kmeans,
                            function(x) x$betweenss)
  save(clusterings, file="km_clust/kmF1m_ns1.rda")
}

## 02.1.5 Apriori factors, kmeans (nstart = 25) ####
if(!file.exists("km_clust/kmF1m_ns25.rda")) {
  clusterings <- data.frame(k=sort(rep(k_range,1000)),
                            i=rep(1:1000,length(k_range)),
                            kmeans=NA)
  lstKmeans<-as.list(rep(NA,1000 * length(k_range)))
  for(cl in 1:nrow(clusterings)) {
    lstKmeans[[cl]] <- 
      kmeans(dfDataF1m, centers= clusterings$k[cl], nstart=25,
             iter.max=20)
    # print(cl)
  }
  
  clusterings$kmeans <- lstKmeans
  str(clusterings$kmeans[1])
  
  clusterings$totwss <- sapply(clusterings$kmeans,
                               function(x) x$tot.withinss)
  clusterings$bss <- sapply(clusterings$kmeans,
                            function(x) x$betweenss)
  save(clusterings, file="km_clust/kmF1m_ns25.rda")
}

## 02.1.6 Apriori factors, hclust (Ward, euclidean) ####
if(!file.exists("km_clust/hF1m_W_euc.rda")) {
  clusterings <- data.frame(i=1:1000,
                            ordering = NA,
                            hc=NA)
  lstOrder <- as.list(rep(NA,1000))
  lstHC<-as.list(rep(NA,1000))
  for(cl in 1:nrow(clusterings)) {
    lstOrder[[cl]] <- sample(1:nrow(dfDataF1m),nrow(dfDataF1m)) 
    df1 <- dfDataF1m[lstOrder[[cl]],]
    distData <- dist(df1, method="euclidean")
    lstHC[[cl]] <- 
      agnes(distData, diss=T, method="ward")
  }
  
  clusterings$ordering <- lstOrder
  clusterings$hc <- lstHC
  str(clusterings$hc[1])
  
  save(clusterings, file="km_clust/hF1m_W_euc.rda")
}

## 02.1.7 Mona, gradings ####
# Relevant for dichotomous data but uncommon

dfMona <- dfDataG
colnames(dfMona) <- substr(colnames(dfMona),2,4)
monaClustering <- mona(dfMona)
plot(monaClustering)

# From https://www.reddit.com/r/rstats/comments/1rj7fr/clustering_using_mona_from_cluster_package/cdoiyym/
divider <- function(test.sample, mona.obj, k){
  ## Create an empty vector in which the results are added one by one
  clusters = c()
  
  ## Divide the data row by row
  for(j in 1:nrow(test.sample)){
    
    ## Pick up the necessary data from the mona object and put them 
    ## in vectors we can later alter
    steps = mona.obj$step
    variable = mona.obj$variable
    order.pat = mona.obj$order
    cluster = 1
    ## Performs a step by step division of the unclustered data
    for(i in 1:k){ 
      
      indx = which(steps == i)
      varia = variable[indx]
      modulator = mona.obj$data[mona.obj$order[indx],varia]
      
      ## Security feature
      if(!is.matrix(modulator)){
        
        ## Cuts the vectors in half, depending on the route the division takes.
        ## This is for the left half
        if(test.sample[j, varia] == modulator){
          steps = steps[1:indx]
          variable = variable[1:indx]
          order.pat = order.pat[1:indx]}
        
        ## This is for the right half. Every time a right turn is made in the tree, a value is added to 
        ## the cluster, depending on the step. 
        if(test.sample[j, varia] != modulator){
          steps = steps[indx:length(steps)]
          variable = variable[indx:length(variable)]
          order.pat = order.pat[indx:length(order.pat)]
          ## Formula for the addition. Basically, the lower the right turn in the tree, the lower the added score
          cluster = cluster + (2^k)*((1/2)^i)
        }
      }
    }
    clusters = append(clusters, cluster)
  }
  return(clusters)
}

clusters <- divider(dfMona,monaClustering,3) # 2: four clusters, 3: eight clusters
(centers <- data.frame(dfMona,cluster=clusters) %>% group_by(cluster) %>% 
    summarise(across(starts_with("Q"), mean)))


### 02.2 Optimal number of clusters ####
## 02.2.1 Students by gradings, kmeans ####
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

# Considering repetitions
load("km_clust/kmG_ns25.rda")

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


# Curvature method


# silhouette with repetitions



## 02.2.2 Students by factors, kmeans ####
# ... a priori factors, mean ####
NbClust(dfDataF1m, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "kmeans", index="all")

# * Among all indices:                                                
# * 8 proposed 2 as the best number of clusters 
# * 4 proposed 3 as the best number of clusters 
# * 1 proposed 5 as the best number of clusters 
# * 9 proposed 6 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 

## 02.2.3 Students by factors, hclust, Ward's method ####
# ... a priori factors, mean ####
NbClust(dfDataF1m, distance="euclidean", min.nc = min(k_range),
        max.nc = max(k_range),
        method = "ward.D2", index="all")

# * Among all indices:                                                
# * 5 proposed 2 as the best number of clusters 
# * 3 proposed 3 as the best number of clusters 
# * 6 proposed 4 as the best number of clusters 
# * 1 proposed 5 as the best number of clusters 
# * 4 proposed 6 as the best number of clusters 
# * 2 proposed 7 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 


#### 03 INTERNAL VALIDATION ####
### 03.1 Sense-making ####
## 03.1.1 For the research team. Ability to explain ####

## 03.1.2 Experts based ####



### 03.2 Validation of groups ####
## 03.2.1 Repeatability: Stochastic methods, Deterministic methods, Ensemble methods, Choosing an optimal solution ####
# ... Stochastic methods: kmeans, gradings ####
vecClustFilenames <- c("km_clust/kmG_ns1.rda",
                       "km_clust/kmG_ns25.rda") 

for(clustFN in vecClustFilenames) {
  load(clustFN)
  for(selk in k_range) {
    sel <- clusterings$kmeans[clusterings$k==selk]
    
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
      pivot_longer(.,1:(ncol(.)-1), names_to = "question", values_to = "mean")
    
    dfBest <- as.data.frame(sel[[which.min(sapply(sel,
                                                  function(x) x$tot.withinss))]]$centers)
    
    dfBest$cluster <- row.names(dfBest) 
    
    dfBestL <- dfBest %>% 
      pivot_longer(.,1:(ncol(.)-1), names_to = "question", values_to = "mean")
    
    dfBestL$cluster <- as.factor(dfBestL$cluster)
    
    print(paste("===",selk,"==="))
    
    # count repetitions which match best
    # ... by matching sorting coordinated of the centers
    coordsBest <- sort(unlist(dfBest[,-ncol(dfBest)]))
    matches <- logical(length(sel))
    for(i in seq(length(sel))) {
      coords <- sort(unlist(dfCenters[dfCenters$i==i,-ncol(dfCenters)]))
      matches[i] <- all(coords==coordsBest)
    }
    print(table(matches))
    
    # ... by matching partition
    partBest <- sel[[which.min(sapply(sel,
                                      function(x) x$tot.withinss))]]$cluster
    cluOrder <- unique(partBest)
    
    # map partition by order
    partBest <- as.numeric(factor(partBest,
                                  levels=cluOrder))
    matches <- logical(length(sel))
    ari <- numeric(length(sel))
    for(i in seq(length(sel))) {
      part1 <- sel[[i]]$cluster
      cluOrder <- unique(part1)
      part1 <- as.numeric(factor(part1,
                                 levels=cluOrder))
      matches[i] <- all(part1==partBest)
      ari[i] <- adjustedRandIndex(part1,partBest)
    }
    print(table(matches))
    
    # visualize ARI distribution
    print(ggplot(NULL) +
            geom_density(aes(x=ari)) +
            scale_x_continuous(limits=c(0,1))+
            theme_classic() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.y = element_blank()))
    
    # visualize center distribution
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
}

# ... Stochastic methods: kmeans, a priori factors ####
vecClustFilenames <- c("km_clust/kmF1m_ns1.rda",
                       "km_clust/kmF1m_ns25.rda") 

for(clustFN in vecClustFilenames) {
  load(clustFN)
  for(selk in k_range) {
    sel <- clusterings$kmeans[clusterings$k==selk]
    
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
    
    dfBest <- as.data.frame(sel[[which.min(sapply(sel,
                                                  function(x) x$tot.withinss))]]$centers)
    
    dfBest$cluster <- row.names(dfBest) 
    
    dfBestL <- dfBest %>% 
      pivot_longer(.,1:(ncol(.)-1), names_to = "factor", values_to = "mean")
    
    dfBestL$cluster <- as.factor(dfBestL$cluster)
    
    print(paste("===",selk,"==="))
    
    # count repetitions which match best
    # ... by matching sorting coordinated of the centers
    coordsBest <- sort(unlist(dfBest[,-ncol(dfBest)]))
    matches <- logical(length(sel))
    for(i in seq(length(sel))) {
      coords <- sort(unlist(dfCenters[dfCenters$i==i,-ncol(dfCenters)]))
      matches[i] <- all(coords==coordsBest)
    }
    print(table(matches))
    
    # ... by matching partition
    partBest <- sel[[which.min(sapply(sel,
                                      function(x) x$tot.withinss))]]$cluster
    cluOrder <- unique(partBest)
    
    # map partition by order
    partBest <- as.numeric(factor(partBest,
                                  levels=cluOrder))
    matches <- logical(length(sel))
    ari <- numeric(length(sel))
    for(i in seq(length(sel))) {
      part1 <- sel[[i]]$cluster
      cluOrder <- unique(part1)
      part1 <- as.numeric(factor(part1,
                                 levels=cluOrder))
      matches[i] <- all(part1==partBest)
      ari[i] <- adjustedRandIndex(part1,partBest)
    }
    print(table(matches))
    
    # visualize ARI distribution
    print(ggplot(NULL) +
            geom_density(aes(x=ari)) +
            scale_x_continuous(limits=c(0,1))+
            theme_classic() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.y = element_blank()))
    
    # visualize center distribution
    print(ggplot(dfCentersL, aes(x=mean, y=1)) + 
            geom_vline(aes(color=cluster,xintercept=mean),
                       data=dfBestL, size=1.5) +
            geom_jitter(width=0, height=0.4, shape=21,
                        alpha=0.1, size=1) +
            scale_y_continuous(limits=c(.5,1.5))+
            facet_wrap(~factor) +
            theme_classic() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.y = element_blank()))
    
    print(ggplot(dfCentersL, aes(x=mean)) + 
            geom_vline(aes(color=cluster,xintercept=mean),
                       data=dfBestL, size=1.5) +
            geom_density() +
            facet_wrap(~factor, scales="free_y") +
            theme_classic() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.y = element_blank()))
  }
}

## 03.2.2 Silhouette analysis ####
# A mean value above 0.5 should be expected according to
# Leonard Kaufman; Peter J. Rousseeuw (1990). Finding groups in data : An introduction to cluster analysis

# Attention to http://www.amse-conference.eu/old/2018/wp-content/uploads/2018/10/%C5%98ezankov%C3%A1.pdf

# ... Gradings, kmeans, euclidean ####
load("km_clust/kmG_ns25.rda")

for(selk in k_range) {
  print(paste("===",selk,"==="))
  sel <- clusterings$kmeans[clusterings$k==selk]
  selClustering <- sel[[which.min(sapply(sel,
                                         function(x) x$tot.withinss))]]
  sil <- silhouette(selClustering$cluster,
                    dist=dist(dfDataG),method="complete")
  print(fviz_silhouette(sil) + theme(legend.position = "none"))
  
  print(table(sil[,3]>0, useNA = "always"))
  print(table(sil[,3]>0, useNA = "always") * 100 / length(sil[,3]))
}

# ... A priori factors, kmeans ####
load("km_clust/kmF1m_ns25.rda")

for(selk in k_range) {
  print(paste("===",selk,"==="))
  sel <- clusterings$kmeans[clusterings$k==selk]
  selClustering <- sel[[which.min(sapply(sel,
                                         function(x) x$tot.withinss))]]
  sil <- silhouette(selClustering$cluster,
                    dist=dist(dfDataF1m),method="complete")
  print(fviz_silhouette(sil) + theme(legend.position = "none"))
  
  print(table(sil[,3]>0, useNA = "always"))
  print(table(sil[,3]>0, useNA = "always") * 100 / length(sil[,3]))
}

## 03.2.3 Bootstrap ####
# https://sele.inf.um.es/evaluome/help.html
# A Jaccard-index mean above 0.75 should be expected


### 03.3 Validation of an individual classification ####
## 03.3.1 Silhouette index ####
## 03.3.2 Probability of being in a group ####

#### 04 EXTERNAL VALIDATION ####
### 04.1 Classification coherence indices ####


#### 05 VISUALIZATION ####
### 05.1 Centers (To distinguish the clusters) ####
## 05.1.1 Heatmap ####

# ... kmeans (4 clusters), euclidean distance ####
# Used an arbitrary number of clusters
load("km_clust/kmF1m_ns25.rda")

sel <- clusterings$kmeans[clusterings$k==4]
clusterG <- sel[[which.min(sapply(sel,
                                  function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterG$centers)
centersAve <- apply(dfMeans,1,mean)

dfMeans$cluster <- as.numeric(factor(1:4,levels=order(centersAve))) #rownames(dfMeans)

dfMeansL <- pivot_longer(dfMeans,1:4,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_viridis_b(option="magma") + theme_bw()


# ... kmeans (8 clusters), euclidean distance ####
# Used an arbitrary number of clusters
sel <- clusterings$kmeans[clusterings$k==8]
clusterG <- sel[[which.min(sapply(sel,
                                  function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterG$centers)
centersAve <- apply(dfMeans,1,mean)

dfMeans$cluster <- as.numeric(factor(1:8,levels=order(centersAve))) 

dfMeansL <- pivot_longer(dfMeans,1:4,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_viridis_b(option="magma") + theme_bw()


# ... kmeans (4 clusters), euclidean distance ####
# Used an arbitrary number of clusters
load("km_clust/kmG_ns25.rda")

sel <- clusterings$kmeans[clusterings$k==4]
clusterG <- sel[[which.min(sapply(sel,
                                  function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterG$centers)
dfMeans$cluster <- rownames(dfMeans)
dfMeansL <- pivot_longer(dfMeans,1:20,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_viridis_b(option="magma") + theme_bw()

# ... kmeans (5 clusters), euclidean distance ####
# Used an arbitrary number of clusters
sel <- clusterings$kmeans[clusterings$k==5]
clusterG <- sel[[which.min(sapply(sel,
                                  function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterG$centers)
dfMeans$cluster <- rownames(dfMeans)
dfMeansL <- pivot_longer(dfMeans,1:20,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=cluster,y=question,fill=mean)) +
  geom_tile() + scale_fill_viridis_b(option="magma") + theme_bw()

## 05.1.2 Line ####
# ... kmeans (4 clusters), euclidean distance ####
sel <- clusterings$kmeans[clusterings$k==4]
clusterG <- sel[[which.min(sapply(sel,
                                  function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterG$centers)
dfMeans$cluster <- rownames(dfMeans)
dfMeansL <- pivot_longer(dfMeans,1:20,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=question,y=mean, color=cluster, group=cluster)) +
  facet_grid(cluster ~ .) + geom_line(size=1) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size=7))

# ... kmeans (5 clusters), euclidean distance ####
sel <- clusterings$kmeans[clusterings$k==5]
clusterG <- sel[[which.min(sapply(sel,
                                  function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterG$centers)
dfMeans$cluster <- rownames(dfMeans)
dfMeansL <- pivot_longer(dfMeans,1:20,names_to="question", values_to = "mean")

ggplot(dfMeansL,aes(x=question,y=mean, color=cluster, group=cluster)) +
  facet_grid(cluster ~ .) + geom_line(size=1) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size=7))


### 05.2 Individual observations ####
## 05.2.1 Nominal data (gradings) ####
sel <- clusterings$kmeans[clusterings$k==4]
clusterBest <- sel[[which.min(sapply(sel,
                                     function(x) x$tot.withinss))]]

dfClusterBest <- data.frame(
  dfDataG, cluster=clusterBest$cluster
)

dfClusterBestL <- pivot_longer(dfClusterBest,1:20,
                               names_to="question", 
                               values_to = "answer")
dfClusterBestL$answer <- as.factor(dfClusterBestL$answer)
dfClusterBestL$cluster <- as.factor(dfClusterBestL$cluster)

tabClusterQuestionAnswer <- table(dfClusterBestL$cluster,
                                  dfClusterBestL$question,
                                  dfClusterBestL$answer)
tabCluster <- table(dfClusterBestL$cluster) / 
  length(unique(dfClusterBestL$question))
sum(tabCluster)

dfCQA <- as.data.frame(tabClusterQuestionAnswer)
colnames(dfCQA) <- c("cluster","question","answer", "frequency")
dfCQA$frequency <- as.numeric(dfCQA$frequency / tabCluster[dfCQA$cluster])
dfCQA$question <- substr(dfCQA$question,2,4)

ggplot(dfCQA, aes(x=answer, y=frequency, fill=cluster)) +
  geom_bar(position = position_dodge2(reverse=T),color="black",
           stat="identity")+
  coord_flip() +
  facet_wrap(~question)+
  theme_classic()+
  scale_fill_brewer(type="qual")+
  scale_y_continuous(breaks=seq(0,1,.5))

## 05.2.2 Continuous data (factors) ####
load("km_clust/kmF1m_ns25.rda")

sel <- clusterings$kmeans[clusterings$k==4]
clusterBest <- sel[[which.min(sapply(sel,
                                     function(x) x$tot.withinss))]]

dfMeans <- as.data.frame(clusterBest$centers)
centersAve <- apply(dfMeans,1,mean)

dfMeans$cluster <- factor(as.numeric(factor(1:4,levels=order(centersAve)))) #rownames(dfMeans)

dfMeansL <- pivot_longer(dfMeans,1:4,names_to="question", values_to = "mean")


dfClusterBest <- data.frame(
  dfDataF1m, cluster=factor(as.numeric(factor(clusterBest$cluster,
                                              levels=order(centersAve))))
)

ggpairs(dfClusterBest[,1:4], 
        diag="blankDiag",
        mapping=ggplot2::aes(color=dfClusterBest$cluster),
        lower=list(continuous=
                     wrap("points",alpha=.2, position=position_jitter())))+
  scale_fill_brewer(type="qual")+
  theme_classic()

for(i in seq(1,ncol(dfClusterBest)-2)) {
  for(j in seq(i+1,ncol(dfClusterBest)-1)) {
    print(ggplot(dfClusterBest,aes_string(x=colnames(dfClusterBest)[i],
                                          y=colnames(dfClusterBest)[j],
                                          color="cluster"))+
            geom_encircle() +
            geom_jitter(shape=21, alpha=.8)+
            geom_point(data=dfMeans, size=20, shape="+")+
            scale_color_brewer(type="qual", palette="Dark2")+
            theme_classic())
  }
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
