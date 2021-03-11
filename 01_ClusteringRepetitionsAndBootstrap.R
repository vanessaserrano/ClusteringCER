##=====================================================##
##  CLUSTERING REPETITIONS AND BOOTSTRAP               ##
##  @authors: Vanessa Serrano, Jordi Cuadros           ##
##=====================================================##


#### 00 SYSTEM PREPARATION ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "cluster")
# cluster: "Finding Groups in Data": Cluster Analysis Extended, Rousseeuw et al. 

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

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

#### 02 CLUSTERING ####
# Let's make 1000 repetitions of selected methods
# and save them to avoid  excessive recalculations

k_range <- 2:8
# Externally decided to be between 2 and 8

## 02.1 Apriori factors, kmeans (nstart = 25) ####
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

## 02.2 Apriori factors, hclust (Ward, euclidean) ####
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

## 03 BOOTSTRAP ####
# https://sele.inf.um.es/evaluome/help.html
# A Jaccard-index mean above 0.75 should be expected

# ... a priori factors, kmeans (nstart = 25) ####
if(!file.exists("km_clust/kmF1m_ns25_bs.rda")) {
  clusterings <- data.frame(k=sort(rep(k_range,1000)),
                            i=rep(1:1000,length(k_range)),
                            kmeans=NA)
  lstKmeans<-as.list(rep(NA,1000 * length(k_range)))
  for(cl in 1:nrow(clusterings)) {
    dfDataF1m_bs <- dfDataF1m[
      sample(1:nrow(dfDataF1m),nrow(dfDataF1m),replace = TRUE),]
    lstKmeans[[cl]] <- 
      kmeans(dfDataF1m_bs, centers= clusterings$k[cl], nstart=25,
             iter.max=20)
    # print(cl)
  }
  
  clusterings$kmeans <- lstKmeans
  str(clusterings$kmeans[1])
  
  clusterings$totwss <- sapply(clusterings$kmeans,
                               function(x) x$tot.withinss)
  clusterings$bss <- sapply(clusterings$kmeans,
                            function(x) x$betweenss)
  save(clusterings, file="km_clust/kmF1m_ns25_bs.rda")
}

# ... a priori factors, hclust (Ward, euclidean) ####
if(!file.exists("km_clust/hF1m_W_euc_bs.rda")) {
  clusterings <- data.frame(i=1:1000,
                            ordering = NA,
                            hc=NA)
  lstOrder <- as.list(rep(NA,1000))
  lstHC<-as.list(rep(NA,1000))
  for(cl in 1:nrow(clusterings)) {
    lstOrder[[cl]] <- sample(1:nrow(dfDataF1m),
                             nrow(dfDataF1m),replace=T) 
    df1 <- dfDataF1m[lstOrder[[cl]],]
    distData <- dist(df1, method="euclidean")
    lstHC[[cl]] <- 
      agnes(distData, diss=T, method="ward")
  }
  
  clusterings$ordering <- lstOrder
  clusterings$hc <- lstHC
  str(clusterings$hc[1])
  
  save(clusterings, file="km_clust/hF1m_W_euc_bs.rda")
}

