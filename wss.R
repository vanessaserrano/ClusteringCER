# data <- dfDataF1m
# partition <- clustering$cluster

wss <- function(data, partition) {
  glAver <- apply(data, 2, mean, na.rm=T)
  
  tss <- apply(data, 1, function(x) {
    sum((x-glAver)^2)  
  })
  tss <- sum(tss)

  clAver <- data.frame(partition, data) %>% group_by(partition) %>% 
    summarise(n=n(),across(1:4,mean),.groups="drop_last")
  bss <- apply(clAver[,-(1:2)], 1, function(x) {
    sum((x-glAver)^2)  
  })
  bss <- sum(clAver[,2]*bss)
  tss-bss
} 


#### TESTING ####
options(install.packages.check.source = "no")

pckgs<-c("tidyverse")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

dfData <- read.table("data/ICI.tsv",sep="\t",header=T)

dfDataF1m <- dfData[,22:41]
dfDataF1m <- dfDataF1m %>% mutate(
  SPR = cQ01 + cQ02 + cQ06 + cQ07 + cQ10 + cQ14 + cQ18 + cQ19,
  MT = cQ05 + cQ08 + cQ12 + cQ15,
  EI = cQ03 + cQ11 + cQ16 + cQ20,
  CC = cQ04 + cQ09 + cQ13 + cQ17) %>% select(SPR,MT,EI,CC)
dfDataF1m$SPR <- dfDataF1m$SPR / 8
dfDataF1m[,c(2:4)] <- dfDataF1m[,c(2:4)] / 4

load("km_clust/kmF1m_ns25.rda")
clustering <- clusterings[2400,]$kmeans[[1]]

wss(dfDataF1m,clustering$cluster)
clustering$tot.withinss


ksel <- 5
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

wss(dfDataF1m, clusteringsHc[400,]$cluster[[1]])
