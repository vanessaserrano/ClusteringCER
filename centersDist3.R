options(install.packages.check.source = "no")
pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "factoextra",
         "NbClust", "mclust", "cluster")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

dfData <- read.table("data/ICI.tsv",sep="\t",header=T)
dfDataG <- dfData[,22:41]

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

selk <- 3
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
