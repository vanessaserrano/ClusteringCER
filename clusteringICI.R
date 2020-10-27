#### CLUSTERING IN CER

options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "ggthemes","RColorBrewer", "factoextra")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

dfData <- read.table("data/ICI.tsv",sep="\t",header=T)

# 1-tier concept inventory
# Gender: 1-Female, 2-Male, 3-Other
# Q01-Q20: Nominal response to the question (1-4), 
#     multiple-choice (select one), 4 answers per question
# cQ01-cQ20: Grading (0, incorrect or 1, correct)

# Only complete response. No missing data

dim(dfData)
summary(dfData)

#### CLUSTERING STUDENTS BY RESPONSES ####
dfDataR <- dfData[,2:21]

#### CLUSTERING STUDENTS BY GRADINGS ####
dfDataG <- dfData[,22:41]

### kmeans ####
## .Example (with 4 clusters) ####
# stats::kmeans uses euclidean distance
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

factoextra::fviz_nbclust(dfDataG, kmeans, method = "wss", 
                         k.max = 20, nstart = 10) 
# optimal ~ 5
factoextra::fviz_nbclust(dfDataG, kmeans, method = "silhouette", 
                         k.max = 20, nstart = 10) 
# optimal ~ 5-6
factoextra::fviz_nbclust(dfDataG, kmeans, method = "gap_stat", 
                         k.max = 20, nstart = 10)
# optimal ~ 4-6

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
