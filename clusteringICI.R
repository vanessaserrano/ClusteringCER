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
### Discussion ####
# Similarity for nominal.
# https://www.researchgate.net/publication/286927854_Similarity_Measures_for_Nominal_Variable_Clustering

dfDataR <- dfData[,2:21]


#### CLUSTERING STUDENTS BY GRADINGS ####
### Discussion ####
# Data is dichotomous (0;1)
# Euclidean distance
# Manhattan distance: Is it different to euclidean in this case? 
#

dfDataG <- dfData[,22:41]

### Comparing distances ####
## .Visualizing distance matrices ####
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

### kmeans, euclidean distance ####
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

# TO DO: Look for additional criteria to select the optimal
#        number of clusters
# TO DO: Add repetitions

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


# TO DO: Validation of the clustering
# TO DO: Error estimation for individual classifications

