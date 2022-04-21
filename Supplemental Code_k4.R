# README --------------------------------------------------------------

# Libraries and Imports -----------------------------------------------
# The following code downloads/installs (if not installed) and
# loads the required packages ti run the analysis 

options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "viridis", "ggpubr",
         "ggalt", "ggthemes", "RColorBrewer",
         "cluster", "GGally", "mclust",
         "factoextra", "gtools", "fossil",
         "dendextend", "ggdendro", "NbClust",
         "ggtext")

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {
  install.packages(pckg,repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

# Next, you will need to set the directory (folder) where the relevant data files live.
# You can do this by setting the path to where your files live with the setwd() function:
#   setwd("path_to_folder")

# Import the ICI data, from the 'ICI.tsv' which should be in the
# working directory
ICI <- read.delim("ICI.tsv") # read in the ICI data

# Manipulate data to calculate average of subscales instead of individual items
ICI.s <- ICI %>%
  rowwise() %>%
  summarize(SPR = mean(c(cQ01, cQ02, cQ06, cQ07, cQ10, cQ14, cQ18, cQ19)),
            MT = mean(c(cQ05, cQ08, cQ12, cQ15)),
            EI = mean(c(cQ03, cQ11, cQ16, cQ20)),
            CC = mean(c(cQ04, cQ09, cQ13, cQ17))) %>%
  select(SPR:CC)

# Create Figures folder
dir.create("Figures", showWarnings = F)

# Set if repetitions should be calculated even when available.
# If set to TRUE, execution may take some hours
# If FALSE, 'hca#CLUSTERS_#REPS.rda', 'hca#CLUSTERS_#REPSboot.rda', 
# 'kmn#CLUSTERS_#REPS.rda' and 'kmn#CLUSTERS_#REPSboot.rda' should be 
# in the working directory (where #CLUSTERS is the number of clusters 
# and #REPS is the number of repetitions)
CALC_REPS <- FALSE


# Example 1 (Figure 1): Hypothetical Data for 3 Students ---------------------------------

# Create the data set with the patterns desired:
fig1 <- tibble(Test = rep(c("Test 1", "Test 2", "Test 3", "Test 4"), each = 3),
               Student = rep(c("Student A", "Student B", "Student C"), 4),
               Correct = c(9,14,21,20,13,32,12,15,24,20,16,32)) 

# Plot/Output the desired plot:
png("Figures/F1_Scores.png", height = 1000, width = 2500, res = 600)
ggplot(fig1, aes(x = Test, y = Correct, color = Student, group = Student)) +
  geom_point() +
  geom_line() +
  ylab("# Correct") + xlab(NULL) +
  scale_color_viridis(discrete = TRUE) +
  theme_classic()
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


# Example 2 (Figure 2): Calculate ARI for Hypothetical Data Sets ----------------------

# Create three data sets with designed number of clusters
df1 <- data.frame(Obs = 1:3,
                  Cluster = c(1,1,2)) # rand.index function takes numbers, not letters, so A = 1 and B = 2
df2 <- data.frame(Obs = 1:3,
                  Cluster = c(1,1,2))
df3 <- data.frame(Obs = 1:3,
                  Cluster = c(1,2,2))

# Calculate Rand Index:
# df1 v df2
rand.index(df1$Cluster, df2$Cluster)
# df1 v df3
rand.index(df1$Cluster, df3$Cluster)
# df2 v df3
rand.index(df2$Cluster, df3$Cluster)

# Calculate Adjusted Rand Index:
# df1 v df2
adjustedRandIndex(df1$Cluster, df2$Cluster)
# df1 v df3
adjustedRandIndex(df1$Cluster, df3$Cluster)
# df2 v df3
adjustedRandIndex(df2$Cluster, df3$Cluster)


# Delete unnecessary objects for next code (to prevent envrionment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))

# Example 3 (Figure 3): Descriptive view of 4 ICI Subscales (JITTER) -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625


# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end 
# to add them all to the same canvas. 

# SPR v MT
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$MT), 3) # Pearson correlation for indicated variables
spr.mt <- ggplot(ICI.s, aes(x = SPR, y = MT)) + # Make the plot
  geom_jitter(alpha = .03, height = .05, width = .05) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.1, vjust=0,  
          label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.1))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.25))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# SPR v EI
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$EI), 3) # Pearson correlation for indicated variables
spr.ei <- ggplot(ICI.s, aes(x = SPR, y = EI)) + # Make the plot
  geom_jitter(alpha = .03, height = .05, width = .05) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.1, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.1))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.25))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# SPR v CC
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$CC), 3) # Pearson correlation for indicated variables
spr.cc <- ggplot(ICI.s, aes(x = SPR, y = CC)) + # Make the plot
  geom_jitter(alpha = .03, height = .05, width = .05) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.1, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.1))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.25))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# MT v EI
dat$rcor <- round(cor(ICI.s$MT, ICI.s$EI), 3) # Pearson correlation for indicated variables
mt.ei <- ggplot(ICI.s, aes(x = MT, y = EI)) + # Make the plot
  geom_jitter(alpha = .03, height = .05, width = .05) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.1, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.1))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.25))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# MT v CC
dat$rcor <- round(cor(ICI.s$MT, ICI.s$CC), 3) # Pearson correlation for indicated variables
mt.cc <- ggplot(ICI.s, aes(x = MT, y = CC)) + # Make the plot
  geom_jitter(alpha = .03, height = .05, width = .05) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.1, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.1))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.25))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# CC v EI
dat$rcor <- round(cor(ICI.s$CC, ICI.s$EI), 3) # Pearson correlation for indicated variables
cc.ei <- ggplot(ICI.s, aes(x = CC, y = EI)) + # Make the plot
  geom_jitter(alpha = .03, height = .05, width = .05) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.1, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.1))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), limits = c(-0.1,1.25))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

png("Figures/F3_DescriptiveTogetherJitter.png", height = 5200, width = 5000, res = 900)
ggarrange(spr.mt, NULL, NULL,
          spr.cc, mt.cc, NULL,
          spr.ei, mt.ei, cc.ei)
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


# Example 3 (Figure 3): Descriptive view of 4 ICI Subscales (HEATMAP) -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625

# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end 
# to add them all to the same canvas. 

# SPR v MT
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$MT), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(SPR, MT) %>% 
  group_by(SPR, MT) %>% summarise(cnt = n())

spr.mt <- ggplot(ICI.s.count, aes(x = SPR, y = MT, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                    name="Count") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

leg <- spr.mt + theme(legend.position = "left",
                      legend.text = element_text(size = 8),
                      legend.title = element_text(size = 9))
leg <- get_legend(leg)

# SPR v EI
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$EI), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(SPR, EI) %>% 
  group_by(SPR, EI) %>% summarise(cnt = n())

spr.ei <- ggplot(ICI.s.count, aes(x = SPR, y = EI, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# SPR v CC
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$CC), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(SPR, CC) %>% 
  group_by(SPR, CC) %>% summarise(cnt = n())

spr.cc <- ggplot(ICI.s.count, aes(x = SPR, y = CC, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# MT v EI
dat$rcor <- round(cor(ICI.s$MT, ICI.s$EI), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(MT, EI) %>% 
  group_by(MT, EI) %>% summarise(cnt = n())

mt.ei <- ggplot(ICI.s.count, aes(x = MT, y = EI, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")
# MT v CC
dat$rcor <- round(cor(ICI.s$MT, ICI.s$CC), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(MT, CC) %>% 
  group_by(MT, CC) %>% summarise(cnt = n())

mt.cc <- ggplot(ICI.s.count, aes(x = MT, y = CC, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# CC v EI
dat$rcor <- round(cor(ICI.s$CC, ICI.s$EI), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(CC, EI) %>% 
  group_by(CC, EI) %>% summarise(cnt = n())

cc.ei <- ggplot(ICI.s.count, aes(x = CC, y = EI, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

png("Figures/F3_DescriptiveTogetherHeatmap.png", 
    height = 5200, width = 5000, res = 900)
ggarrange(spr.mt, NULL, NULL, 
          spr.cc, mt.cc, NULL,
          spr.ei, mt.ei, cc.ei,
          legend.grob=leg,
          legend="right")
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


# Example 3 (Figure 3): Descriptive view of 4 ICI Subscales (DOTS) -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625

# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end 
# to add them all to the same canvas. 

# SPR v MT
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$MT), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(SPR, MT) %>% 
  group_by(SPR, MT) %>% summarise(cnt = n())
MAXSPRMT <- max(ICI.s.count$cnt)

spr.mt <- ggplot(ICI.s.count, aes(x = SPR, y = MT, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_size_continuous(name="Count") +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

leg <- spr.mt + theme(legend.position = "left",
                      legend.text = element_text(size = 8),
                      legend.title = element_text(size = 9))
leg <- get_legend(leg)

# SPR v EI
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$EI), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(SPR, EI) %>% 
  group_by(SPR, EI) %>% summarise(cnt = n())

MAXSPREI <- max(ICI.s.count$cnt)

spr.ei <- ggplot(ICI.s.count, aes(x = SPR, y = EI, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_size_continuous() +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# SPR v CC
dat$rcor <- round(cor(ICI.s$SPR, ICI.s$CC), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(SPR, CC) %>% 
  group_by(SPR, CC) %>% summarise(cnt = n())

MAXSPRCC <- max(ICI.s.count$cnt)

spr.cc <- ggplot(ICI.s.count, aes(x = SPR, y = CC, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_size_continuous() +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# MT v EI
dat$rcor <- round(cor(ICI.s$MT, ICI.s$EI), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(MT, EI) %>% 
  group_by(MT, EI) %>% summarise(cnt = n())

MAXSMTEI <- max(ICI.s.count$cnt)

mt.ei <- ggplot(ICI.s.count, aes(x = MT, y = EI, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_size_continuous() +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")
# MT v CC
dat$rcor <- round(cor(ICI.s$MT, ICI.s$CC), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(MT, CC) %>% 
  group_by(MT, CC) %>% summarise(cnt = n())

MAXMTCC <- max(ICI.s.count$cnt)

mt.cc <- ggplot(ICI.s.count, aes(x = MT, y = CC, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_size_continuous() +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# CC v EI
dat$rcor <- round(cor(ICI.s$CC, ICI.s$EI), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(CC, EI) %>% 
  group_by(CC, EI) %>% summarise(cnt = n())

MAXCCEI <- max(ICI.s.count$cnt)

cc.ei <- ggplot(ICI.s.count, aes(x = CC, y = EI, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = .5, y = 1.15, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=seq(0,1,length.out = 5), 
                     limits = c(-0.125,1.25))+
  scale_size_continuous() +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

png("Figures/F3_DescriptiveTogetherDots.png", 
    height = 5200, width = 5000, res = 900)
ggarrange(spr.mt, NULL, NULL, 
          spr.cc, mt.cc, NULL,
          spr.ei, mt.ei, cc.ei,
          legend.grob=leg,
          legend="right")
dev.off()

# Delete unnecessary objects for next code (to prevent envrionment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


# Example 4: Dendrograms for HCA on ICI -----------------------------------
# ...Figure 4(top): Dendrogram from HCA Euclidean/Ward --------------------------

dist.ICI <- dist(ICI.s, method = "euclidean") # compute distance matrix
hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters


# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) %>%
  set("branches_k_color", k=3, value = c("black", "red", "green"))
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(sort(table(sol),decreasing = T)[c(1,2,3)])
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster = 
           ifelse(x<=clusterSizes[1],"1",ifelse(x<=clusterSizes[2],"2","3")))

# Create dendrogram and output plot:
png("Figures/F4original_hcaICIeucward.png", res = 600, height = 1000, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage (original order)") +
  guides(color = FALSE) +
  ylab("height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5))
dev.off()

# To generate the means and standard deviations in the table, need a data set with cluster assignments in them:
hca <- ICI.s %>%
  mutate(Cluster = sol)

# Generate the means/standard deviations using dplyr:
hca %>%
  group_by(Cluster) %>%
  summarise(Percent = n() / nrow(hca),
            SPR.m = mean(SPR),
            SPR.s = sd(SPR),
            MT.m = mean(MT),
            MT.s = sd(MT),
            EI.m = mean(EI),
            EI.s = sd(EI),
            CC.m = mean(CC),
            CC.s = sd(CC))

# ...Figure 4(A): Dendrogram for HCA Euclidean/Average --------------------------

# This section of code is identical to the previous except it swaps out the linkage measure and updates the 
# cluster sizes when determining the colors.

dist.ICI <- dist(ICI.s, method = "euclidean") # compute distance matrix
hc <- hclust(dist.ICI, method = "average")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) %>%
  set("branches_k_color", k=3, value = c("black", "red", "green"))
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(sort(table(sol),decreasing = T)[c(2,3,1)])
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster =
           ifelse(x<=clusterSizes[1],"1",ifelse(x<=clusterSizes[2],"2","3")))

# Create dendrogram and output plot:
png("Figures/F4a_hcaICIeucavg.png", res = 600, height = 1000, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Euclidean distance and average linkage (original order)") +
  guides(color = FALSE) +
  ylab("height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5))
dev.off()

# ...Figure 4(B): Dendrogram for HCA Manhattan/Ward --------------------------

# This section of code is identical to the previous except it swaps out the distance measure and updates the 
# cluster sizes when determining the colors.

dist.ICI <- dist(ICI.s, method = "manhattan") # compute distance matrix
hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) %>%
  set("branches_k_color", k=3, value = c("black", "red", "green"))
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(sort(table(sol),decreasing = T)[c(1,3,2)])
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster =
           ifelse(x<=clusterSizes[1],"1",ifelse(x<=clusterSizes[2],"2","3")))

# Create dendrogram and output plot:
png("Figures/F4b_hcaICImanward.png", res = 600, height = 1000, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Manhattan distance and Ward's linkage (original order)") +
  guides(color = FALSE) +
  ylab("height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5))
dev.off()

# ...Figure 4(C): Dendrogram from HCA Euclidean/Ward Random Order --------------------------

# This section of code is identical to the previous except it randomizes the order of the data and updates the 
# cluster sizes when determining the colors.

set.seed(1)                                   # set a seed so we can recover the random order data
ICI.shuffle <- ICI.s[sample(1:nrow(ICI.s)),]  # randomize the order of the data
dist.ICI <- dist(ICI.shuffle, method = "euclidean") # compute distance matrix
hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) %>%
  set("branches_k_color", k=3, value = c("black", "red", "green"))
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(sort(table(sol),decreasing = T)[c(1,3,2)])
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster =
           ifelse(x<=clusterSizes[1],"1",ifelse(x<=clusterSizes[2],"2","3")))


# Create dendrogram and output plot:
png("Figures/F4c_hcaICIeucward.png", res = 600, height = 1000, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage (random order)") +
  guides(color = FALSE) +
  ylab("height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5))
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))

# Example 5: k-means on ICI -----------------------------------------------
# ...Figure 5: Default (Euclidean) k-means on ICI -------------------------------------

set.seed(1)                                         # set a random seed to recover original starting values
ran.centers <- sample(1:nrow(ICI.s), 3)             # define random centers to start the algorithm
sol <- kmeans(ICI.s, centers = ICI.s[ran.centers,]) # run the k-means clustering using the random centers

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of MT variable
kmn <- ICI.s %>%
  mutate(Cluster = as.character(sol$cluster)) 
kmn.s <- kmn %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            MT.m = mean(MT)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-MT.m)

# Rename arbirary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
kmn.r <- kmn %>%
  mutate(Cluster = recode(Cluster, `1` = kmn.s$Cluster[1],
                          `2` = kmn.s$Cluster[2],
                          `3` = kmn.s$Cluster[3]))
kmn.l <- kmn.r %>%
  mutate(Cluster = paste0("Cluster ", Cluster, " (", kmn.s$n[match(kmn$Cluster, kmn.s$Cluster)], "%)")) %>%
  gather("Scale", "Score", -Cluster)

# Create/Output the plot (Figure 5)
png("Figures/F5_kmneucorig.png", res = 600, height = 1500, width = 3000)
ggplot(kmn.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(alpha = .3) +
  theme_classic() +
  ggtitle("k-means with euclidean distance (random centers #1)") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(plot.title = element_text(size = 10, hjust = .5, vjust = .5))
dev.off()

kmn.r %>%
  group_by(Cluster) %>%
  summarise(N = n(),
            SPR.m = mean(SPR),
            SPR.s = sd(SPR),
            MT.m = mean(MT),
            MT.s = sd(MT),
            EI.m = mean(EI),
            EI.s = sd(EI),
            CC.m = mean(CC),
            CC.s = sd(CC))


# ...Supplemental Figure 1: k-means using Euclidean distance with different starting centers on ICI ------------------

# The only difference between this code and the one to produce Figure 5 is changing the seed, which pulls a different
# set of random starting values

set.seed(45)                                         # set a random seed difference than original
ran.centers <- sample(1:nrow(ICI.s), 3)             # define random centers to start the algorithm
sol <- kmeans(ICI.s, centers = ICI.s[ran.centers,]) # run the k-means clustering using the random centers


# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of MT variable
kmn <- ICI.s %>%
  mutate(Cluster = as.character(sol$cluster)) 
kmn.s <- kmn %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            MT.m = mean(MT)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-MT.m)

# Rename arbirary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
kmn.r <- kmn %>%
  mutate(Cluster = recode(Cluster, `1` = kmn.s$Cluster[1],
                          `2` = kmn.s$Cluster[2],
                          `3` = kmn.s$Cluster[3]))
kmn.l <- kmn.r %>%
  mutate(Cluster = paste0("Cluster ", Cluster, " (", kmn.s$n[match(kmn$Cluster, kmn.s$Cluster)], "%)")) %>%
  gather("Scale", "Score", -Cluster)

# Create/Output the plot (Figure 5)
png("Figures/SF1_kmneucdiffcenters.png", res = 600, height = 1500, width = 3000)
ggplot(kmn.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(alpha = .3) +
  theme_classic() +
  ggtitle("k-means with euclidean distance (random centers #2)") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(plot.title = element_text(size = 10, hjust = .5, vjust = .5))
dev.off()

kmn.r %>%
  group_by(Cluster) %>%
  summarise(N = n(),
            SPR.m = mean(SPR),
            SPR.s = sd(SPR),
            MT.m = mean(MT),
            MT.s = sd(MT),
            EI.m = mean(EI),
            EI.s = sd(EI),
            CC.m = mean(CC),
            CC.s = sd(CC))

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))

# Example 6: Descriptive distribution across possible scores --------------

# Generate a table of how many (percent) students chose each response possibility for each of the four variables
round(table(ICI.s$SPR) / nrow(ICI.s) * 100, 1)
round(table(ICI.s$MT) / nrow(ICI.s) * 100, 1)
round(table(ICI.s$EI) / nrow(ICI.s) * 100, 1)
round(table(ICI.s$CC) / nrow(ICI.s) * 100, 1)


# Example 7: Determining appropriate number of clusters -------------------

# For Figures 6A-C, the x-axis is always 1 through 8, but we need to calculate the Within SS (6A),
# Avg. Silhouette (6B), and Gap Statistic plus standard deviation (6C). The following code does this and
# stores all values in one data frame.

k_range <- 1:8

clustex <- tibble(Cluster = k_range) # set up x-axis (# clusters)

# Compute Within SS using factoextra that visualizes the results to NbClust just to pull y-values. In 
# the figures, we run both Eucliden ("e_") and Ward's method ("w_"), which is why those prefixes appear.

# k-means, Within SS
e_wss <- factoextra::fviz_nbclust(ICI.s, kmeans, method = "wss", 
                                  k.max = max(k_range), nstart = 10) 
clustex$E_WSS <- e_wss$data$y

# k-means, Avg Silhouette
e_sil <- factoextra::fviz_nbclust(ICI.s, kmeans, method = "silhouette", 
                                  k.max = max(k_range), nstart = 10) 
clustex$E_Sil <- e_sil$data$y

# k-means, Gap Statistic + Standard Deviation
e_gap <- factoextra::fviz_nbclust(ICI.s, kmeans, method = "gap_stat", 
                                  k.max = max(k_range), nstart = 10, iter.max = 20, nboot = 50) 
clustex$E_Gap <- e_gap$data$gap
clustex$E_GapLow <- e_gap$data$ymin
clustex$E_GapHigh <- e_gap$data$ymax


# Hierarchical, Ward, Within SS
w_wss <- factoextra::fviz_nbclust(ICI.s, hcut, method = "wss", 
                                  k.max = max(k_range), nstart = 10) 
clustex$W_WSS <- w_wss$data$y

# Hierarchical, Ward, Avg Silhouette
w_sil <- factoextra::fviz_nbclust(ICI.s, hcut, method = "silhouette", 
                                  k.max = max(k_range), nstart = 10) 
clustex$W_Sil <- w_sil$data$y

# Hierarchical, Ward, Gap Statistic + Standard Deviation
w_gap <- factoextra::fviz_nbclust(ICI.s, hcut, method = "gap_stat", 
                                  k.max = max(k_range), nstart = 10, iter.max = 20, nboot = 50) 
clustex$W_Gap <- w_gap$data$gap
clustex$W_GapLow <- w_gap$data$ymin
clustex$W_GapHigh <- w_gap$data$ymax

# ...Figure 6(A): Elbow method --------------------------------------------

# Manipulate the data with all number of clusters information for plotting: 
clustexA <- clustex %>%
  select(Cluster, E_WSS, W_WSS) %>%
  gather("Method", "WSS", -Cluster) %>%
  mutate(Method = recode(Method, E_WSS = "K-means", W_WSS = "Hierarchical"))

# Plot the data; saved to an object because Figure 6 has multiple plots to be used in ggarrange below
fig6A <- ggplot(clustexA, aes(x = Cluster, y = WSS, color = Method, group = Method)) + 
  geom_point() + 
  geom_line() + 
  ylab("Within SS") +
  scale_color_manual(values = viridis(3, option = "D")[1:2]) +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("A") +
  guides(color = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
print(fig6A) # if you want to view the plot alone

# ...Figure 6(B): Average Silhouette method -------------------------------

# Manipulate the data with all number of clusters information for plotting: 
clustexB <- clustex %>%
  select(Cluster, E_Sil, W_Sil) %>%
  gather("Method", "Sil", -Cluster) %>%
  mutate(Method = recode(Method, E_Sil = "K-means", W_Sil = "Hierarchical"))

# Plot the data; saved to an object because Figure 6 has multiple plots to be used in ggarrange below
fig6B <- ggplot(clustexB, aes(x = Cluster, y = Sil, color = Method, group = Method)) + 
  geom_point() + 
  geom_line() + 
  ylab("Avg Silhouette") +
  scale_color_manual(values = viridis(3, option = "D")[1:2]) +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("B") +
  guides(color = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
print(fig6B) # if you want to view the plot alone

# ...Figure 6(C): Gap Statistic method ------------------------------------

# Manipulate the data with all number of clusters information for plotting: 
clustexC <- clustex %>%
  select(Cluster, E_Gap, W_Gap) %>%
  gather("Method", "Gap", -Cluster) %>%
  mutate(Method = recode(Method, E_Gap = "K-means", W_Gap = "Hierarchical"),
         High = c(clustex$E_GapHigh, clustex$W_GapHigh),
         Low = c(clustex$E_GapLow, clustex$W_GapLow))

# Plot the data; saved to an object because Figure 6 has multiple plots to be used in ggarrange below
fig6C <- ggplot(clustexC, aes(x = Cluster, y = Gap, color = Method, group = Method)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High), width = .2) +
  geom_line() + 
  ylab("Gap Statistic") +
  scale_color_manual(values = viridis(3, option = "D")[1:2]) +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("C") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
print(fig6C) # if you want to view the plot alone

# To view all on one plot, use ggarrange to put them altoether:
png("Figures/F6_NoClusters.png", res = 600, height = 1500, width = 5000)
ggarrange(fig6A, fig6B, fig6C, nrow = 1, widths = c(1.25,1.25,2))
dev.off()

# ...Table 3: Number of clusters suggested by various indices -------------

# Use NbClust to calculate all 30 metrics using k-means:
NbClust(ICI.s, distance="euclidean", min.nc = max(c(2,min(k_range))),
        max.nc = max(k_range),
        method = "kmeans", index="all")

# Use NbClust to calculate all 30 metrics using hierarchical
# with Ward's method:
NbClust(ICI.s, distance="euclidean", min.nc = max(c(2,min(k_range))),
        max.nc = max(k_range),
        method = "ward.D2", index="all")

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))

# Example 8: 1,000 Repetitions to determine optimal solution (hierarchical) ---------------

    # /////////////////// WARNING WARNING WARNING /////////////////// #

# WARNING: This section of code will take a considerable amount of time to run (2-10 hours depending
# on the quality of your computer's processor)! If you want to verify the code is working, try changing the 
# number of iterations to a smaller number, like 5, then change back to 1,000 once it is working.

# ...Conducting 1,000 repetitions and saving results ----------------------

# First, define the number of clusters you wish to analyze; which is better to do one at a time because of time constraints.
# Also, we will think about how many times we want to iterate the analysis (iterations), so we can easily change this.
ksel <- 4        # examine 4-cluster solution
iterations <- 1000  # run 1,000 repetitions

if(CALC_REPS) {
  # We will store all results into an object called "clusterings", so we will set that up first to collect the
  # data order, the dendrogram information, within sum of squares for each cluster, and total sums of squares
  clusterings <- data.frame(i = 1:iterations,
                            df = NA,
                            wss = NA,
                            totss = NA,
                            sil = NA,
                            avgsil = NA,
                            centers = NA)
  
  # Next build blank lists to store the results of each individual run
  lstDF <- as.list(rep(NA,iterations))
  lstWss <- as.list(rep(NA,iterations))
  lstTotSS <- as.list(rep,NA,iterations)
  lstSil <- as.list(rep,NA,iterations)
  lstAvgSil <- as.list(rep,NA,iterations)
  lstCenters <- as.list(rep,NA,iterations)
  
  # We also need a custom function to calculate the within SS, which is not readily available in agnes:
  calc_SS <- function(df, cluster){
    df.c <- df[df$Cluster == cluster,]
    sum(as.matrix(dist(df.c)^2)) / (2 * nrow(df.c))
  } 
  
  # The for loop will run each iteration, shuffling the data, running the hierarchical analysis, and computing
  # the within SS, total SS, silhouette, and cluster centers for each iteration
  for(cl in 1:nrow(clusterings)) {
    print(paste("hc -",cl))
    set.seed(cl)
    ranorder <- sample(1:nrow(ICI.s),nrow(ICI.s))
    df1 <- ICI.s[ranorder,]
    distData <- dist(df1, method="euclidean")
    hc <- agnes(distData, diss=T, method="ward")
    df1$Cluster <- cutree(hc, ksel)
    lstDF[[cl]] <- df1
    lstWss[[cl]] <- unlist(lapply(1:ksel, calc_SS, df = df1))
    lstTotSS[[cl]] <- sum(lstWss[[cl]])
    sil <- silhouette(df1$Cluster, dist = distData, method = "complete")
    lstSil[[cl]] <- sil[,3]
    lstAvgSil[[cl]] <- mean(sil[,3])
    lstCenters[[cl]] <- df1 %>% group_by(Cluster) %>% summarize_all(., mean)
  }
  
  # Finally, write the results to the clustering object and save this object because it took a long time to run:
  clusterings$df <- lstDF
  clusterings$wss <- lstWss
  clusterings$totss <- lstTotSS
  clusterings$sil <- lstSil
  clusterings$avgsil <- lstAvgSil
  clusterings$centers <- lstCenters
  
  save(clusterings, file="hca_1000_reps.rda")
}

# If you having previously run the code, you will need to call in the save results, otherwise, you can 
# comment the line out:
load("hca_1000_reps.rda")
clusteringshca <- clusterings

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s", "clusteringshca", "CALC_REPS")))

# ...Finding number of unique solutions -----------------------------------

# Examine how many unique total sum of squares to gauge how many unique solutions were obtained based on 
# total sums of squares or average silhouette:
length(unique(unlist(clusteringshca$totss)))
length(unique(round(unlist(clusteringshca$avgsil),6)))

# Count unique by fingerprint each partition
fingerprint <- as.data.frame(t(apply(clusteringshca, 1, function(x)
  c(x$wss, as.numeric(table(x$df[,5]))))))
fingerprint <- as.data.frame(t(apply(fingerprint,1,sort)))
fingerprint <- round(fingerprint,1)
fingerprint <- apply(fingerprint, 1, paste, collapse=" ")
length(unique(fingerprint))


# Example 9: Visualizing the solutions for HCA analysis -------------------
# ...Figure 7: Boxplot of HCA solution optimized to Total SS ---------------------------------

# For the following analysis, we have chosen the solution with the maximum silhouette, but could also 
# choose the solution with the minimum total sum of squares. All we do is identify the solution that led
# to either outcome; currently, the code as written examines the maximum silhouette but code is commented out 
# that would lead to the minimum total sums of square. In the following code, simply substitute "bestsoln_sil" 
# with "bestsoln_totss" to swap between the two solutions.
bestsoln_sil <- which.max(clusteringshca$avgsil)
bestsoln_totss <- which.min(clusteringshca$totss)

# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(clusteringshca$df[[bestsoln_sil]]$Cluster))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(clusteringshca$df[[bestsoln_sil]]$Cluster)),
         Percent = round((Size / sum(Size)) * 100, digits = 1))

hca.box <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)"))) %>%
  pivot_longer(-Cluster, names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = Value*100)

# Create/Output the plot
png("Figures/F7_hcabox.png", height = 2000, width = 2500, res = 600)
ggplot(hca.box, aes(x = Subscale, fill = Cluster, color = Cluster, y = Value)) +
  geom_boxplot(position = position_dodge2(padding=.2)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster) +
  guides(fill = FALSE, color = FALSE) +
  labs(x="Subscale", y="Avg Score (%)")+
  theme_classic()
dev.off()

# ...Supplemental Figure 2: Alternative Figure 7 (boxplot 2) --------------------------

hca.box <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], "\n(",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], "\n(",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], "\n(",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], "\n(",
                                       tab$Percent[tab$Cluster == 4], "%)"))) %>%
  pivot_longer(-Cluster, names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = Value*100)

png("Figures/SF2_hcabox2.png", height = 2000, width = 3500, res = 600)
ggplot(hca.box, aes(x = Cluster, fill = Cluster, color = Cluster, y = Value)) +
  geom_boxplot(position = position_dodge2(padding=.2)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Subscale) +
  guides(fill = FALSE, color = FALSE) +
  labs(x="Subscale", y="Avg Score (%)")+
  theme_classic()
dev.off()

# ...Supplemental Figure 3: Alternative Figure 7 (heat map) ------------------

# Pull just the centers to plot a heat map
hca.heat <- clusteringshca$centers[[bestsoln_sil]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], "\n(",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], "\n(",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], "\n(",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], "\n(",
                                       tab$Percent[tab$Cluster == 4], "%)"))) %>%
  pivot_longer(-Cluster, names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = round(Value*100, digits = 1))

# Create/Output the plot
png("Figures/SF3_hcaheat.png", height = 1500, width = 2500, res = 600)
ggplot(hca.heat,aes(x = Cluster, y = Subscale, fill = Value, label = Value)) +
  geom_tile() +
  geom_text(aes(color = Value > 30)) +
  xlab(NULL) + ylab("Subscale") +
  labs(fill = "Mean") +
  scale_fill_viridis() +
  scale_x_discrete() +
  scale_color_manual(values=c("white","black")) +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())
dev.off()

# ...Supplemental Figure 4: Alternative Figure 7 (scatter plots) --------------------------

# Pull the data to plot a scatter plot
hca.scat <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)")),
         SPR = SPR*100,
         MT = MT*100,
         CC = CC*100,
         EI = EI*100)

# Pull just the centers to plot a 2D-density plot
hca.center <- clusteringshca$centers[[bestsoln_sil]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)")),
         SPR = SPR*100,
         MT = MT*100,
         CC = CC*100,
         EI = EI*100)

# Create/Output Plot #1
hca.spr.mt <- ggplot(hca.scat, aes(x = SPR, y = MT, color = Cluster)) +
  geom_encircle() +
  geom_jitter(shape=21, alpha=.2)+
  geom_point(data=hca.center, size=5, shape="+") +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster) + 
  scale_x_continuous(breaks=seq(0,100,by=25)) + 
  scale_y_continuous(breaks=seq(0,100,by=25)) + 
  guides(color = FALSE) +
  theme_classic()
print(hca.spr.mt) # to view one plot by itself

# Create/Output Plot #2
hca.cc.ei <- ggplot(hca.scat, aes(x = CC, y = EI, color = Cluster)) +
  geom_encircle() +
  geom_jitter(shape=21, alpha=.2)+
  geom_point(data=hca.center, size=5, shape="+") +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster) + 
  scale_x_continuous(breaks=seq(0,100,by=25)) + 
  scale_y_continuous(breaks=seq(0,100,by=25)) + 
  theme_classic()
print(hca.cc.ei) # to view one plot by itself

# Create/Output the two plots together
png("Figures/SF4_hcascat.png", height = 2500, width = 4500, res = 600)
ggarrange(hca.spr.mt, hca.cc.ei, widths = c(1,1.5))
dev.off()

# Example 10: Silhouette Plot (hierarchical) ---------------------------------------------
# ...Figure 8: Silhouette Plot --------------------------------------------

# Pull required information from silhouette values
hca.sil <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)")),
         Silhouette = clusteringshca$sil[[bestsoln_sil]]) %>%
  arrange(Cluster, -Silhouette)

png("Figures/F8_hcasil.png", height = 1500, width = 2500, res = 600)
ggplot(hca.sil, aes(x = 1:nrow(hca.sil), y = Silhouette, color = Cluster)) +
  geom_segment(aes(xend = 1:nrow(hca.sil), yend = 0)) +
  geom_hline(yintercept = mean(hca.sil$Silhouette), lty = 2) +
  xlab(NULL) +
  theme_classic() +
  scale_color_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
dev.off()

# Example 11: 1,000 Bootstrap samples to determine stability (hierarchical) ------------------------------------

# /////////////////// WARNING WARNING WARNING /////////////////// #

# WARNING: This section of code will take a considerable amount of time to run (2-10 hours depending
# on the quality of your computer's processor)! If you want to verify the code is working, try changing the 
# number of iterations to a smaller number, like 5, then change back to 1,000 once it is working.

# ...Conducting 1,000 bootstrap samples and saving results ----------------------

# We have not yet gotten rid of our previous environment, so we still have an object that houses the "best"
# solution according to silhouette or within SS, whichever we chose. We will use this solution as the default
# solution to compare bootstrap examples.
bestsoln <- clusteringshca$df[[bestsoln_sil]]

# Define the number of iterations to be used
iterations <- 1000  # run 1,000 repetitions

if(CALC_REPS) {
  # This time, all we really need is the solutions in a data frame and adjusted Rand index
  clusterings <- data.frame(i = 1:iterations,
                            df = NA,
                            ari = NA)
  
  # Next build blank lists to store the results of each individual run and the adjusted Rand index
  lstDF <- as.list(rep(NA,iterations))
  lstARI <- as.list(rep(NA,iterations))
  
  # The for loop will run each iteration, shuffling the data, running the hierarchical analysis, and computing
  # the within SS, total SS, silhouette, and cluster centers for each iteration
  for(cl in 1:nrow(clusterings)) {
    print(cl)
    set.seed(cl)
    bootsamp <- sample(1:nrow(bestsoln), nrow(bestsoln), replace = TRUE)
    df1 <- bestsoln[bootsamp,] %>% select(-Cluster)
    distData <- dist(df1, method="euclidean")
    hc <- agnes(distData, diss=T, method="ward")
    df1$Cluster <- cutree(hc, ksel)
    df1$OCluster <- bestsoln$Cluster[bootsamp]
    lstDF[[cl]] <- df1
    lstARI[[cl]] <- adjustedRandIndex(df1$OCluster, df1$Cluster)
  }
  
  # Finally, write the results to the clustering object and save this object because it took a long time to run:
  clusterings$df <- lstDF
  clusterings$ari <- lstARI
  
  save(clusterings, file="hca_1000_boot.rda")
}

# ...Supplemental Figure 5: Adjusted Rand-Index Comparison for HCA --------

# Load the saved data and pull the ARI
load("hca_1000_boot.rda")
boothca <- clusterings
boothca.ari <- data.frame(ARI = unlist(boothca$ari))

# Plot/Output the desired graph:
png("Figures/SF5_hcaboot.ari.png", height = 1500, width = 2000, res = 600)
ggplot(boothca.ari, aes(x = ARI)) +
  geom_histogram(fill = "grey80", color = "black", binwidth = 0.025,
                 boundary=1) + 
  geom_density(aes(y = ..density..*1000*0.025)) +
  ylab("Count") + xlab("Adjusted Rand Index") +
  scale_x_continuous(limits=c(0,1))+
  theme_classic()
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))

# Example 12: k-means analyses --------------------------------------------

# This section contains everything done for hierarchical results, except now for k-means analyses. The code is very
# similar to that of the hierarchical, so much of it is not commented; see the hierarchical example for the rationale
# behind code.

# Unlike HCA, k-means is quite a fast algorithm, this section of code should have no problem running in ~5 minutes or less.

# ...Conducting 1,000 repetitions and saving results ----------------------

ksel <- 4        
iterations <- 1000  
distData <- dist(ICI.s, method="euclidean") # dissimiliarity matrix to calculate silhouette
df1 <- ICI.s                               # temp data set to avoid overwriting actual data in loop to come

if(CALC_REPS) {
  clusterings <- data.frame(i = 1:iterations,
                            df = NA,
                            sil = NA,
                            totss = NA,
                            bwss = NA,
                            avgsil = NA,
                            centers = NA)
  
  lstDF <- as.list(rep(NA,iterations))
  lstSil <- as.list(rep(NA,iterations))
  lstTotSS <- as.list(rep(NA,iterations))
  lstBwSS <- as.list(rep(NA,iterations))
  lstAvgSil <- as.list(rep(NA,iterations))
  lstCenters <- as.list(rep(NA,iterations))
  
  for(cl in 1:nrow(clusterings)) {
    print(cl)
    set.seed(cl)
    kmn <- kmeans(ICI.s, centers = ksel, nstart=25, iter.max=20)
    df1$Cluster <- kmn$cluster
    lstDF[[cl]] <- df1
    sil <- silhouette(df1$Cluster, dist = distData, method = "complete")
    lstSil[[cl]] <- sil[,3]
    lstAvgSil[[cl]] <- mean(sil[,3])
    lstTotSS[[cl]] <- kmn$tot.withinss
    lstBwSS[[cl]] <- kmn$betweenss
    lstCenters[[cl]] <- df1 %>% group_by(Cluster) %>% summarize_all(., mean)
  }
  
  clusterings$df <- lstDF
  clusterings$sil <- lstSil
  clusterings$avgsil <- lstAvgSil
  clusterings$totss <- lstTotSS
  clusterings$bwss <- lstBwSS
  clusterings$centers <- lstCenters
  
  save(clusterings, file="kmn_1000_reps.rda")
}

# If you having previously run the code, you will need to call in the save results, otherwise, you can 
# comment the line out:
load("kmn_1000_reps.rda")
clusteringskmn <- clusterings

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s", "clusteringskmn", "CALC_REPS")))

# ...Finding number of unique solutions -----------------------------------

length(unique(unlist(clusteringskmn$totss)))
length(unique(round(unlist(clusteringskmn$avgsil),6)))

# Count unique by fingerprint each partition
fingerprint <- as.data.frame(t(apply(clusteringskmn, 1, function(x) {
  c(as.numeric(table(x$df$Cluster)),
    round(as.numeric(x$centers$SPR),3))
})))
fingerprint <- as.data.frame(t(apply(fingerprint,1,sort)))
fingerprint <- round(fingerprint,1)
fingerprint <- apply(fingerprint, 1, paste, collapse=" ")
length(unique(fingerprint))


# ...Figure 9: Boxplot of KMN solution optimized to Total SS --------------

bestsoln_sil <- which.max(clusteringskmn$avgsil)  # optimal solution based on silhouette

# JCVS: reversed
# bestsoln_totss <- which.max(clusteringskmn$totss) # optimal solution based on total (within) ss
# bestsoln_bwss <- which.min(clusteringskmn$totss)  # optimal solution based on total (between) ss
bestsoln_totss <- which.min(clusteringskmn$totss) # optimal solution based on total (within) ss
bestsoln_bwss <- which.max(clusteringskmn$bwss)  # optimal solution based on total (between) ss


tab <- tibble(as.vector(table(clusteringskmn$df[[bestsoln_totss]]$Cluster))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(clusteringskmn$df[[bestsoln_totss]]$Cluster)),
         Percent = round((Size / sum(Size)) * 100, digits = 1))

kmn.box <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)"))) %>%
  pivot_longer(-Cluster, names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = Value*100)

png("Figures/F9_kmnbox.png", height = 2000, width = 2500, res = 600)
ggplot(kmn.box, aes(x = Subscale, fill = Cluster, color = Cluster, y = Value)) +
  geom_boxplot(position = position_dodge2(padding=.2)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster) +
  guides(fill = FALSE, color = FALSE) +
  labs(x="Subscale", y="Avg Score (%)")+
  theme_classic()
dev.off()

# ...Supplemental Figure 6: Alternative Figure 9 (boxplot 2) --------------------------

kmn.box <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], "\n(",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], "\n(",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], "\n(",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], "\n(",
                                       tab$Percent[tab$Cluster == 4], "%)"))) %>%
  pivot_longer(-Cluster, names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = Value*100)

png("Figures/SF6_kmnbox2.png", height = 2000, width = 3500, res = 600)
ggplot(kmn.box, aes(x = Cluster, fill = Cluster, color = Cluster, y = Value)) +
  geom_boxplot(position = position_dodge2(padding=.2)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Subscale) +
  guides(fill = FALSE, color = FALSE) +
  labs(x="Subscale", y="Avg Score (%)")+
  theme_classic()
dev.off()

# ...Supplemental Figure 7: Alternative Figure 9 (heat map) ------------------

# Pull just the centers to plot a heat map
kmn.heat <- clusteringskmn$centers[[bestsoln_totss]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], "\n(",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], "\n(",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], "\n(",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], "\n(",
                                       tab$Percent[tab$Cluster == 4], "%)"))) %>%
  pivot_longer(-Cluster, names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = round(Value*100, digits = 1))

# Create/Output the plot
png("Figures/SF7_kmnheat.png", height = 1500, width = 2500, res = 600)

ggplot(kmn.heat,aes(x = Cluster, y = Subscale, fill = Value, label = Value)) +
  geom_tile() +
  geom_text(aes(color = Value > 30)) +
  xlab(NULL) + ylab("Subscale") +
  labs(fill = "Mean") +
  scale_fill_viridis() +
  scale_x_discrete() +
  scale_color_manual(values=c("white","black")) +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())
dev.off()

# ...Supplemental Figure 8: Alternative Figure 9 (scatter plots) --------------------------

# Pull the data to plot a scatter plot
kmn.scat <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)")),
         SPR = SPR*100,
         MT = MT*100,
         CC = CC*100,
         EI = EI*100)

# Pull just the centers to plot a 2D-density plot
kmn.center <- clusteringskmn$centers[[bestsoln_totss]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)")),
         SPR = SPR*100,
         MT = MT*100,
         CC = CC*100,
         EI = EI*100)

# Create/Output Plot #1
kmn.spr.mt <- ggplot(kmn.scat, aes(x = SPR, y = MT, color = Cluster)) +
  geom_encircle() +
  geom_jitter(shape=21, alpha=.2)+
  geom_point(data=kmn.center, size=5, shape="+") +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster) + 
  scale_x_continuous(breaks=seq(0,100,by=25)) + 
  scale_y_continuous(breaks=seq(0,100,by=25)) + 
  guides(color = FALSE) +
  theme_classic()
print(kmn.spr.mt) # to view one plot by itself

# Create/Output Plot #2
kmn.cc.ei <- ggplot(kmn.scat, aes(x = CC, y = EI, color = Cluster)) +
  geom_encircle() +
  geom_jitter(shape=21, alpha=.2)+
  geom_point(data=kmn.center, size=5, shape="+") +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster) + 
  scale_x_continuous(breaks=seq(0,100,by=25)) + 
  scale_y_continuous(breaks=seq(0,100,by=25)) + 
  theme_classic()
print(kmn.cc.ei) # to view one plot by itself

# Create/Output the two plots together
png("Figures/SF8_kmnscat.png", height = 2500, width = 4500, res = 600)
ggarrange(kmn.spr.mt, kmn.cc.ei, widths = c(1,1.5))
dev.off()


# ...Supplemental Figure 9: Silhouette Plot (k-means) --------------------------------------------

# Pull required information from silhouette values
kmn.sil <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = recode(Cluster, `1` = paste0("Cluster 1\n N = ", tab$Size[tab$Cluster == 1], " (",
                                                tab$Percent[tab$Cluster == 1], "%)"),
                          `2` = paste0("Cluster 2\n N = ", tab$Size[tab$Cluster == 2], " (",
                                       tab$Percent[tab$Cluster == 2], "%)"),
                          `3` = paste0("Cluster 3\n N = ", tab$Size[tab$Cluster == 3], " (",
                                       tab$Percent[tab$Cluster == 3], "%)"),
                          `4` = paste0("Cluster 4\n N = ", tab$Size[tab$Cluster == 4], " (",
                                       tab$Percent[tab$Cluster == 4], "%)")),
         Silhouette = clusteringskmn$sil[[bestsoln_sil]]) %>%
  arrange(Cluster, -Silhouette)

png("Figures/SF9_kmnsil.png", height = 1500, width = 2500, res = 600)
ggplot(kmn.sil, aes(x = 1:nrow(kmn.sil), y = Silhouette, color = Cluster)) +
  geom_segment(aes(xend = 1:nrow(kmn.sil), yend = 0)) +
  geom_hline(yintercept = mean(kmn.sil$Silhouette), lty = 2) +
  xlab(NULL) +
  theme_classic() +
  scale_color_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
dev.off()

# ...Conducting 1,000 bootstrap samples and saving results ----------------------

ksel <- 4
bestsoln <- clusteringskmn$df[[bestsoln_totss]]

# Define the number of iterations to be used
iterations <- 1000  # run 1,000 repetitions

if(CALC_REPS) {
  # This time, all we really need is the solutions in a data frame and adjusted Rand index
  clusterings <- data.frame(i = 1:iterations,
                            df = NA,
                            ari = NA)
  
  # Next build blank lists to store the results of each individual run and the adjusted Rand index
  lstDF <- as.list(rep(NA,iterations))
  lstARI <- as.list(rep(NA,iterations))
  
  # The for loop will run each iteration, shuffling the data, running the hierarchical analysis, and computing
  # the within SS, total SS, silhouette, and cluster centers for each iteration
  for(cl in 1:nrow(clusterings)) {
    print(cl)
    set.seed(cl)
    bootsamp <- sample(1:nrow(bestsoln), nrow(bestsoln), replace = TRUE)
    df1 <- bestsoln[bootsamp,] %>% select(-Cluster)
    distData <- dist(df1, method="euclidean")
    kmn <- kmeans(df1, centers = ksel, nstart=25, iter.max=20)
    df1$Cluster <- kmn$cluster
    df1$OCluster <- bestsoln$Cluster[bootsamp]
    lstDF[[cl]] <- df1
    lstARI[[cl]] <- adjustedRandIndex(df1$OCluster, df1$Cluster)
  }
  
  # Finally, write the results to the clustering object and save this object because it took a long time to run:
  clusterings$df <- lstDF
  clusterings$ari <- lstARI
  
  save(clusterings, file="kmn_1000_boot.rda")
}

# ...Supplemental Figure 10: Adjusted Rand-Index Comparison for k-means --------

load("kmn_1000_boot.rda")
bootkmn <- clusterings
bootkmn.ari <- data.frame(ARI = unlist(bootkmn$ari))

# Plot/Output the desired graph:
png("Figures/SF10_kmnboot.ari.png", height = 1500, width = 2000, res = 600)
ggplot(bootkmn.ari, aes(x = ARI)) +
  geom_histogram(fill = "grey80", color = "black", binwidth = 0.025,
                 boundary=1) + 
  geom_density(aes(y = ..density..*1000*0.025)) +
  ylab("Count") + xlab("Adjusted Rand Index") +
  scale_x_continuous(limits=c(0,1))+
  theme_classic()
dev.off()

