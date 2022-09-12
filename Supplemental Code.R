#### README --------------------------------------------------------------
# This script runs in R - download R at https://www.r-project.org/. You can additionally
# download RStudio for ease of viewing code and output: https://www.rstudio.com/products/rstudio/download/

# The following code contains all relevant code to reproduce images and analyses as described
# in the published paper. Questions about this code including how it works and how you can modify
# it to suit your needs can be directed to jharshman@auburn.edu

#### LIBRARIES AND IMPORTS -----------------------------------------------
# The following code downloads/installs (if not installed) and
# loads the required packages to run the analysis 

options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "viridis", "gridExtra", "ggpubr", 
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




#### SETTINGS -----------------------------------------------
# Next, you will need to set the directory (folder) where the relevant data files live.
# You can do this by setting the path to where your files live with the setwd() function:
#   setwd("path_to_folder")

# Create Figures folder
dir.create("Figures", showWarnings = F)

# Remove preexisting figures
prevFigures <- dir(path="Figures", pattern=".*[.]png",)
if(length(prevFigures)>0) prevFigures <- paste0("Figures/",prevFigures)
file.remove(prevFigures)

# Set if repetitions should be calculated even when available.
# If set to TRUE, execution may take some hours
# If FALSE, 'hca_#CLUSTERS_#REPS_reps.rda', 'hca_#CLUSTERS_#REPS_boot.rda', 
# 'kmn_#CLUSTERS_#REPS_reps.rda' and 'kmn_#CLUSTERS_#REPS_boot.rda' should be 
# in the working directory (where #CLUSTERS is the number of clusters 
# and #REPS is the number of repetitions)
CALC_REPS <- FALSE


#### EXAMPLE 1: Hypothetical Data for 3 Students ---------------------------------
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
rm(list=setdiff(ls(), c("CALC_REPS")))


#### EXAMPLE 2: Calculate ARI for Hypothetical Data Sets ----------------------
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

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("CALC_REPS")))


#### DATA IMPORT -----------------------------------------------
# Import the ICI data, from the 'ICI.tsv' which should be in the
# working directory
ICI <- read.delim("ICI.tsv") # read in the ICI data

# Manipulate data to calculate average of subscales instead of individual items
ICI.s <- ICI %>%
  rowwise() %>%
  summarize(
    Factor1 = mean(c(
      cQ01, cQ02, cQ06, cQ07, cQ10, cQ14, cQ18, cQ19
    )),
    Factor2 = mean(c(cQ05, cQ08, cQ12, cQ15)),
    Factor3 = mean(c(cQ03, cQ11, cQ16, cQ20)),
    Factor4 = mean(c(cQ04, cQ09, cQ13, cQ17))
  ) %>%
  select(Factor1:Factor4)

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))

#### EXAMPLE 3: Descriptive view of ICI ------------------------------------
### Descriptive view of 4 ICI Subscales - jitter -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625

# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end
# to add them all to the same canvas.

ICI.s100 <- ICI.s %>% 
  mutate(Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Factor1 v Factor2
dat$rcor <-
  round(cor(ICI.s$Factor1, ICI.s$Factor2), 3) # Pearson correlation for indicated variables
factor1.factor2 <-
  ggplot(ICI.s100, aes(x = Factor1, y = Factor2)) + # Make the plot
  geom_jitter(alpha = .03,
              height = 5,
              width = 5) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 110, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.1)) +
  scale_y_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# Factor1 v Factor3
dat$rcor <-
  round(cor(ICI.s$Factor1, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
factor1.factor3 <-
  ggplot(ICI.s100, aes(x = Factor1, y = Factor3)) + # Make the plot
  geom_jitter(alpha = .03,
              height = 5,
              width = 5) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 110, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.1)) +
  scale_y_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# Factor1 v Factor4
dat$rcor <-
  round(cor(ICI.s$Factor1, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
factor1.factor4 <-
  ggplot(ICI.s100, aes(x = Factor1, y = Factor4)) + # Make the plot
  geom_jitter(alpha = .03,
              height = 5,
              width = 5) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 110, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.1)) +
  scale_y_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# Factor2 v Factor3
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
factor2.factor3 <- 
  ggplot(ICI.s100, aes(x = Factor2, y = Factor3)) + # Make the plot
  geom_jitter(alpha = .03,
              height = 5,
              width = 5) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 110, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.1)) +
  scale_y_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# Factor2 v Factor4
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
factor2.factor4 <- 
  ggplot(ICI.s100, aes(x = Factor2, y = Factor4)) + # Make the plot
  geom_jitter(alpha = .03,
              height = 5,
              width = 5) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 110, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.1)) +
  scale_y_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

# Factor3 v Factor4
dat$rcor <- round(cor(ICI.s$Factor4, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
factor3.factor4 <- 
  ggplot(ICI.s100, aes(x = Factor3, y = Factor4)) + # Make the plot
  geom_jitter(alpha = .03,
              height = 5,
              width = 5) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 110, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.1)) +
  scale_y_continuous(breaks = 100 * seq(0, 1, length.out = 5),
                     limits = 100 * c(-0.1, 1.25)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

png("Figures/SF1_DescriptiveTogetherJitter.png", height = 5200,
    width = 6000, res = 900)
ggarrange(factor1.factor2, NULL, NULL,
          factor1.factor3, factor2.factor3, NULL,
          factor1.factor4, factor2.factor4, factor3.factor4)

dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


### Descriptive view of 4 ICI Subscales - heatmap -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625

# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end 
# to add them all to the same canvas. 

# Factor1 v Factor2
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor2), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor1, Factor2) %>% 
  group_by(Factor1, Factor2) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor2 = Factor2 * 100)

factor1.factor2 <- 
  ggplot(ICI.s.count, aes(x = Factor1, y = Factor2, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                    name="Count",
                    limits=c(1,500),
                    breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

leg <- factor1.factor2 + 
  guides(fill = guide_colorbar(reverse=TRUE)) +
  theme(legend.position = "left",
                      legend.text = element_text(size = 8),
                      legend.title = element_text(size = 9))
leg <- get_legend(leg)

# Factor1 v Factor3
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor1, Factor3) %>% 
  group_by(Factor1, Factor3) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor3 = Factor3 * 100)

factor1.factor3 <- 
  ggplot(ICI.s.count, aes(x = Factor1, y = Factor3, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor1 v Factor4
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor1, Factor4) %>% 
  group_by(Factor1, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor4 = Factor4 * 100)

factor1.factor4 <- 
  ggplot(ICI.s.count, aes(x = Factor1, y = Factor4, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor2 v Factor3
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor2, Factor3) %>% 
  group_by(Factor2, Factor3) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor2 = Factor2 * 100,
         Factor3 = Factor3 * 100)

factor2.factor3 <- 
  ggplot(ICI.s.count, aes(x = Factor2, y = Factor3, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor2 v Factor4
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor2, Factor4) %>% 
  group_by(Factor2, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor2 = Factor2 * 100,
         Factor4 = Factor4 * 100)

factor2.factor4 <- 
  ggplot(ICI.s.count, aes(x = Factor2, y = Factor4, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor3 v Factor4
dat$rcor <- round(cor(ICI.s$Factor3, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor3, Factor4) %>% 
  group_by(Factor3, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor3 = Factor3 * 100,
         Factor4 = Factor4 * 100)

factor3.factor4 <- 
  ggplot(ICI.s.count, aes(x = Factor3, y = Factor4, fill = cnt)) + # Make the plot
  geom_tile() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

png("Figures/F4_DescriptiveTogetherHeatmap.png", 
    height = 5200, width = 6500, res = 900)
ggarrange(factor1.factor2, NULL, NULL,
          factor1.factor3, factor2.factor3, NULL,
          factor1.factor4, factor2.factor4, factor3.factor4,
          legend.grob=leg,
          legend="right")
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


### Descriptive view of 4 ICI Subscales - dots -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625

# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end 
# to add them all to the same canvas. 

# Factor1 v Factor2
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor2), 3) # Pearson correlation for indicated variables

ICI.s.count <- ICI.s %>% select(Factor1, Factor2) %>% 
  group_by(Factor1, Factor2) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor2 = Factor2 * 100)

factor1.factor2 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor2, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        breaks=c(1,(1:5)*100),
                        range=c(0.5,4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

leg <- factor1.factor2 + theme(legend.position = "left",
                      legend.text = element_text(size = 8),
                      legend.title = element_text(size = 9))
leg <- get_legend(leg)

# Factor1 v Factor3
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor3), 3) # Pearson correlation for indicated variables

ICI.s.count <- ICI.s %>% select(Factor1, Factor3) %>% 
  group_by(Factor1, Factor3) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor3 = Factor3 * 100)

factor1.factor3 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor3, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        breaks=c(1,(1:5)*100),
                        range=c(0.5,4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor1 v Factor4
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor4), 3) # Pearson correlation for indicated variables

ICI.s.count <- ICI.s %>% select(Factor1, Factor4) %>% 
  group_by(Factor1, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor4 = Factor4 * 100)

factor1.factor4 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor4, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        breaks=c(1,(1:5)*100),
                        range=c(0.5,4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor2 v Factor3
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor2, Factor3) %>% 
  group_by(Factor2, Factor3) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor2 = Factor2 * 100,
         Factor3 = Factor3 * 100)

factor2.factor3 <- ggplot(ICI.s.count, aes(x = Factor2, y = Factor3, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        breaks=c(1,(1:5)*100),
                        range=c(0.5,4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor2 v Factor4
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor2, Factor4) %>% 
  group_by(Factor2, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor2 = Factor2 * 100,
         Factor4 = Factor4 * 100)

factor2.factor4 <- ggplot(ICI.s.count, aes(x = Factor2, y = Factor4, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        breaks=c(1,(1:5)*100),
                        range=c(0.5,4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor3 v Factor4
dat$rcor <- round(cor(ICI.s$Factor3, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor3, Factor4) %>% 
  group_by(Factor3, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor3 = Factor3 * 100,
         Factor4 = Factor4 * 100)

factor3.factor4 <- ggplot(ICI.s.count, aes(x = Factor3, y = Factor4, size = cnt)) + # Make the plot
  geom_point(alpha = .25) +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks=100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        breaks=c(1,(1:5)*100),
                        range=c(0.5,4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

png("Figures/XF1_DescriptiveTogetherDots.png", 
    height = 5200, width = 6500, res = 900)
ggarrange(factor1.factor2, NULL, NULL,
          factor1.factor3, factor2.factor3, NULL,
          factor1.factor4, factor2.factor4, factor3.factor4,
          legend.grob=leg,
          legend="right")
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


### Descriptive view of 4 ICI Subscales - colored dots -------

dat <- data.frame(x = .5, y = .625, rcor = 0) # create correlation information at x=0.5, y = 0.625

# To create one ggplot with multiple plots, make each on individually and then use ggarrange at end 
# to add them all to the same canvas. 

# Factor1 v Factor2
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor2), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor1, Factor2) %>% 
  group_by(Factor1, Factor2) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor2 = Factor2 * 100)

factor1.factor2 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor2, 
                                  size = cnt, color = cnt)) + # Make the plot
  geom_point() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        range=c(0.5,5)) +
  scale_color_viridis(name="Count",direction=-1,
                      guide="legend",
                      limits=c(1,500))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

leg <- factor1.factor2 + theme(legend.position = "left",
                      legend.text = element_text(size = 8),
                      legend.title = element_text(size = 9))
leg <- get_legend(leg)

# Factor1 v Factor3
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor1, Factor3) %>% 
  group_by(Factor1, Factor3) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor3 = Factor3 * 100)

factor1.factor3 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor3, 
                                  size = cnt, color = cnt)) + # Make the plot
  geom_point() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        range=c(0.5,5)) +
  scale_color_viridis(name="Count",direction=-1,
                      guide="legend",
                      limits=c(1,500))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor1 v Factor4
dat$rcor <- round(cor(ICI.s$Factor1, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor1, Factor4) %>% 
  group_by(Factor1, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor4 = Factor4 * 100)

factor1.factor4 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor4, 
                                  size = cnt, color = cnt)) + # Make the plot
  geom_point() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        range=c(0.5,5)) +
  scale_color_viridis(name="Count",direction=-1,
                      guide="legend",
                      limits=c(1,500))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor2 v Factor3
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor3), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor2, Factor3) %>% 
  group_by(Factor2, Factor3) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor2 = Factor2 * 100,
         Factor3 = Factor3 * 100)

factor2.factor3 <- ggplot(ICI.s.count, aes(x = Factor2, y = Factor3, 
                                 size = cnt, color = cnt)) + # Make the plot
  geom_point() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        range=c(0.5,5)) +
  scale_color_viridis(name="Count",direction=-1,
                      guide="legend",
                      limits=c(1,500))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor2 v Factor4
dat$rcor <- round(cor(ICI.s$Factor2, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor2, Factor4) %>% 
  group_by(Factor2, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor2 = Factor2 * 100,
         Factor4 = Factor4 * 100)

factor2.factor4 <- ggplot(ICI.s.count, aes(x = Factor2, y = Factor4, 
                                 size = cnt, color = cnt)) + # Make the plot
  geom_point() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        range=c(0.5,5)) +
  scale_color_viridis(name="Count",direction=-1,
                      guide="legend",
                      limits=c(1,500))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

# Factor3 v Factor4
dat$rcor <- round(cor(ICI.s$Factor3, ICI.s$Factor4), 3) # Pearson correlation for indicated variables
ICI.s.count <- ICI.s %>% select(Factor3, Factor4) %>% 
  group_by(Factor3, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor3 = Factor3 * 100,
         Factor4 = Factor4 * 100)

factor3.factor4 <- ggplot(ICI.s.count, aes(x = Factor3, y = Factor4, 
                                 size = cnt, color = cnt)) + # Make the plot
  geom_point() +
  annotate(geom="richtext", fill=NA, label.color=NA,
           x = 50, y = 115, vjust=0,  
           label = paste("<i>r</i> =", dat$rcor), size = 3) +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.125))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5), 
                     limits = 100 * c(-0.125,1.25))+
  scale_size_continuous(name="Count",
                        limits=c(1,500),
                        range=c(0.5,5)) +
  scale_color_viridis(name="Count",direction=-1,
                      guide="legend",
                      limits=c(1,500))+
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

png("Figures/XF2_DescriptiveTogetherCDots.png", 
    height = 5200, width = 6500, res = 900)
ggarrange(factor1.factor2, NULL, NULL,
          factor1.factor3, factor2.factor3, NULL,
          factor1.factor4, factor2.factor4, factor3.factor4,
          legend.grob=leg,
          legend="right")
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


### Descriptive view of 4 ICI Subscales - box plots -------

data.box <- ICI.s %>%
  pivot_longer(1:4,names_to = "Subscale", values_to = "Value") %>%
  mutate(Value = Value*100)

data.lim <- data.box %>% group_by(Subscale) %>% 
  summarise(lowerW = quantile(Value, 0.25) - 1.5 * IQR(Value),
                       upperW =quantile(Value, 0.75) + 1.5 * IQR(Value))

data.out <- merge(data.box, data.lim) %>% 
  filter(Value > upperW | Value < lowerW)

# Create/Output the plot
png("Figures/F3_DescriptiveTogetherBoxPlots.png", 
    height = 2000, width = 2000, res = 600)
ggplot(data.box, aes(x = Subscale, y = Value)) +
  geom_boxplot(fill="grey", outlier.shape = NA) +
  geom_jitter(data = data.out, shape=21, fill="grey", alpha=0.5,
              height=0, width=0.2, color="black")+
  guides(fill = "none", color = "none") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


### Descriptive view of 4 ICI Subscales - simplified scatter plots -------

# Pull the data to plot a scatter plot
data.scat <- ICI.s %>%
  mutate(Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Pull just the centers to plot
data.center <- ICI.s %>%
  summarise(Factor1 = mean(Factor1),
            Factor2 = mean(Factor2),
            Factor3 = mean(Factor3),
            Factor4 = mean(Factor4)) %>% 
  mutate(Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Create/Output Plot #1
factor1.factor2 <- ggplot(data.scat, aes(x = Factor1, y = Factor2)) +
  geom_jitter(shape=21, alpha=.2,
              height=5, width=5)+
  geom_point(data=data.center, size=5, shape="+") +
  scale_x_continuous(breaks=seq(0,100,by=25)) + 
  scale_y_continuous(breaks=seq(0,100,by=25)) + 
  theme_classic()
print(factor1.factor2) # to view one plot by itself

# Create/Output Plot #2
factor3.factor4 <- ggplot(data.scat, aes(x = Factor3, y = Factor4)) +
  geom_jitter(shape=21, alpha=.2,
              height=5, width=5)+
  geom_point(data=data.center, size=5, shape="+") +
  scale_x_continuous(breaks=seq(0,100,by=25)) + 
  scale_y_continuous(breaks=seq(0,100,by=25)) + 
  theme_classic()
print(factor3.factor4) # to view one plot by itself

# Create/Output the two plots together
png(paste0("Figures/SF3_DescriptiveTogetherSimpleScatter.png"), height = 2500, width = 4500,
    res = 600)
ggarrange(factor1.factor2, factor3.factor4,
          widths = c(1,1), ncol=2)
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


### Descriptive view of 4 ICI Subscales - simplified heatmaps -------
ICI.s.count <- ICI.s %>% select(Factor1, Factor2) %>% 
  group_by(Factor1, Factor2) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor1 = Factor1 * 100,
         Factor2 = Factor2 * 100)

factor1.factor2 <- ggplot(ICI.s.count, aes(x = Factor1, y = Factor2, fill = cnt)) + # Make the plot
  geom_tile() +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")

leg <- factor1.factor2 + 
  guides(fill = guide_colorbar(reverse=TRUE)) +
  theme(legend.position = "left",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9))
leg <- get_legend(leg)

# Factor3 v Factor4
ICI.s.count <- ICI.s %>% select(Factor3, Factor4) %>% 
  group_by(Factor3, Factor4) %>%
  summarise(cnt = n(), .groups="drop") %>% 
  mutate(Factor3 = Factor3 * 100,
         Factor4 = Factor4 * 100)

factor3.factor4 <- ggplot(ICI.s.count, aes(x = Factor3, y = Factor4, fill = cnt)) + # Make the plot
  geom_tile() +
  scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5))+
  scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5))+
  scale_fill_viridis(discrete = FALSE, direction=-1,
                     name="Count",
                     limits=c(1,500),
                     breaks=c(1,(1:5)*100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position="none")


# Create/Output the two plots together
png(paste0("Figures/SF2_DescriptiveTogetherSimpleHeatmap.png"),
    height = 2500, width = 5000,
    res = 600)
ggarrange(factor1.factor2, factor3.factor4,
          widths = c(1,1), ncol=2,
          legend.grob=leg,
          legend="right")
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


#### EXAMPLE 4: Basic HCA on ICI -----------------------------------
### Euclidean/Ward, original order - dendrogram --------------------------

dist.ICI <- dist(ICI.s, method = "euclidean") # compute distance matrix
hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters


# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(sol))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(sol)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) 
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(table(sol))
clOrder <- sol[as.numeric(dend_dat$labels$label)]
clOrder <- unique(clOrder)
clusterSizes <- clusterSizes[clOrder]
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster = clOrder[sapply(dend_dat$segments$x, 
                                  function(xseg) sum(clusterSizes+0.5 <= xseg)+1)]) %>% 
  mutate(Cluster=tab$Label3lines[Cluster])

# Create dendrogram and output plot:
png("Figures/F6_hcaICIeucward.png", res = 600,
    height = 1500, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage (original order)") +
  guides(color = guide_legend("")) +
  ylab("Height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5),
        legend.position = "top")
dev.off()


# To generate the means and standard deviations in the table, need a data set with cluster assignments in them:
hca <- ICI.s %>%
  mutate(Cluster = sol)

# Generate the means/standard deviations using dplyr:
hca %>%
  group_by(Cluster) %>%
  summarise(Percent = n() / nrow(hca),
            Factor1.m = mean(Factor1),
            Factor1.s = sd(Factor1),
            Factor2.m = mean(Factor2),
            Factor2.s = sd(Factor2),
            Factor3.m = mean(Factor3),
            Factor3.s = sd(Factor3),
            Factor4.m = mean(Factor4),
            Factor4.s = sd(Factor4))


### Euclidean/Ward, original order - box plot --------------------------

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of Factor2 variable
hca <- ICI.s %>%
  mutate(Cluster = sol)
hca.s <- hca %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            Factor2.m = mean(Factor2)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-Factor2.m)

# Rename arbitrary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
hca.r <- hca 

hca.l <- hca.r %>%
  mutate(Cluster=tab$Label2lines[Cluster]) %>% 
  gather("Scale", "Score", -Cluster) %>% 
  mutate(Score = Score * 100)

hca.lim <- hca.l %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

hca.out <- merge(hca.l, hca.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot
png("Figures/F5_hcaICIeucward_box.png", res = 600,
    height = 2400, width = 3500)
ggplot(hca.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage\n(original order)") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()

altjHV <- ggplot(hca.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.5,
              height=5, width=0.1)+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage\n(original order)") +
  labs(x=NULL, y="Score")+
  theme_classic()

altjHc <- ggplot(hca.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = hca.out, shape=21, alpha=0.1,
              position=position_jitter(height=0, width=0.2, seed=1))+
  geom_point(data = hca.out, shape=21, fill=NA,
             position=position_jitter(height=0, width=0.2, seed=1))+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage\n(original order)") +
  labs(x=NULL, y="Score")+
  theme_classic()


### Euclidean/Average - dendrogram --------------------------

# This section of code is identical to the previous except it swaps out the linkage measure and updates the 
# cluster sizes when determining the colors.

dist.ICI <- dist(ICI.s, method = "euclidean") # compute distance matrix
hc <- hclust(dist.ICI, method = "average")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters


# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(sol))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(sol)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) 
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(table(sol))
clOrder <- sol[as.numeric(dend_dat$labels$label)]
clOrder <- unique(clOrder)
clusterSizes <- clusterSizes[clOrder]
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster = clOrder[sapply(dend_dat$segments$x, 
                                  function(xseg) sum(clusterSizes+0.5 <= xseg)+1)]) %>% 
  mutate(Cluster=tab$Label3lines[Cluster])

# Create dendrogram and output plot:
png("Figures/XF3c_hcaICIeucavg.png", res = 600,
    height = 1500, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Euclidean distance and average linkage (original order)") +
  guides(color = guide_legend("")) +
  ylab("Height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5),
        legend.position = "top")
dev.off()


### Euclidean/Average - box plot --------------------------

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of Factor2 variable
hca <- ICI.s %>%
  mutate(Cluster = sol)
hca.s <- hca %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            Factor2.m = mean(Factor2)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-Factor2.m)

# Rename arbitrary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
hca.r <- hca 

hca.l <- hca.r %>%
  mutate(Cluster=tab$Label2lines[Cluster]) %>% 
  gather("Scale", "Score", -Cluster) %>% 
  mutate(Score = Score * 100)

hca.lim <- hca.l %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

hca.out <- merge(hca.l, hca.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot
png("Figures/F7c_hcaICIeucavg_box.png", res = 600,
    height = 2400, width = 3500)
ggplot(hca.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("Hierarchical with Euclidean distance and average linkage\n(original order)") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()


### Manhattan/Ward - dendrogram  --------------------------

# This section of code is identical to the previous except it swaps out the distance measure and updates the 
# cluster sizes when determining the colors.

dist.ICI <- dist(ICI.s, method = "manhattan") # compute distance matrix
hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters


# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(sol))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(sol)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) 
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(table(sol))
clOrder <- sol[as.numeric(dend_dat$labels$label)]
clOrder <- unique(clOrder)
clusterSizes <- clusterSizes[clOrder]
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster = clOrder[sapply(dend_dat$segments$x, 
                                  function(xseg) sum(clusterSizes+0.5 <= xseg)+1)]) %>% 
  mutate(Cluster=tab$Label3lines[Cluster])

# Create dendrogram and output plot:
png("Figures/XF3b_hcaICImanward.png", res = 600,
    height = 1500, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Manhattan distance and Ward's linkage (original order)") +
  guides(color = guide_legend("")) +
  ylab("Height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5),
        legend.position = "top")
dev.off()


### Manhattan/Ward - box plot --------------------------

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of Factor2 variable
hca <- ICI.s %>%
  mutate(Cluster = sol)
hca.s <- hca %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            Factor2.m = mean(Factor2)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-Factor2.m)

# Rename arbitrary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
hca.r <- hca 

hca.l <- hca.r %>%
  mutate(Cluster=tab$Label2lines[Cluster]) %>% 
  gather("Scale", "Score", -Cluster) %>% 
  mutate(Score = Score * 100)

hca.lim <- hca.l %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

hca.out <- merge(hca.l, hca.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot
png("Figures/F7b_hcaICImanward_box.png", res = 600,
    height = 2400, width = 3500)
ggplot(hca.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("Hierarchical with Manhattan distance and Ward's linkage\n(original order)") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()


### Euclidean/Ward, random order - dendrogram --------------------------

# This section of code is identical to the previous except it randomizes the order of the data and updates the 
# cluster sizes when determining the colors.

set.seed(1)                                   # set a seed so we can recover the random order data
ICI.shuffle <- ICI.s[sample(1:nrow(ICI.s)),]  # randomize the order of the data
dist.ICI <- dist(ICI.shuffle, method = "euclidean") # compute distance matrix
hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters


# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(sol))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(sol)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))


# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hc)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hc) 
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(table(sol))
clOrder <- sol[as.numeric(dend_dat$labels$label)]
clOrder <- unique(clOrder)
clusterSizes <- clusterSizes[clOrder]
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster = clOrder[sapply(dend_dat$segments$x, 
                                  function(xseg) sum(clusterSizes+0.5 <= xseg)+1)]) %>% 
  mutate(Cluster=tab$Label3lines[Cluster])

# Create dendrogram and output plot:
png("Figures/XF3a_hcaICIeucward.png", res = 600,
    height = 1500, width = 4000)
ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = Cluster)) +
  theme_classic() +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage (random order)") +
  guides(color = guide_legend("")) +
  ylab("Height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5),
        legend.position = "top")
dev.off()


### Euclidean/Ward, random order - box plot ------------

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of Factor2 variable
hca <- ICI.s %>%
  mutate(Cluster = sol)

hca.s <- hca %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            Factor2.m = mean(Factor2)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-Factor2.m)

# Rename arbitrary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
hca.r <- hca 

hca.l <- hca.r %>%
  mutate(Cluster=tab$Label2lines[Cluster]) %>% 
  gather("Scale", "Score", -Cluster) %>% 
  mutate(Score = Score * 100)

hca.lim <- hca.l %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

hca.out <- merge(hca.l, hca.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot
png("Figures/F7a_hcaICIeucward_box.png", res = 600,
    height = 2400, width = 3500)
ggplot(hca.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("Hierarchical with Euclidean distance and Ward's linkage\n(random order)") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()


# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


#### EXAMPLE 5: Basic k-means on ICI -----------------------------------------------
### k-means on ICI, random start #1 -------------------------------------

set.seed(1)                                         # set a random seed to recover original starting values
ran.centers <- sample(1:nrow(ICI.s), 3)             # define random centers to start the algorithm
sol <- kmeans(ICI.s, centers = ICI.s[ran.centers,]) # run the k-means clustering using the random centers

# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(sol$cluster))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(sol$cluster)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of Factor2 variable
kmn <- ICI.s %>%
  mutate(Cluster = as.character(sol$cluster)) 
kmn.s <- kmn %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            Factor2.m = mean(Factor2)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-Factor2.m)

# Rename arbitrary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
kmn.r <- kmn

kmn.l <- kmn.r %>%
  mutate(Cluster=tab$Label2lines[as.numeric(Cluster)]) %>% 
  gather("Scale", "Score", -Cluster) %>% 
  mutate(Score = Score*100)

kmn.lim <- kmn.l %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

kmn.out <- merge(kmn.l, kmn.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot (Figure 5)
png("Figures/F8_kmneucorig.png", res = 600,
    height = 2400, width = 3500)
ggplot(kmn.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(data = kmn.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("k-means with euclidean distance (random centers #1)") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()

kmn.r %>%
  group_by(Cluster) %>%
  summarise(N = n(),
            Factor1.m = mean(Factor1),
            Factor1.s = sd(Factor1),
            Factor2.m = mean(Factor2),
            Factor2.s = sd(Factor2),
            Factor3.m = mean(Factor3),
            Factor3.s = sd(Factor3),
            Factor4.m = mean(Factor4),
            Factor4.s = sd(Factor4))


### k-means on ICI, random start #2 ------------------

# The only difference between this code and the one to produce Figure 5 is changing the seed, which pulls a different
# set of random starting values

set.seed(45)                                         # set a random seed difference than original
ran.centers <- sample(1:nrow(ICI.s), 3)             # define random centers to start the algorithm
sol <- kmeans(ICI.s, centers = ICI.s[ran.centers,]) # run the k-means clustering using the random centers

# Save a table of group sizes/percent for which to rename cluster groupings
tab <- tibble(as.vector(table(sol$cluster))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(sol$cluster)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

# Combine assigned clusters into original data and manipulate data to count total sizes as a percent, order by
# increase mean of Factor2 variable
kmn <- ICI.s %>%
  mutate(Cluster = as.character(sol$cluster)) 
kmn.s <- kmn %>%
  group_by(Cluster) %>%
  summarize(n = n(),
            Factor2.m = mean(Factor2)) %>%
  mutate(n = round(n/nrow(ICI.s)*100, 1)) %>%
  arrange(-Factor2.m)

# Rename arbitrary Clusters 1, 2, and 3 to include their cluster sizes, then convert to long form for ggplot
kmn.r <- kmn

kmn.l <- kmn.r %>%
  mutate(Cluster=tab$Label2lines[as.numeric(Cluster)]) %>% 
  gather("Scale", "Score", -Cluster) %>% 
  mutate(Score = Score*100)

kmn.lim <- kmn.l %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

kmn.out <- merge(kmn.l, kmn.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot
png("Figures/SF10_kmneucdiffcenters.png", res = 600,
    height = 2400, width = 3500)
ggplot(kmn.l, aes(x = Scale, y = Score, fill = Cluster, color = Cluster)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(data = kmn.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol=2) +
  guides(fill = "none", color = "none") +
  ggtitle("k-means with euclidean distance (random centers #2)") +
  labs(x=NULL, y="Score")+
  theme_classic()
dev.off()

kmn.r %>%
  group_by(Cluster) %>%
  summarise(N = n(),
            Factor1.m = mean(Factor1),
            Factor1.s = sd(Factor1),
            Factor2.m = mean(Factor2),
            Factor2.s = sd(Factor2),
            Factor3.m = mean(Factor3),
            Factor3.s = sd(Factor3),
            Factor4.m = mean(Factor4),
            Factor4.s = sd(Factor4))

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS")))


#### EXAMPLE 6: Descriptive distribution across possible scores --------------

# Generate a table of how many (percent) students chose each response possibility for each of the four variables
round(table(ICI.s$Factor1) / nrow(ICI.s) * 100, 1)
round(table(ICI.s$Factor2) / nrow(ICI.s) * 100, 1)
round(table(ICI.s$Factor3) / nrow(ICI.s) * 100, 1)
round(table(ICI.s$Factor4) / nrow(ICI.s) * 100, 1)


#### EXAMPLE 7: Determining appropriate number of clusters -------------------

# For these figures, the x-axis is always 1 through 8, but we need to calculate the Within SS (A),
# Avg. Silhouette (B), and Gap Statistic plus standard deviation (C). The following code does this and
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

### Elbow method (A) --------------------------------------------

# Manipulate the data with all number of clusters information for plotting: 
clustexA <- clustex %>%
  select(Cluster, E_WSS, W_WSS) %>%
  gather("Method", "WSS", -Cluster) %>%
  mutate(Method = recode(Method, E_WSS = "K-means", W_WSS = "Hierarchical"))

# Plot the data; saved to an object because Figure 6 has multiple plots to be used in ggarrange below
figA <- ggplot(clustexA, aes(x = Cluster, y = WSS, color = Method, group = Method)) + 
  geom_point() + 
  geom_line() + 
  ylab("Within SS") +
  scale_color_manual(values = viridis(3, option = "D")[1:2]) +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("A") +
  guides(color = "none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
print(figA) # if you want to view the plot alone

### Average Silhouette method (B) -------------------------------

# Manipulate the data with all number of clusters information for plotting: 
clustexB <- clustex %>%
  select(Cluster, E_Sil, W_Sil) %>%
  gather("Method", "Sil", -Cluster) %>%
  mutate(Method = recode(Method, E_Sil = "K-means", W_Sil = "Hierarchical"))

# Plot the data; saved to an object because Figure 6 has multiple plots to be used in ggarrange below
figB <- ggplot(clustexB, aes(x = Cluster, y = Sil, color = Method, group = Method)) + 
  geom_point() + 
  geom_line() + 
  ylab("Avg Silhouette") +
  scale_color_manual(values = viridis(3, option = "D")[1:2]) +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("B") +
  guides(color = "none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
print(figB) # if you want to view the plot alone

### Gap Statistic method (C) ------------------------------------

# Manipulate the data with all number of clusters information for plotting: 
clustexC <- clustex %>%
  select(Cluster, E_Gap, W_Gap) %>%
  gather("Method", "Gap", -Cluster) %>%
  mutate(Method = recode(Method, E_Gap = "K-means", W_Gap = "Hierarchical"),
         High = c(clustex$E_GapHigh, clustex$W_GapHigh),
         Low = c(clustex$E_GapLow, clustex$W_GapLow))

# Plot the data; saved to an object because Figure 6 has multiple plots to be used in ggarrange below
figC <- ggplot(clustexC, aes(x = Cluster, y = Gap, color = Method, group = Method)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Low, ymax = High), width = .2) +
  geom_line() + 
  ylab("Gap Statistic") +
  scale_color_manual(values = viridis(3, option = "D")[1:2]) +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("C") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
print(figC) # if you want to view the plot alone

# To view all on one plot, use ggarrange to put them altoether:
png("Figures/XF4_NoClusters.png", res = 600, height = 1500, width = 5000)
ggarrange(figA, figB, figC, nrow = 1, widths = c(1.25,1.25,2))
dev.off()

### Number of clusters suggested by various indices -------------

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


#### * OPTIMAL CLUSTERING ON ICI * ---------------------------------

# First, define the number of clusters you wish to analyze; which is better to do one at a time because of time constraints.
# Also, we will think about how many times we want to iterate the analysis (iterations), so we can easily change this.

ksels <- 3:6             # Numbers of clusters to be calculated
kpref <- 6               # 6 is the suggested preferred number of clusters
iterations <- 1000       # 1000 is the suggested number of iterations


# For each of the numbers of clusters to compute...
ksel <- 6
for(ksel in ksels) {
  
#### EXAMPLE 8: Optimal HCA ---------------

    # /////////////////// WARNING WARNING WARNING /////////////////// #

# WARNING: This section of code will take a considerable amount of time to run (2-10 hours depending
# on the quality of your computer's processor)! If you want to verify the code is working, try changing the 
# number of iterations to a smaller number, like 5, then change back to the desired value once it is working.

### Conducting repetitions and saving results ----------------------

if(CALC_REPS | 
   !file.exists(paste0("hca_",ksel,"_",iterations,"_reps.rda"))) {
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
  
  save(clusterings, file=paste0("hca_",ksel,"_",iterations,"_reps.rda"))
}

# If you having previously run the code, you will need to call in the save results, otherwise, you can 
# comment the line out:
load(paste0("hca_",ksel,"_",iterations,"_reps.rda"))
clusteringshca <- clusterings
Names1 <- c("Factor1", "Factor2", "Factor3", "Factor4", "Cluster")
clusteringshca$df <- lapply(clusteringshca$df, `names<-`, Names1)
Names2 <- c("Cluster", "Factor1", "Factor2", "Factor3", "Factor4")
clusteringshca$centers <- lapply(clusteringshca$centers, `names<-`, Names2)

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s", "clusteringshca", "CALC_REPS",
                        "ksel", "kpref","iterations")))


### Finding number of unique solutions -----------------------------------

# Examine how many unique total sum of squares to gauge how many unique solutions were obtained based on 
# total sums of squares or average silhouette:
length(unique(unlist(clusteringshca$totss)))
length(unique(round(unlist(clusteringshca$avgsil),6)))

# Count unique by fingerprint each partition
fingerprint <- as.data.frame(t(apply(clusteringshca, 1, function(x)
  c(x$wss, as.numeric(table(x$df[,5]))))))
fingerprint <- as.data.frame(t(apply(fingerprint,1,sort)))
fingerprint <- round(fingerprint, 1)
fingerprint <- apply(fingerprint, 1, paste, collapse=" ")
length(unique(fingerprint))

### Selecting best HCA solution ------------------

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
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

#### EXAMPLE 9: Visualizing HCA solution -------------------

### HCA solution, box plot (cluster facets) ---------------------------------

hca.box <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster]) %>%
  pivot_longer(-Cluster, names_to = "Scale", values_to = "Score") %>%
  mutate(Score = Score*100)

hca.lim <- hca.box %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

hca.out <- merge(hca.box, hca.lim) %>% 
  filter(Score > upperW | Score < lowerW)

if(ksel==kpref) {
  png(paste0("Figures/F9_hca",ksel,"box.png"),
          height = ifelse(ksel>4, 2500, 2000), width = 3500, res = 600)
} else {
  png(paste0("Figures/SF15_hca",ksel,"box.png"),
      height = ifelse(ksel>4, 2500, 2000), width = 3500, res = 600)
}

g <- ggplot(hca.box, aes(x = Scale, fill = Cluster, color = Cluster, y = Score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol = 2) +
  guides(fill = "none", color = "none") +
  labs(x=NULL, y="Score")+
  theme_classic()
print(g)
dev.off()


### HCA solution, box plot (factor facets) ---------------------------------

hca.box <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label3lines[Cluster]) %>%
  pivot_longer(-Cluster, names_to = "Scale", values_to = "Score") %>%
  mutate(Score = Score*100)

hca.lim <- hca.box %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

hca.out <- merge(hca.box, hca.lim) %>% 
  filter(Score > upperW | Score < lowerW)

# Create/Output the plot
png(paste0("Figures/SF2_hca",ksel,"box2.png"),
    height = ifelse(ksel>4, 5000, 2000), width = 3500, res = 600)
g <- ggplot(hca.box, aes(x = Cluster, fill = Cluster, color = Cluster, y = Score)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(data = hca.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Scale) +
  guides(fill = "none", color = "none") +
  labs(x=NULL, y="Score")+
  theme_classic()
if(ksel>4) g <- g +facet_wrap(~Scale, ncol= 1)
print(g)       
dev.off()

### HCA solution, centers heatmap ---------------------------------

# Pull just the centers to plot a heat map
hca.heat <- clusteringshca$centers[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label3lines[Cluster]) %>%
  pivot_longer(-Cluster, names_to = "Scale", values_to = "Score") %>%
  mutate(Score = round(Score * 100, digits = 1))
hca.heat$Scale <- as.factor(hca.heat$Scale)

# Create/Output the plot
png(paste0("Figures/SF3_hca",ksel,"heat.png"),
    height = 1500, width = ifelse(ksel>4,3500,2500), res = 600)
g <- ggplot(hca.heat,aes(x = Cluster, y = Scale, fill = Score,
                    label = Score)) +
  geom_tile() +
  geom_text(aes(color = Score > 30)) +
  xlab(NULL) + ylab(NULL) +
  labs(fill = "Avg\nScore") +
  scale_fill_viridis(direction=-1) +
  scale_x_discrete() +
  scale_y_discrete(limits=rev(levels(hca.heat$Scale))) +
  scale_color_manual(values=c("black","white")) +
  guides(color = "none") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())
print(g)
dev.off()


### HCA solution, simplified scatter plots ---------------------------------

# Pull the data to plot a scatter plot
hca.scat <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Pull just the centers
hca.center <- clusteringshca$centers[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

clColors <- viridis(ksel)
lsGrobs <- vector(mode="list", ceiling(ksel/2)*4)

for(cl in 1:(ceiling(ksel/2)*4)) {
  lsGrobs[[cl]] <- as_ggplot(text_grob(""))
}

for(cl in 1:ksel) {
  clScat <- hca.scat[hca.scat$Cluster == tab$Label2lines[cl],]
  clCenter <- hca.center[hca.center$Cluster == tab$Label2lines[cl],]
  
  # Create/Output Plot #1
  hca.factor1.factor2 <- ggplot(clScat,
      aes(x = Factor1, y = Factor2)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=ifelse(ksel>4,0.6,0.5),
                color=clColors[cl], height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  # Create/Output Plot #2
  hca.factor3.factor4 <- ggplot(clScat, 
          aes(x = Factor3, y = Factor4)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=ifelse(ksel>4,0.6,0.5),
                color=clColors[cl], height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  gcl <- ggarrange(hca.factor1.factor2, hca.factor3.factor4,
                   widths = c(1,1))
  
  lsGrobs[[2*cl-1]] <- text_grob(
    gsub("\n",", ",tab$Label2lines[cl]))
  lsGrobs[[2*cl-1]] <- as_ggplot(lsGrobs[[2*cl-1]]) + 
    theme(plot.margin=margin(5,5,0,5),
          panel.background = element_rect(color="black", size=1))
  lsGrobs[[2*cl]] <- gcl + 
    theme(plot.margin=margin(0,5,5,5),
          panel.background = element_rect(color="black", size=1))
}

# Create/Output the plots together
png(paste0("Figures/SF4_hca",ksel,"scat.png"),
    height = ceiling(ksel/2) * 2000, width = 8000, res = 600)
grid.arrange(grobs=lsGrobs, ncol=2, nrow=ceiling(ksel/2)*2,
          heights=rep(c(1,5),ceiling(ksel/2)), 
          layout_matrix=matrix(c(sort(c(seq(1,ceiling(ksel/2)*4,4),
                                      seq(2,ceiling(ksel/2)*4,4))),
                                 sort(c(seq(3,ceiling(ksel/2)*4,4),
                                      seq(4,ceiling(ksel/2)*4,4)))),
                                 ncol=2, nrow=ceiling(ksel/2)*2),
          padding=2)
dev.off()


### HCA solution, simplified scatter plots (black border) ---------------------------------

# Pull the data to plot a scatter plot
hca.scat <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Pull just the centers
hca.center <- clusteringshca$centers[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

clColors <- viridis(ksel)
lsGrobs <- vector(mode="list", ceiling(ksel/2)*4)

for(cl in 1:(ceiling(ksel/2)*4)) {
  lsGrobs[[cl]] <- as_ggplot(text_grob(""))
}

for(cl in 1:ksel) {
  clScat <- hca.scat[hca.scat$Cluster == tab$Label2lines[cl],]
  clCenter <- hca.center[hca.center$Cluster == tab$Label2lines[cl],]
  
  # Create/Output Plot #1
  hca.factor1.factor2 <- ggplot(clScat,
                                aes(x = Factor1, y = Factor2)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=.3, color="black",
                fill=clColors[cl],
                height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none", fill="none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  # Create/Output Plot #2
  hca.factor3.factor4 <- ggplot(clScat, 
                                aes(x = Factor3, y = Factor4)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=.3, color="black",
                fill=clColors[cl],
                height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none", fill="none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  gcl <- ggarrange(hca.factor1.factor2, hca.factor3.factor4,
                   widths = c(1,1))
  
  lsGrobs[[2*cl-1]] <- text_grob(
    gsub("\n",", ",tab$Label2lines[cl]))
  lsGrobs[[2*cl-1]] <- as_ggplot(lsGrobs[[2*cl-1]]) + 
    theme(plot.margin=margin(5,5,0,5),
          panel.background = element_rect(color="black", size=1))
  lsGrobs[[2*cl]] <- gcl + 
    theme(plot.margin=margin(0,5,5,5),
          panel.background = element_rect(color="black", size=1))
}

# Create/Output the plots together
png(paste0("Figures/SF4_hca",ksel,"scat_bk.png"),
    height = ceiling(ksel/2) * 2000, width = 8000, res = 600)
grid.arrange(grobs=lsGrobs, ncol=2, nrow=ceiling(ksel/2)*2,
             heights=rep(c(1,5),ceiling(ksel/2)), 
             layout_matrix=matrix(c(sort(c(seq(1,ceiling(ksel/2)*4,4),
                                           seq(2,ceiling(ksel/2)*4,4))),
                                    sort(c(seq(3,ceiling(ksel/2)*4,4),
                                           seq(4,ceiling(ksel/2)*4,4)))),
                                  ncol=2, nrow=ceiling(ksel/2)*2),
             padding=2)
dev.off()


### HCA solution, simplified heatmaps ---------------------------------

# Pull the data to plot the heatmaps
hca.hmap <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

lsGrobs <- vector(mode="list", ceiling(ksel/2)*4)

for(cl in 1:(ceiling(ksel/2)*4)) {
  lsGrobs[[cl]] <- as_ggplot(text_grob(""))
}

for(cl in 1:ksel) {
  clHm <- hca.hmap[hca.hmap$Cluster == tab$Label2lines[cl],]
  
  hca.count <- clHm %>% select(Factor1, Factor2) %>% 
    group_by(Factor1, Factor2) %>%
    summarise(cnt = n(), .groups="drop") %>% 
    mutate(prc = round(cnt / nrow(clHm) * 100, 0))
  hca.count$prc[hca.count$prc>50] <- 50
  
  factor1.factor2 <- ggplot(hca.count,
          aes(x = Factor1, y = Factor2, fill = prc)) + # Make the plot
    geom_tile(width=12.5, height=25) +
    scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_fill_viridis(discrete = FALSE, direction=-1,
                       name="Percent over\ncluster size",
                       limits=c(0,50),
                       breaks=(0:5)*10) +
    theme_classic() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          legend.position="none",
          plot.background = element_rect(fill=NA, color=NA))
  
  leg <- factor1.factor2 + 
    guides(fill = guide_colorbar(reverse=TRUE)) +
    theme(legend.position = "left",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.background = element_rect(fill=NA),
          legend.box.margin = margin(0,5,0,0))
  leg <- get_legend(leg)
  
  # Factor3 v Factor4
  hca.count <- clHm %>% select(Factor3, Factor4) %>% 
    group_by(Factor3, Factor4) %>%
    summarise(cnt = n(), .groups="drop")  %>% 
    mutate(prc = round(cnt / nrow(clHm) * 100, 0))
  hca.count$prc[hca.count$prc>50] <- 50
  
  factor3.factor4 <- ggplot(hca.count, 
                            aes(x = Factor3, y = Factor4, fill = prc)) + # Make the plot
    geom_tile(width=25, height=25) +
    scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_fill_viridis(discrete = FALSE, direction=-1,
                       name="Percent over\ncluster size",
                       limits=c(0,50),
                       breaks=(0:5)*10) +
    theme_classic() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          legend.position="none",
          plot.background = element_rect(fill=NA, color=NA))
  
  
    gcl <- ggarrange(factor1.factor2, factor3.factor4,
                     widths = c(1,1), ncol=2,
                     legend.grob=leg,
                     legend="right")
  
  lsGrobs[[2*cl-1]] <- text_grob(
    gsub("\n",", ",tab$Label2lines[cl]))
  lsGrobs[[2*cl-1]] <- as_ggplot(lsGrobs[[2*cl-1]]) + 
    theme(plot.margin=margin(5,5,0,5),
          panel.background = element_rect(color="black", size=1))
  lsGrobs[[2*cl]] <- gcl + 
    theme(plot.margin=margin(0,5,5,5),
          panel.background = element_rect(color="black", size=1))
}

# Create/Output the plots together
png(paste0("Figures/SF6_hca",ksel,"heatmaps.png"),
    height = ceiling(ksel/2) * 2000, width = 8000, res = 600)
grid.arrange(grobs=lsGrobs, ncol=2, nrow=ceiling(ksel/2)*2,
             heights=rep(c(1,5),ceiling(ksel/2)), 
             layout_matrix=matrix(c(sort(c(seq(1,ceiling(ksel/2)*4,4),
                                           seq(2,ceiling(ksel/2)*4,4))),
                                    sort(c(seq(3,ceiling(ksel/2)*4,4),
                                           seq(4,ceiling(ksel/2)*4,4)))),
                                  ncol=2, nrow=ceiling(ksel/2)*2),
             padding=2)
dev.off()


### HCA solution, dendrogram ---------------------------------

dist.ICI <- dist(ICI.s, method = "euclidean") # compute distance matrix
# hc <- hclust(dist.ICI, method = "ward.D2")    # run hca algorithm
# sol <- cutree(hc, 3)                          # cut dendrogram into 3 clusters

set.seed(bestsoln_sil)
ranorder <- sample(1:nrow(ICI.s),nrow(ICI.s))
df1 <- ICI.s[ranorder,]
distData <- dist(df1, method="euclidean")
hcbest <- agnes(distData, diss=T, method="ward")
sol <- cutree(hcbest, ksel)

# Will build the dendrogram by hand to plot in ggplot and customize colors. If you want default dendrogram,
# simply use:
#   plot(hcbest)

# Create data that holds segments and labels: 
dend <- as.dendrogram(hcbest) 
dend_dat <- dendro_data(dend, type = "rectangle")

# Set the color by the initial position of the segment (x)
clusterSizes <- as.numeric(table(sol))
clOrder <- sol[as.numeric(dend_dat$labels$label)]
clOrder <- unique(clOrder)
clusterSizes <- clusterSizes[clOrder]
clusterSizes <- cumsum(clusterSizes)
dend.cust <- dend_dat$segments %>%
  mutate(Cluster = clOrder[sapply(dend_dat$segments$x, 
                          function(xseg) sum(clusterSizes+0.5 <= xseg)+1)]) %>% 
  mutate(Cluster=tab$Label3lines[Cluster])

# Create dendrogram and output plot:
png(paste0("Figures/SF7_hca",ksel,"dendro.png"), res = 600,
    height = 1500, width = 4000)
g <- ggplot(dend.cust) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend,
                   color = Cluster)) +
  theme_classic() +
  guides(color=guide_legend("")) +
  ylab("Height") +
  scale_colour_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5),
        legend.position = "top")
print(g)
dev.off()

#### EXAMPLE 10: Silhouette Plot for HCA ---------------------------------------------

# Pull required information from silhouette values
hca.sil <- clusteringshca$df[[bestsoln_sil]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Silhouette = clusteringshca$sil[[bestsoln_sil]]) %>%
  arrange(Cluster, -Silhouette)

png(paste0("Figures/F8_hca",ksel,"sil.png"), height = 1500, width = 2500, res = 600)
g <- ggplot(hca.sil, aes(x = 1:nrow(hca.sil), y = Silhouette, color = Cluster)) +
  geom_segment(aes(xend = 1:nrow(hca.sil), yend = 0)) +
  geom_hline(yintercept = mean(hca.sil$Silhouette), lty = 2) +
  xlab(NULL) +
  theme_classic() +
  scale_color_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
print(g)
dev.off()


#### EXAMPLE 11: Bootstrap samples to determine stability of HCA ------------------------------------

# /////////////////// WARNING WARNING WARNING /////////////////// #

# WARNING: This section of code will take a considerable amount of time to run (2-10 hours depending
# on the quality of your computer's processor)! If you want to verify the code is working, try changing the 
# number of iterations to a smaller number, like 5, then change back to 1,000 once it is working.

### Conducting bootstrap samples and saving results ----------------------

# We have not yet gotten rid of our previous environment, so we still have an object that houses the "best"
# solution according to silhouette or within SS, whichever we chose. We will use this solution as the default
# solution to compare bootstrap examples.
bestsoln <- clusteringshca$df[[bestsoln_sil]]

if(CALC_REPS | 
   !file.exists(paste0("hca_",ksel,"_",iterations,"_boot.rda"))) {
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
  
  save(clusterings, file=paste0("hca_",ksel,"_",iterations,"_boot.rda"))
}

### Adjusted Rand-Index distribution for HCA --------

# Load the saved data and pull the ARI
load(paste0("hca_",ksel,"_",iterations,"_boot.rda"))
boothca <- clusterings
boothca.ari <- data.frame(ARI = unlist(boothca$ari))

# Plot/Output the desired graph:
png(paste0("Figures/SF8_hca",ksel,"boot_ari.png"), height = 1500, width = 2000, res = 600)
g <- ggplot(boothca.ari, aes(x = ARI)) +
  geom_histogram(fill = "grey80", color = "black", binwidth = 0.025,
                 boundary=1) + 
  geom_density(aes(y = ..density..*1000*0.025)) +
  ylab("Count") + xlab("Adjusted Rand Index") +
  scale_x_continuous(limits=c(0,1))+
  theme_classic()
print(g)
dev.off()

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s","CALC_REPS",
                        "ksel", "kpref", "iterations")))

#### EXAMPLE 12: Optimal k-means analyses --------------------------------------------

# This section contains everything done for hierarchical results, except now for k-means analyses. The code is very
# similar to that of the hierarchical, so much of it is not commented; see the hierarchical example for the rationale
# behind code.

# Unlike HCA, k-means is quite a fast algorithm, this section of code should have no problem running in ~5 minutes or less.

### Conducting repetitions and saving results ----------------------

distData <- dist(ICI.s, method="euclidean") # dissimiliarity matrix to calculate silhouette
df1 <- ICI.s                                # temp data set to avoid overwriting actual data in loop to come

if(CALC_REPS  | 
   !file.exists(paste0("kmn_",ksel,"_",iterations,"_reps.rda"))) {
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
  
  save(clusterings, file=paste0("kmn_",ksel,"_",iterations,"_reps.rda"))
}

# If you having previously run the code, you will need to call in the save results, otherwise, you can 
# comment the line out:
load(paste0("kmn_",ksel,"_",iterations,"_reps.rda"))
clusteringskmn <- clusterings
Names1 <- c("Factor1", "Factor2", "Factor3", "Factor4", "Cluster")
clusteringskmn$df <- lapply(clusteringskmn$df, `names<-`, Names1)
Names2 <- c("Cluster", "Factor1", "Factor2", "Factor3", "Factor4")
clusteringskmn$centers <- lapply(clusteringskmn$centers, `names<-`, Names2)

# Delete unnecessary objects for next code (to prevent environment from being bogged down or overcrowded):
rm(list=setdiff(ls(), c("ICI.s", "clusteringskmn", "CALC_REPS",
                        "ksel", "kpref", "iterations")))

### Finding number of unique solutions -----------------------------------

length(unique(unlist(clusteringskmn$totss)))
length(unique(round(unlist(clusteringskmn$avgsil),6)))

# Count unique by fingerprint each partition
fingerprint <- as.data.frame(t(apply(clusteringskmn, 1, function(x) {
  c(as.numeric(table(x$df$Cluster)),
    round(as.numeric(x$centers$Factor1),3))
})))
fingerprint <- as.data.frame(t(apply(fingerprint,1,sort)))
fingerprint <- round(fingerprint,1)
fingerprint <- apply(fingerprint, 1, paste, collapse=" ")
length(unique(fingerprint))

### Selecting best k-means solution ------------------

bestsoln_sil <- which.max(clusteringskmn$avgsil)  # optimal solution based on silhouette
bestsoln_totss <- which.min(clusteringskmn$totss) # optimal solution based on total (within) ss
bestsoln_bwss <- which.max(clusteringskmn$bwss)  # optimal solution based on total (between) ss

tab <- tibble(as.vector(table(clusteringskmn$df[[bestsoln_totss]]$Cluster))) %>%
  rename(Size = 1) %>%
  mutate(Cluster = 1:length(unique(clusteringskmn$df[[bestsoln_totss]]$Cluster)),
         Percent = round((Size / sum(Size)) * 100, digits = 1)) %>% 
  mutate(Label2lines = paste0("Cluster ", Cluster, "\nN = ", Size, " (",
                              Percent, "%)"),
         Label3lines = paste0("Cluster ", Cluster, "\nN = ", Size, "\n(",
                              Percent, "%)"))

### Visualizing k-means solution - box plot (cluster facet) ------------------

kmn.box <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster]) %>%
  pivot_longer(-Cluster, names_to = "Scale", values_to = "Score") %>%
  mutate(Score = Score * 100)

kmn.lim <- kmn.box %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

kmn.out <- merge(kmn.box, kmn.lim) %>% 
  filter(Score > upperW | Score < lowerW)

png(paste0("Figures/F9_kmn",ksel,"box.png"),
    height = ifelse(ksel>4, 2500, 2000), width = 3500, res = 600)
g <- ggplot(kmn.box, aes(x = Scale, fill = Cluster, color = Cluster, y = Score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = kmn.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Cluster, ncol = 2) +
  guides(fill = "none", color = "none") +
  labs(x=NULL, y="Score")+
  theme_classic()
print(g)
dev.off()

### Visualizing k-means solution - box plot (factor facet) ------------------

kmn.box <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label3lines[Cluster]) %>%
  pivot_longer(-Cluster, names_to = "Scale", values_to = "Score") %>%
  mutate(Score = Score * 100)

kmn.lim <- kmn.box %>% group_by(Cluster, Scale) %>% 
  summarise(lowerW = quantile(Score, 0.25) - 1.5 * IQR(Score),
            upperW =quantile(Score, 0.75) + 1.5 * IQR(Score),
            .groups="drop")

kmn.out <- merge(kmn.box, kmn.lim) %>% 
  filter(Score > upperW | Score < lowerW)

png(paste0("Figures/SF7_kmn",ksel,"box2.png"),
    height = ifelse(ksel>4, 5000, 2000), width = 3500, res = 600)
g <- ggplot(kmn.box, aes(x = Cluster, fill = Cluster, color = Cluster, y = Score)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(data = kmn.out, shape=21, alpha=0.3,
              height=0, width=0.2, color="black") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~Scale) +
  guides(fill = "none", color = "none") +
  labs(x=NULL, y="Score")+
  theme_classic()
if(ksel>4) g <- g +facet_wrap(~Scale, ncol= 1)
print(g)       
dev.off()

### Visualizing k-means solution - centers heatmap ------------------

# Pull just the centers to plot a heat map
kmn.heat <- clusteringskmn$centers[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label3lines[Cluster]) %>%
  pivot_longer(-Cluster, names_to = "Scale", values_to = "Score") %>%
  mutate(Score = round(Score * 100, digits = 1))
kmn.heat$Scale <- as.factor(kmn.heat$Scale)

# Create/Output the plot
png(paste0("Figures/SF8_kmn",ksel,"heat.png"), 
    height = 1500, width = ifelse(ksel>4, 3500, 2500), res = 600)
g <- ggplot(kmn.heat,aes(x = Cluster, y = Scale, fill = Score,
                    label = Score)) +
  geom_tile() +
  geom_text(aes(color = Score > 30)) +
  xlab(NULL) + ylab(NULL) +
  labs(fill = "Avg\nScore") +
  scale_fill_viridis(direction=-1) +
  scale_x_discrete() +
  scale_y_discrete(limits=rev(levels(kmn.heat$Scale))) +
  scale_color_manual(values=c("black","white")) +
  guides(color = "none") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())
print(g)
dev.off()

### Visualizing k-means solution - simplified scatter plots ------------------

# Pull the data to plot a scatter plot
kmn.scat <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Pull just the centers
kmn.center <- clusteringskmn$centers[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

clColors <- viridis(ksel)
lsGrobs <- vector(mode="list", ceiling(ksel/2)*4)

for(cl in 1:(ceiling(ksel/2)*4)) {
  lsGrobs[[cl]] <- as_ggplot(text_grob(""))
}

for(cl in 1:ksel) {
  clScat <- kmn.scat[kmn.scat$Cluster == tab$Label2lines[cl],]
  clCenter <- kmn.center[kmn.center$Cluster == tab$Label2lines[cl],]
  
  # Create/Output Plot #1
  kmn.factor1.factor2 <- ggplot(clScat,
                                aes(x = Factor1, y = Factor2)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=ifelse(ksel>4,0.6,0.5),
                color=clColors[cl], height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  # Create/Output Plot #2
  kmn.factor3.factor4 <- ggplot(clScat, 
                                aes(x = Factor3, y = Factor4)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=ifelse(ksel>4,0.6,0.5),
                color=clColors[cl], height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  gcl <- ggarrange(kmn.factor1.factor2, kmn.factor3.factor4,
                   widths = c(1,1))
  
  lsGrobs[[2*cl-1]] <- text_grob(
    gsub("\n",", ",tab$Label2lines[cl]))
  lsGrobs[[2*cl-1]] <- as_ggplot(lsGrobs[[2*cl-1]]) + 
    theme(plot.margin=margin(5,5,0,5),
          panel.background = element_rect(color="black", size=1))
  lsGrobs[[2*cl]] <- gcl + 
    theme(plot.margin=margin(0,5,5,5),
          panel.background = element_rect(color="black", size=1))
}

# Create/Output the plots together
png(paste0("Figures/SF9_kmn",ksel,"scat.png"),
    height = ceiling(ksel/2) * 2000, width = 8000, res = 600)
grid.arrange(grobs=lsGrobs, ncol=2, nrow=ceiling(ksel/2)*2,
             heights=rep(c(1,5),ceiling(ksel/2)), 
             layout_matrix=matrix(c(sort(c(seq(1,ceiling(ksel/2)*4,4),
                                           seq(2,ceiling(ksel/2)*4,4))),
                                    sort(c(seq(3,ceiling(ksel/2)*4,4),
                                           seq(4,ceiling(ksel/2)*4,4)))),
                                  ncol=2, nrow=ceiling(ksel/2)*2),
             padding=2)
dev.off()


### Visualizing k-means solution - simplified scatter plots (black border) ------------------

# Pull the data to plot a scatter plot
kmn.scat <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

# Pull just the centers
kmn.center <- clusteringskmn$centers[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

clColors <- viridis(ksel)
lsGrobs <- vector(mode="list", ceiling(ksel/2)*4)

for(cl in 1:(ceiling(ksel/2)*4)) {
  lsGrobs[[cl]] <- as_ggplot(text_grob(""))
}

for(cl in 1:ksel) {
  clScat <- kmn.scat[kmn.scat$Cluster == tab$Label2lines[cl],]
  clCenter <- kmn.center[kmn.center$Cluster == tab$Label2lines[cl],]
  
  # Create/Output Plot #1
  kmn.factor1.factor2 <- ggplot(clScat,
                                aes(x = Factor1, y = Factor2)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=.3, color="black",
                fill=clColors[cl],
                height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none", fill="none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  # Create/Output Plot #2
  kmn.factor3.factor4 <- ggplot(clScat, 
                                aes(x = Factor3, y = Factor4)) +
    geom_encircle(color=clColors[cl]) +
    geom_jitter(shape=21, alpha=.3, color="black",
                fill=clColors[cl],
                height=5, width=5)+
    geom_point(data=clCenter, size=5, shape="+",
               color="black") +
    scale_x_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    scale_y_continuous(breaks=seq(0,100,by=25),
                       limits=c(-10,110)) + 
    guides(color = "none", fill="none") +
    theme_classic() +
    theme(plot.background = element_rect(fill=NA, color=NA))
  
  gcl <- ggarrange(kmn.factor1.factor2, kmn.factor3.factor4,
                   widths = c(1,1))
  
  lsGrobs[[2*cl-1]] <- text_grob(
    gsub("\n",", ",tab$Label2lines[cl]))
  lsGrobs[[2*cl-1]] <- as_ggplot(lsGrobs[[2*cl-1]]) + 
    theme(plot.margin=margin(5,5,0,5),
          panel.background = element_rect(color="black", size=1))
  lsGrobs[[2*cl]] <- gcl + 
    theme(plot.margin=margin(0,5,5,5),
          panel.background = element_rect(color="black", size=1))
}

# Create/Output the plots together
png(paste0("Figures/SF9_kmn",ksel,"scat_bk.png"),
    height = ceiling(ksel/2) * 2000, width = 8000, res = 600)
grid.arrange(grobs=lsGrobs, ncol=2, nrow=ceiling(ksel/2)*2,
             heights=rep(c(1,5),ceiling(ksel/2)), 
             layout_matrix=matrix(c(sort(c(seq(1,ceiling(ksel/2)*4,4),
                                           seq(2,ceiling(ksel/2)*4,4))),
                                    sort(c(seq(3,ceiling(ksel/2)*4,4),
                                           seq(4,ceiling(ksel/2)*4,4)))),
                                  ncol=2, nrow=ceiling(ksel/2)*2),
             padding=2)
dev.off()


### Visualizing k-means solution - simplified heatmaps ------------------

# Pull the data to plot the heatmaps
kmn.hmap <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Factor1 = Factor1*100,
         Factor2 = Factor2*100,
         Factor3 = Factor3*100,
         Factor4 = Factor4*100)

lsGrobs <- vector(mode="list", ceiling(ksel/2)*4)

for(cl in 1:(ceiling(ksel/2)*4)) {
  lsGrobs[[cl]] <- as_ggplot(text_grob(""))
}

for(cl in 1:ksel) {
  clHm <- kmn.hmap[kmn.hmap$Cluster == tab$Label2lines[cl],]
  
  kmn.count <- clHm %>% select(Factor1, Factor2) %>% 
    group_by(Factor1, Factor2) %>%
    summarise(cnt = n(), .groups="drop") %>% 
    mutate(prc = round(cnt / nrow(clHm) * 100, 0))
  kmn.count$prc[kmn.count$prc>50] <- 50
  
  factor1.factor2 <- ggplot(kmn.count,
                            aes(x = Factor1, y = Factor2, fill = prc)) + # Make the plot
    geom_tile(width=12.5, height=25) +
    scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_fill_viridis(discrete = FALSE, direction=-1,
                       name="Percent over\ncluster size",
                       limits=c(0,50),
                       breaks=(0:5)*10) +
    theme_classic() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          legend.position="none",
          plot.background = element_rect(fill=NA, color=NA))
  
  leg <- factor1.factor2 + 
    guides(fill = guide_colorbar(reverse=TRUE)) +
    theme(legend.position = "left",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.background = element_rect(fill=NA),
          legend.box.margin = margin(0,5,0,0))
  leg <- get_legend(leg)
  
  # Factor3 v Factor4
  kmn.count <- clHm %>% select(Factor3, Factor4) %>% 
    group_by(Factor3, Factor4) %>%
    summarise(cnt = n(), .groups="drop")  %>% 
    mutate(prc = round(cnt / nrow(clHm) * 100, 0))
  kmn.count$prc[kmn.count$prc>50] <- 50
  
  factor3.factor4 <- ggplot(kmn.count, 
                            aes(x = Factor3, y = Factor4, fill = prc)) + # Make the plot
    geom_tile(width=25, height=25)  +
    scale_x_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_y_continuous(breaks= 100 * seq(0,1,length.out = 5),
                       limits=c(-15,115))+
    scale_fill_viridis(discrete = FALSE, direction=-1,
                       name="Percent over\ncluster size",
                       limits=c(0,50),
                       breaks=(0:5)*10) +
    theme_classic() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          legend.position="none",
          plot.background = element_rect(fill=NA, color=NA))
  
  
  gcl <- ggarrange(factor1.factor2, factor3.factor4,
                   widths = c(1,1), ncol=2,
                   legend.grob=leg,
                   legend="right")
  
  lsGrobs[[2*cl-1]] <- text_grob(
    gsub("\n",", ",tab$Label2lines[cl]))
  lsGrobs[[2*cl-1]] <- as_ggplot(lsGrobs[[2*cl-1]]) + 
    theme(plot.margin=margin(5,5,0,5),
          panel.background = element_rect(color="black", size=1))
  lsGrobs[[2*cl]] <- gcl + 
    theme(plot.margin=margin(0,5,5,5),
          panel.background = element_rect(color="black", size=1))
}

# Create/Output the plots together
png(paste0("Figures/SF10_kmn",ksel,"heatmaps.png"),
    height = ceiling(ksel/2) * 2000, width = 8000, res = 600)
grid.arrange(grobs=lsGrobs, ncol=2, nrow=ceiling(ksel/2)*2,
             heights=rep(c(1,5),ceiling(ksel/2)), 
             layout_matrix=matrix(c(sort(c(seq(1,ceiling(ksel/2)*4,4),
                                           seq(2,ceiling(ksel/2)*4,4))),
                                    sort(c(seq(3,ceiling(ksel/2)*4,4),
                                           seq(4,ceiling(ksel/2)*4,4)))),
                                  ncol=2, nrow=ceiling(ksel/2)*2),
             padding=2)
dev.off()


### Visualizing k-means solution - silhouette plot ------------------

# Pull required information from silhouette values
kmn.sil <- clusteringskmn$df[[bestsoln_totss]] %>%
  mutate(Cluster = tab$Label2lines[Cluster],
         Silhouette = clusteringskmn$sil[[bestsoln_sil]]) %>%
  arrange(Cluster, -Silhouette)

png(paste0("Figures/SF11_kmn",ksel,"sil.png"), height = 1500, width = 2500, res = 600)
g <- ggplot(kmn.sil, aes(x = 1:nrow(kmn.sil), y = Silhouette, color = Cluster)) +
  geom_segment(aes(xend = 1:nrow(kmn.sil), yend = 0)) +
  geom_hline(yintercept = mean(kmn.sil$Silhouette), lty = 2) +
  xlab(NULL) +
  theme_classic() +
  scale_color_viridis(discrete = TRUE) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
print(g)
dev.off()

### Determining stability of k-means ------------------------ 
## . Conducting bootstrap samples and saving results ----------------------

bestsoln <- clusteringskmn$df[[bestsoln_totss]]

if(CALC_REPS  | 
   !file.exists(paste0("kmn_",ksel,"_",iterations,"_boot.rda"))) {
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
  
  save(clusterings, file=paste0("kmn_",ksel,"_",iterations,"_boot.rda"))
}

## . Adjusted Rand-Index distribution for k-means --------

load(paste0("kmn_",ksel,"_",iterations,"_boot.rda"))
bootkmn <- clusterings
bootkmn.ari <- data.frame(ARI = unlist(bootkmn$ari))

# Plot/Output the desired graph:
png(paste0("Figures/SF12_kmn",ksel,"boot_ari.png"), height = 1500, width = 2000, res = 600)
g <- ggplot(bootkmn.ari, aes(x = ARI)) +
  geom_histogram(fill = "grey80", color = "black", binwidth = 0.025,
                 boundary=1) + 
  geom_density(aes(y = ..density..*1000*0.025)) +
  ylab("Count") + xlab("Adjusted Rand Index") +
  scale_x_continuous(limits=c(0,1))+
  theme_classic()
print(g)
dev.off()

# End of the loop for number of clusters
}

