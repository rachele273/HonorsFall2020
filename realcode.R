#using code from Dr. Warfa "Latent Profile Analysis"
# install R packages.
install.packages("mclust")
install.packages("dplyr")
install.packages("readxl")
install.packages("reshape2")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("tidyLPA")
library(ggpubr)
library(mclust)
library(dplyr)
library(readxl)
library(reshape2)
library(factoextra)
library(ggplot2)
library(tidyLPA)
#imported dataset with frequencies--dataset has already been modified to contain only the columns of interest
# carry out LPA on percent time data matrix.
# VEI means that the clusters have variable volume, the same shape and orientation equal to coordinate axes.
mod1 <- Mclust(BioData)
summary(mod1)
summary(mod1$BIC) # Mclust VEI (diagonal, equal shape) model with 2 components
plot(mod1, what = "BIC", legendArgs = list(x = "bottomright", ncol=3))
# Bayesian Information Criterion (BIC) values for 14 models to CDOP data
library(factoextra)
fviz_mclust(mod1, "BIC", palette = "jco")
#remove "conn" column. this has 0 for all values and it is preventing the next line of code
BioData=subset(BioData,select=-c(conn))
install.packages("ggsci")
library(ggsci)
library(ggplot2)
library(gridExtra)
palette.pals()
#I was not able to load color palette jco
fviz_mclust(mod1, "classification", geom = "point", pointsize = 1.5, palette.colors(palette="Classic Tableau"))
fviz_mclust(mod1, "uncertainty", palette.colors(palette="Classic Tableau"))
# cluster results for each class session.
#imported new dataset with "conn"
mod2 <- Mclust(BioData_FrequenciesALL_withconn_)
summary(mod2)
mod2results <- mod2$classification
summary(mod2results)
#merging clusters with original data
LPA.results <- bind_cols(BioData_FrequenciesALL_withconn_, mod2results)
#renaming cluster column
LPA.results <- rename(LPA.results[-1], cluster=...19)
# data visualization for CDOP data by cluster.
LPA.results_long <- melt(LPA.results, 
                         id.vars = c("cluster"), 
                         variable.name = "Code", 
                         value.name = "Response")
LPA.results_long <- LPA.results_long %>% mutate(Percent = Response*100, Response=NULL)
cluster1 <- filter(LPA.results_long, cluster==1)
cluster2 <- filter(LPA.results_long, cluster==2)
cluster3 <- filter(LPA.results_long, cluster==3)
cluster4 <- filter(LPA.results_long, cluster==4)
cluster5 <- filter(LPA.results_long, cluster==5)
cluster6 <- filter(LPA.results_long, cluster==6)
cluster7 <- filter(LPA.results_long, cluster==7)
cluster8 <- filter(LPA.results_long, cluster==8)
cluster9 <- filter(LPA.results_long, cluster==9)
library(ggplot2)
# Percent time for cluster 1.
p1 <- ggplot(cluster1) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster I") +
  scale_colour_brewer(palette = "Spectral")
plot(p1)
# Percent time for cluster 2.
p2 <- ggplot(cluster2) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster II") +
  scale_colour_brewer(palette = "Spectral")
plot(p2)
# Percent time for cluster 3.
p3 <- ggplot(cluster3) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster III") +
  scale_colour_brewer(palette = "Spectral")
plot(p3)
# Percent time for cluster 4.
p4 <- ggplot(cluster4) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster IV") +
  scale_colour_brewer(palette = "Spectral")
plot(p4)
# Percent time for cluster 5.
p5 <- ggplot(cluster5) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster V") +
  scale_colour_brewer(palette = "Spectral")
plot(p5)
# Percent time for cluster 6.
p6 <- ggplot(cluster6) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster VI") +
  scale_colour_brewer(palette = "Spectral")
plot(p6)
# Percent time for cluster 7.
p7 <- ggplot(cluster7) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster VII") +
  scale_colour_brewer(palette = "Spectral")
plot(p7)
# Percent time for cluster 8.
p8 <- ggplot(cluster8) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster VIII") +
  scale_colour_brewer(palette = "Spectral")
plot(p8)
# Percent time for cluster 9.
p9 <- ggplot(cluster9) +
  geom_boxplot(aes(Code, Percent, fill = Code),
               position = position_dodge(0.80)) + scale_y_continuous(limits=c(0, 100)) +
  geom_jitter(aes(x=Code, y=Percent, fill = Code), trim = FALSE, binaxis='y', 
              position=position_jitter(0.2)) + theme_classic() + 
  labs(y="Percentage of 2-Minute Time Intervals", 
       x= "CDOP Individual Codes") + scale_fill_discrete(NULL) + 
  theme(axis.title.y = element_text(size=12, face="bold"), 
        axis.text.y=element_text(size=12), 
        axis.title.x = element_text(size=11, face="bold"),
        axis.text.x=element_text(size=10, angle=45, margin = margin(t=10)), 
        legend.text=element_blank(), legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(size=14, face="bold")) + 
  ggtitle("Cluster IX") +
  scale_colour_brewer(palette = "Spectral")
plot(p9)
require(gridExtra)
grid.arrange(p1,p2, p3,p4, p5, p6, p7, p8, p9)


