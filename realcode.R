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
fviz_mclust(mod1, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
fviz_mclust(mod1, "uncertainty", palette = "jco")

