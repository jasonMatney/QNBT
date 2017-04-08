rm(list=ls())
setwd("C:\\Users\\jamatney\\Desktop\\QNBT")
list.files()

for (package in c('sp', 
                  'gstat', 
                  'dplyr', 
                  'ggplot2',
                  'Hmisc')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}

PADUS_NRRS <- read.csv("PADUS_NRRS.txt")
PADUS_Pano <- read.csv("PADUS_Pano.txt")
str(PADUS_NRRS)
str(PADUS_Pano)
PADUS_Pano[is.na(PADUS_Pano)] <- as.integer(0)

# PADUS - NRRS
plot(PADUS_NRRS$Count_, PADUS_NRRS$Freq_2008)
summary(PADUS_NRRS$Freq_2008)

# Grand Canyon and Yosemite National Parks
PADUS_NRRS[PADUS_NRRS$Freq_2008 > 40000,]
PADUS_NRRS.mat <- as.matrix(PADUS_NRRS[,32:39])
rcorr(PADUS_NRRS.mat)

# PADUS - Pano
plot(PADUS_Pano$Count_, PADUS_Pano$Count_2008)
summary(PADUS_Pano$Count_2008)

# Yellowstone and Yosemite National Parks
PADUS_Pano[PADUS_Pano$Count_2008 > 1100, ]
PADUS_Pano.mat <- as.matrix(PADUS_Pano[,32:39])
rcorr(PADUS_Pano.mat)
