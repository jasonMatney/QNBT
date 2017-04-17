rm(list=ls())
setwd("/Users/jason/Desktop/QNBT")
options(scipen=10000)
list.files()

for (package in c('sp', 
                  'gstat', 
                  'dplyr', 
                  'ggplot2',
                  'Hmisc',
                  'gvlma',
                  'minpack.lm',
                  'heR.Misc')) {
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

# GLM
counts.N <- PADUS_NRRS.mat[,1]
outcome.N <- PADUS_NRRS.mat[,2:8]
glm.N <- glm(counts.N ~ outcome.N, family = gaussian())
anova(glm.N)
summary(glm.N)
# plot(glm.N)

counts.P <- PADUS_Pano.mat[,1]
outcome.P <- PADUS_Pano.mat[,2:8]
glm.P <- glm(counts.P ~ outcome.P, family = gaussian())
anova(glm.P)
summary(glm.P)
# plot(glm.P)

##----------##
## analysis ##
##----------##
PADUS_NRRS.dat <- as.data.frame(PADUS_NRRS.mat)
PADUS_Pano.dat <- as.data.frame(PADUS_Pano.mat)
drops <- c("Count_")
PADUS_NRRS.dat <- PADUS_NRRS.dat[ , !(names(PADUS_NRRS.dat) %in% drops)]
PADUS_Pano.dat <- PADUS_Pano.dat[ , !(names(PADUS_Pano.dat) %in% drops)]

head(PADUS_NRRS.dat)
head(PADUS_Pano.dat)

PADUS_NRRS.dat$NRRS_sum <- rowSums(PADUS_NRRS.dat, na.rm = TRUE)
PADUS_Pano.dat$Pano_sum <- rowSums(PADUS_Pano.dat, na.rm = TRUE)

head(PADUS_NRRS.dat)
head(PADUS_Pano.dat)

# DF <- as.data.frame(cbind(PADUS_NRRS.mat,PADUS_Pano.mat))
# drops <- c("Count_")
# DF <- DF[ , !(names(DF) %in% drops)]
# names(DF) <- c("NRRS_2008","NRRS_2009","NRRS_2010","NRRS_2011","NRRS_2012","NRRS_2013","NRRS_2014",
#                "Pano_2008","Pano_2009","Pano_2010","Pano_2011","Pano_2012","Pano_2013","Pano_2014")
# head(DF)
# nrow(DF)

DF <- as.data.frame(cbind(PADUS_NRRS.dat$NRRS_sum,PADUS_Pano.dat$Pano_sum))
names(DF) <- c("NRRS","Pano")
head(DF)

# remove 2008 zero visitation rows
DF <- DF[apply(DF["NRRS"],1,function(z) !any(z==0)),] 
DF <- DF[apply(DF["Pano"],1,function(z) !any(z==0)),] 

head(DF)
nrow(DF)
DF.mat <- as.matrix(DF)
nrow(DF.mat)

# data prep
# pano.2008 <- DF[,"Pano_2008"]
# nrrs.2008 <- DF[,"NRRS_2008"]
# x.2009 <- PADUS_Pano.mat[,3]
head(DF)
pano <- DF[,"Pano"]
nrrs <- DF[,"NRRS"]

plot(pano, nrrs, xlab="pano", ylab="nrrs")

DF.dat <- DF
names(DF.dat) <- c("y","x")
head(DF.dat)

# fit a power function to the log-transformed data
z <- nls(y ~ Y_0 * x^B, data=DF.dat, start=list(Y_0=500, B=0.6))

summary(z)             # parameter estimates and overall model fit
coef(z)                # model coefficients (means, slopes, intercepts)
# confint(z)             # confidence intervals for parameters

plot(y ~ x, data=DF.dat,xlab='pano', ylab='nrrs', log = "xy", main = "Log-log Plot")
box()
# ticks <- seq(0, 10, by=1)
# labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
# axis(1, at=c(0.01, 0.1, 1, 10, 100), labels=labels)

lines(seq(0,10000,0.1), 
      predict(z, 
              newdata=data.frame(x = seq(0,10000,0.1))), col='red')


### lm ###
zlm <- lm(y ~ x, data = dat.2008)
plot(lm(y ~ x, data = dat.2008))
#abline(zlm)
lines(seq(0,7,0.1), 
      predict(zlm, 
              newdata=data.frame(x = seq(0,7,0.1))))

resid(z)               # residuals
fitted(z)              # predicted values

predict(z, newdata=log.2009) # predicted values for new observations
# anova(z1, z2)          # compare fits of 2 models, "full" vs "reduced"
logLik(z)              # log-likelihood of the parameters
AIC(z)                 # Akaike Information Criterion
BIC(z)                  # Bayesian Information Criterion
