rm(list=ls())
setwd("C:\\Users\\jamatney\\Desktop\\QNBT")
list.files()

for (package in c('sp', 
                  'gstat', 
                  'dplyr', 
                  'ggplot2',
                  'Hmisc',
                  'minpack.lm')) {
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
str(PADUS_NRRS.dat)
DF <- as.data.frame(cbind(PADUS_NRRS.mat,PADUS_Pano.mat))
drops <- c("Count_")
DF <- DF[ , !(names(DF) %in% drops)]
names(DF) <- c("NRRS_2008",
               "NRRS_2009",
               "NRRS_2010",
               "NRRS_2011",
               "NRRS_2012",
               "NRRS_2013",
               "NRRS_2014",
               "Pano_2008",
               "Pano_2009",
               "Pano_2010",
               "Pano_2011",
               "Pano_2012",
               "Pano_2013",
               "Pano_2014")
head(DF)
nrow(DF)

# remove 2008 zero visitation rows
DF <- DF[apply(DF["NRRS_2008"],1,function(z) !any(z==0)),] 

# data prep
y.2008 <- DF[,"NRRS_2008"]
x.2008 <- DF[,"Pano_2008"]
# x.2009 <- PADUS_Pano.mat[,3]

plot(log(y.2008), log(x.2008))
dat.2008 <- as.data.frame(cbind(y.2008, x.2008))
log.2008 <- log(dat.2008)
log.2008 <- do.call(data.frame,lapply(log.2008, function(x) replace(x, is.infinite(x),NA)))
names(log.2008) <- c("y","x")

log.2009 <- log(as.data.frame(x.2009))
log.2009 <- do.call(data.frame,lapply(log.2009, function(x) replace(x, is.infinite(x),NA)))
names(log.2009) <- c("x")

# lm
fit.2008 <- lm(y~x, data=log.2008)
summary(fit.2008)
gvlma(fit.2008)

# fit a power function to the log-transformed data
z <- nls(y ~ Y_0 * x^B, data = log.2008, start = list(Y_0=6, B=1))
summary(z)             # parameter estimates and overall model fit
coef(z)                # model coefficients (means, slopes, intercepts)
confint(z)             # confidence intervals for parameters
plot(y ~ x, data=log.2008)
lines(seq(0,7,0.1), 
      predict(z, 
              newdata=data.frame(x = seq(0,7,0.1))))
resid(z)               # residuals
fitted(z)              # predicted values

predict(z, newdata=log.2009) # predicted values for new observations
# anova(z1, z2)          # compare fits of 2 models, "full" vs "reduced"
logLik(z)              # log-likelihood of the parameters
AIC(z)                 # Akaike Information Criterion
BIC(z)                  # Bayesian Information Criterion

