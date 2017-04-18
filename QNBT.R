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
                  'arm',
                  'bpp',
                  'bbmle')) {
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

# PADUS - Pano
plot(PADUS_Pano$Count_, PADUS_Pano$Count_2008)
summary(PADUS_Pano$Count_2008)

# Yellowstone and Yosemite National Parks
PADUS_Pano[PADUS_Pano$Count_2008 > 1100, ]
PADUS_Pano.mat <- as.matrix(PADUS_Pano[,32:39])
rcorr(PADUS_Pano.mat)

##---------------##
## QNBT analysis ##
##---------------##
# data prep
PADUS_NRRS.dat <- as.data.frame(PADUS_NRRS.mat)
PADUS_Pano.dat <- as.data.frame(PADUS_Pano.mat)
drops <- c("Count_")
PADUS_NRRS.dat <- PADUS_NRRS.dat[ , !(names(PADUS_NRRS.dat) %in% drops)]
PADUS_Pano.dat <- PADUS_Pano.dat[ , !(names(PADUS_Pano.dat) %in% drops)]

head(PADUS_NRRS.dat)
head(PADUS_Pano.dat)

PADUS_NRRS.dat$NRRS_sum <- rowSums(PADUS_NRRS.dat, na.rm = TRUE)
PADUS_Pano.dat$Pano_sum <- rowSums(PADUS_Pano.dat, na.rm = TRUE)

PADUS_NRRS.dat$NRRS_mean <- rowMeans(PADUS_NRRS.dat, na.rm = TRUE)
PADUS_Pano.dat$Pano_mean <- rowMeans(PADUS_Pano.dat, na.rm = TRUE)

head(PADUS_NRRS.dat)
head(PADUS_Pano.dat)

# DF <- as.data.frame(cbind(PADUS_NRRS.mat,PADUS_Pano.mat))
# drops <- c("Count_")
# DF <- DF[ , !(names(DF) %in% drops)]
# names(DF) <- c("NRRS_2008","NRRS_2009","NRRS_2010","NRRS_2011","NRRS_2012","NRRS_2013","NRRS_2014",
#                "Pano_2008","Pano_2009","Pano_2010","Pano_2011","Pano_2012","Pano_2013","Pano_2014")
# head(DF)
# nrow(DF)

DF.sum <- as.data.frame(cbind(PADUS_NRRS.dat$NRRS_sum,PADUS_Pano.dat$Pano_sum))
names(DF.sum) <- c("y","x")
head(DF.sum)

DF.mean <- as.data.frame(cbind(PADUS_NRRS.dat$NRRS_mean,PADUS_Pano.dat$Pano_mean))
names(DF.mean) <- c("y","x")
head(DF.mean)

# remove zero visitation rows
DF.sum <- DF.sum[apply(DF.sum,1,function(z) !any(z==0)),] 

# remove zero visitation rows
DF.mean <- DF.mean[apply(DF.mean,1,function(z) !any(z==0)),] 

# view
plot(DF.mean[,"x"], DF.mean[,"y"], log="xy", xlab="pano", ylab="nrrs")

##----------------------------------------------------##
##-- Non linear curve fitting to untransformed data --##
##----------------------------------------------------##
# fit a power function to the non-transformed data
z <- nls(y ~ Y_0 * x^B, data=DF.mean, start=list(Y_0=500, B=0.6))
summary(z)            # parameter estimates and overall model fit
coef(z)               # model coefficients (means, slopes, intercepts)
# confint(z)          # confidence intervals for parameters

plot(y ~ x, data=DF.mean, xlab='pano', ylab='nrrs', log = "xy", main = "nonlinear least squares (nls) log-log plot")
lines(seq(0,10000,0.1), 
      predict(z, 
              newdata=data.frame(x = seq(0,10000,0.1))), col='red')

##------------------------------##
##-- generalized linear model --##
##------------------------------##
# data prep #
# one way to fit a power law is to log10 transformal both variables.  
# However, we want to use the natural log as this was the Nature paper's approach
e <- exp(1) 
plot(y ~ x, data=DF.mean, 
     xlab='pano', 
     ylab='nrrs', 
     log = "yx",
     main = "Linear fit to natural log transformed data (mean) - B=0.3221555 Yo=474.9223")

linear.fit <- lm(log(y) ~ log(x), data=DF.mean) # natural log
linear.fit$coefficients # these are you parameters

B <- as.numeric(linear.fit$coefficients[2])  # B
print(Y_0 <- as.numeric(e^linear.fit$coefficients[1]))  # Y_0
x <- DF.mean$x
print(paste("B:", B))
print(paste("Yo:", Y_0))
head(x)
ypred <- Y_0*x^B   # this is the predicted curve 
DF.results <- cbind(ypred, x)
head(DF.results)
lines(ypred ~ x, data=DF.results, xlab='pano', ylab='ypred - nrrs',  col='red')

## ------------- ##
## GLM model fit ##
## ------------- ##
# First, we’ll fit a model to our data with glm() 
# to make sure we can recover the parameters underlying our simulated data:
mu_start <- c(linear.fit$coefficients[1],linear.fit$coefficients[2])
m_glm <- glm(y ~ log(x), start=mu_start, family = Gamma(link = "log"), data=DF.mean)
m_glm_ci <- confint(m_glm)
display(m_glm)
coef(m_glm)

# residual plot
plot(x=log(DF.mean$x), y=resid(m_glm),  ylab="Residuals", xlab="Panoramio data", main="Panoramio residuals")
logLik(m_glm)
AIC(m_glm)
BIC(m_glm)
plot(m_glm)

# fitted
fit_glm <- fitted(m_glm)
df.pred <- as.data.frame(seq(0,341,1))
y_pred <- predict(model.glm, newdata=df.pred)
plot(df.pred[,1], y_pred)

# prediction
log.lin.sig <- summary(m_glm)$dispersion
log.lin.pred <- exp(predict(m_glm) + 0.5*log.lin.sig)
basicPlot <- function(...){
  plot(y ~ x, data=DF.mean, bty="n", lwd=2,
       main="Pano vs. NRRS", log="xy",
       col="#00526D", 
       xlab="Pano", 
       ylab="NRRS", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}
basicPlot()
lines(DF.mean$x, log.lin.pred, col="red", lwd=2)
legend(x="bottomright", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "log-transformed LM"),
       col=c("#00526D","red"), pch=c(1,NA))

# pearson statistic 
head(DF.mean)
rcorr(DF.mean[,1], DF.mean[,2], type=c("pearson"))

##---------------------##
# ---- sample code ---- #
##---------------------##
# zlm <- glm(y ~ x, data = DF.mean)
# plot(y ~ x, data = DF.mean, log="xy")
# #abline(zlm)
# lines(seq(0,1000,0.1), 
#       predict(zlm, 
#               newdata=data.frame(x = seq(0,1000,0.1))))
# 
# resid(z)               # residuals
# fitted(z)              # predicted values
# 
# predict(z, newdata=log.2009) # predicted values for new observations
# # anova(z1, z2)          # compare fits of 2 models, "full" vs "reduced"
# logLik(z)              # log-likelihood of the parameters
# AIC(z)                 # Akaike Information Criterion
# BIC(z)                  # Bayesian Information Criterion
