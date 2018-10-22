#------------------------------
# QNBT
#------------------------------
rm(list=ls())
library(rjags)
library(spBayes)
library(coda)
library(rjags)
library(fields)
library(MBA)
library(geoR)
library(sp)
library(rgdal)
library(sf)
library(mice)
library(R2jags)
library(parallel)
library(dplyr)

#------------------------------#------------------------------
#  Load the data. 
#------------------------------#------------------------------
dir <- "F:\\MSvBRK2018\\CSV\\"
setwd(dir)
file <- "MSvBRK2018.csv"
QNBT <- read.csv(paste0(dir,file), sep=",")
head(QNBT)
USA <- readOGR(dsn=paste0(dir,"states.gdb"), layer="USA")
dim(QNBT)

#--------------------#
#-- Spatiotemporal --#
#--------------------#
coords <- as.matrix(QNBT[,c("PointX","PointY")])
head(coords)
#plot(USA, xlim=c(-110,-80),ylim=c(25,50))
#points(s, col='red')

nps <- as.matrix(QNBT[,c(4:111)])
head(nps)
nps[is.na(nps)] <- 0
pano <- as.matrix(QNBT[,c(112:219)])
pano[is.na(pano)] <- 0
head(pano)
flickr <- as.matrix(QNBT[,c(220:327)])
flickr[is.na(flickr)] <- 0
head(flickr)

dim(nps)
dim(pano)
dim(flickr)

# change this varaible accordingly
Y <- nps
# change negative value to 0




D <- as.matrix(dist(coords))
dim(D)
dim(QNBT)
X <- as.matrix(QNBT[,328:398])

X.df <- as.data.frame(cbind(1,X))
X <- as.matrix(X.df)



## Test and Train
smp_size <- floor(0.75 * nrow(Y))
smp_size

## set the seed to make your partition reproducible
set.seed(123)
train_ind  <- sample(seq_len(nrow(Y)), size = smp_size)


Y_train <- Y[train_ind,]
X_train <- X[train_ind,]
pano_train <- pano[train_ind,]
flickr_train <- flickr[train_ind,]

Y_test <- Y[-train_ind,]
X_test <- X[-train_ind,]
pano_test <- pano[-train_ind,]
flickr_test <- flickr[-train_ind,]


ntrain  <- nrow(Y_train)
ntest   <- nrow(Y_test)  
## 



t <- ncol(Y_train)
n <- nrow(X_train)
p <- ncol(X_train)
dim(X_train)


lm.m <- lm(Y_train ~ X_train - 1)
summary(lm.m)
cf <- as.data.frame(coefficients(lm.m))
cf.vec <- as.vector(rowMeans(cf))

spatiotemporal_model <- "model{ 

# Likelihood
for(i in 1:n){
  for(j in 1:t){
    y[i,j] ~ dnorm(mu[i,j] + w[i], tau.sq)
  }
}

for(i in 1:n){
  w.mu[i] <- 0
}

for(i in 1:n){
  for(j in 1:t){
    mu[i,j] <- b_0 + b_1%*%pano[i,j] + b_2%*%flickr[i,j] + X_b[i]
  }
}

X_b <- X%*%beta
b_0 ~ dnorm(0, 0.001)
b_1 ~ dnorm(0, 0.001)
b_2 ~ dnorm(0, 0.001)

for(i in 1:p){
  beta[i] ~ dnorm(0, 0.000001)
}

sigma.sq ~ dgamma(2, 0.001)
sigma.sq.w ~ dgamma(2, 0.001)
phi ~ dunif(0.003, 3)
tau.sq <- 1/sigma.sq

for(i in 1:n){
  for(j in 1:n){
    C.w[i,j] <- sigma.sq.w*exp(-phi*D[i,j])
  }
}

P.w <- inverse(C.w)

w ~ dmnorm(w.mu, P.w)

}
"

data <- list("y" = Y_train, 
             "t" = t, 
             "X" = X_train, 
             "n" = n,
             "p" = p,
             "D" = D, 
             "pano" = pano_train, 
             "flickr" = flickr_train)

inits <- list(beta = cf.vec, 
              sigma.sq.w = 4000,
              sigma.sq = 4000,
              phi = 0.0075)

model1 <- jags.model(textConnection(spatiotemporal_model),
                     data=data, 
                     n.chains=3,
                     inits=inits,
                     n.adapt = 10000)

summary(model1)

update(model1, 20000)



#------------------------------#------------------------------#
# sp-samps
#------------------------------#------------------------------#
params <-c("w","beta","b_0","b_1","b_2",
           "sigma.sq","sigma.sq.w","phi","tau.sq")

samps   <- coda.samples(model1, 
                        variable.names=params, 
                        n.iter=10000)

plot(samps, density=FALSE)
plot(samps[c(paste("beta","w", "sigma.sq", "sigma.sq.w", "phi", "tau.sq"))], density=FALSE)

gelman.diag(samps)
# 
# #------------------------------#------------------------------#
# # w
# #------------------------------#------------------------------#
burn.in <- 1000
sub.samps <- as.matrix(window(samps, start=burn.in))
# 
w.hat <- apply(sub.samps[,paste("w[",1:n,"]",sep="")], 2, mean)
# 
# 
visits <- Y 
par(mfrow=c(1,1))
usabox <- as.vector(c(-140,-55,20,55))
surf <- mba.surf(cbind(coords, visits), no.X=1000, no.Y=1000, extend=TRUE, b.box=usabox)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", xlab="X", ylab="Y", main="NPS Visitation (2006-2014)")
points(coords, col="grey")
plot(USA, add=TRUE, border="grey")
# 
# surf <- mba.surf(cbind(coords.mod, resids), no.X=1000, no.Y=1000, b.box=usabox, extend=TRUE)$xyz.est
# image.plot(surf, main="Non-spatial model residuals")
# points(coords, col="darkgrey")
# plot(USA, add=TRUE, border="black")
# 
# surf <- mba.surf(cbind(coords.mod, w.hat), no.X=1000, no.Y=1000, b.box=usabox, extend=TRUE)$xyz.est
# image.plot(surf, main="Spatial random effects")
# points(coords, col="black")
# plot(USA, add=TRUE, border="black")
# 
# #------------------------------#------------------------------#
# # obs-fitted
# #------------------------------#------------------------------#
# nps.fitted.sp <- apply(sub.samps[,paste("y[",1:n,"]",sep="")], 2, mean)
# # 
# # par(mfrow=c(1,2))
# # plot(nps.fitted, Y, main="Non-spatial model fitted values")
# # plot(nps.fitted.sp, Y, main="Spatial model fitted values")
# 
# 
# #------------------------------#------------------------------#
# # DIC
# #------------------------------#------------------------------#
# dic.samples(jags.m, n.iter=100, thin=10)
# dic.samples(jags.sp.m, n.iter=100, thin=10)
