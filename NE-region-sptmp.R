#------------------------------#
# QNBT
#------------------------------#
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
library(olsrr)
library(maps)
library(mapdata)
library(ggplot2)
library(glmnet)
library(raster)
library(RColorBrewer)
library(colorRamps)
library(colorspace)
library(rasterVis)
library(gridExtra)
library(grid)


##------------------------------#------------------------------##
##  Load the data. 
##------------------------------#------------------------------##
dir <- "F:\\MSvBRK2018\\CSV\\"
setwd(dir)
file <- "QNBT_NE.csv"
QNBT <- read.csv(paste0(dir,file), sep=",")
head(QNBT)
USA <- readOGR(dsn=paste0(dir,"states.gdb"), layer="USA")
usabox <- as.vector(c(-140,-55,20,55))
# QNBT <- QNBT[QNBT$Unit_Code %in% c('ACAD','ARCH','BADL','BIBE','BISC','BLCA','BRCA','CANY','CARE','CAVE','CHIS','CONG','CRLA','CUVA','DEVA','EVER','GLBA','GLAC',
#                                    'GRCA','GRTE','GRBA','GRSA','GRSM','GUMO','HOSP','ISRO','JOTR','KICA','LACL','LAVO','MACA','MEVE','MORA','NOCA','OLYM','PEFO','PINN',
#                                    'REDW','ROMO','SAGU','SEQU','SHEN','THIS','VOYA','WICA','WRST','YELL','YOSE','ZION'),]
# QNBT <- sample_n(QNBT,100)
# QNBT[,"Unit_Code"]

fgdb <- "F:/MSvBRK2018/NE_Region.gdb"
PAD.lyr <- "PADUS1_4NoDesignation_NE_Region"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
PADUS_NE_region <- readOGR(dsn=fgdb, layer="PADUS_NE_Region_proj")


## Remove PARK S WITH 0 VISITAITON ##
dim(QNBT)
QNBT_NPS <- QNBT[,4:111]
head(QNBT_NPS)
## Remove rows with 0 visitation
row_sub = apply(QNBT_NPS, 1, function(row) all(row !=0 ))
## REmove rows with 0 visitation
QNBT <- QNBT[row_sub,]



#### SELECT TOP 100 PARKS with MOST Visitation in July 2010 ####
# QNBT <- QNBT[order(-QNBT$NPS_2010_07),] 
# QNBT <- QNBT[1:100,]
# #### ------------------------------------------------- ####

##--------------------------##
##------ Plot America ------##
##--------------------------##

usa <- map_data("usa")
# Variable for park centroids 
coords <- as.matrix(QNBT[,c("PointX","PointY")])
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_point(aes(PointX, PointY),data=as.data.frame(coords), col='red')
dim(QNBT)
# ggplot() + geom_polygon(data = PADUS_NE_region, 
#                         aes(x=long, 
#                             y = lat, 
#                             group = group)) + 
#   coord_fixed(1.3) + 
#   geom_point(aes(PointX, PointY)
#              ,data=as.data.frame(coords), col='red') +
#   layer(sp.polygons(ne.states, lwd=1, col="darkgrey"))
# 
#levelplot(rr, xlim=c(-85,-65), ylim=c(35,50), margin=FALSE, maxpixels = 10000000, par.settings=BuRdTheme(),main="SUMMER NPS Visitation Posterior Estimates - NE Region") + 
  # layer(sp.polygons(ne.states, lwd=1, col="darkgrey"))

##-------------------##
##-- Set Variables --##
##-------------------##
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

Y <- nps
D <- as.matrix(dist(coords))
dim(D)
dim(QNBT)
# have to remove some variables bc there's more staic vars than points

## CHECK THIS!
X <- as.matrix(QNBT[,328:ncol(QNBT)])
head(X)

##--------------------------------------------##
##---- Linear Regression (Initial Values) ----##
##--------------------------------------------##
data.df <- as.data.frame(cbind(rowMeans(Y),
                               rowMeans(pano),
                               rowMeans(flickr),
                               X))
colnames(data.df)[1] <- "NPS_Visitation"
colnames(data.df)[2] <- "Panoramio"
colnames(data.df)[3] <- "Flickr"
dim(data.df)


##--------------------##
## Variable Selection ##
##--------------------##

#----------#
#- SUMMER -#
#----------#
Y.summer <- Y[,cbind(c(6:8),
                     c(18:20),
                     c(30:32),
                     c(42:44),
                     c(54:56),
                     c(66:68),
                     c(78:80),
                     c(90:92),
                     c(102:104))]

pano.summer <- pano[,cbind(c(6:8),
                     c(18:20),
                     c(30:32),
                     c(42:44),
                     c(54:56),
                     c(66:68),
                     c(78:80),
                     c(90:92),
                     c(102:104))]

flickr.summer <- flickr[,cbind(c(6:8),
                               c(18:20),
                               c(30:32),
                               c(42:44),
                               c(54:56),
                               c(66:68),
                               c(78:80),
                               c(90:92),
                               c(102:104))]
data.summer <- as.data.frame(cbind(rowMeans(Y.summer),
                                   rowMeans(pano.summer),
                                   rowMeans(flickr.summer),
                                   X))
colnames(data.summer)[1] <- "NPS_Visitation"
colnames(data.summer)[2] <- "Panoramio"
colnames(data.summer)[3] <- "Flickr"
str(data.summer)


## TOO MANY VARIABLES (MORE THAN OBSERVATIONS)
col.names.remove <- c("ZC_qnum25", "ZC_qnum75",
                      "ZC_qldt25", "ZC_qldt75",
                      "ZC_qdur25","ZC_qdur75",
                      "ZC_qdis25","ZC_qdis75",
                      "NPSclosest_numP25","NPSclosest_numP75",
                      "NPSclosest_leadT25","NPSclosest_leadT75",
                      "NPSclosest_dur25","NPSclosest_dur75",
                      "NPSclosest_dis25","NPSclosest_dis75",
                      "Site50miles_numP25","Site50miles_numP75",
                      "Site50miles_leadT25","Site50miles_leadT75",
                      "Site50miles_dur25","Site50miles_dur75",
                      "Site50miles_dis25","Site50miles_dis75",
                      "Near_dist_class8_miles","Near_dist_class9_miles",
                      "Near_dist_class10_miles")
# 
data.summer.mod <- data.summer[,!(colnames(data.summer) %in% col.names.remove)]
str(data.summer.mod)
#----------------#
#- SUMMER MEANS -#
#----------------#
model.summer <- lm(NPS_Visitation ~ ., data=data.summer.mod)

summary(model.summer)
k <- ols_step_both_aic(model.summer)
plot(k)
pred_aic <- k$predictors
pred_aic
X.mod.summer <- X[,which(colnames(X) %in% pred_aic)]
data.step.summer <-as.data.frame(cbind(rowMeans(Y.summer),
                                      rowMeans(pano.summer),
                                       rowMeans(flickr.summer),
                                       X.mod.summer))
colnames(data.step.summer)[1] <- "NPS_Visitation"
colnames(data.step.summer)[2] <- "Panoramio"
colnames(data.step.summer)[3] <- "Flickr"
head(data.step.summer)

lm.mod.summer <- lm(NPS_Visitation ~., data=data.step.summer) 
# get only the BETA coefficients!
summary(lm.mod.summer)

### CHECK THIS ####
cf <- coefficients(lm.mod.summer)[4:length(coefficients(lm.mod.summer))]
cf

head(X.mod.summer)
t <- ncol(Y.summer)
n <- nrow(X.mod.summer)
p <- ncol(X.mod.summer)

##-----------------------------------##
##---- Spatiotemporal JAGS Model ----##
##-----------------------------------##
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
b_0 ~ dnorm(2, 0.001)
b_1 ~ dnorm(2, 0.001)
b_2 ~ dnorm(2, 0.001)

for(i in 1:p){
  beta[i] ~ dnorm(2, 0.000001)
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

data <- list("y" = Y.summer, 
             "t" = t, 
             "X" = X.mod.summer, 
             "n" = n,
             "p" = p,
             "D" = D, 
             "pano" = pano.summer, 
             "flickr" = flickr.summer)

inits <- list(beta = cf, 
              sigma.sq.w = 1000,
              sigma.sq = 1000,
              phi = 0.0075)

start.time <- Sys.time()
fit <- jags.model(textConnection(spatiotemporal_model),
                  data=data, 
                  n.chains=3,
                  inits=inits,
                  n.adapt = 100000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
update(fit, 100000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

summary(fit)

#------------------------------#------------------------------#
# sp-sampsn
#------------------------------#------------------------------#
params <-c("y","beta","b_0","b_1","b_2",
           "sigma.sq","sigma.sq.w","phi","tau.sq","w")

samples   <- coda.samples(fit, 
                        variable.names=params, 
                        n.iter=5000)

#save(samples, file = "NE_Region_summer_205k_samples.RData")
load("NE_Region_summer_205k_samples.RData")

dic.samples(fit, n.iter=20000, thin=100)

# converged: 
gelman.diag(samples[,c("b_0", "b_1", "b_2", paste("beta[",1:p,"]",sep=""))])
gelman.plot(samples[,c("b_0", "b_1", "b_2", paste("beta[",1:p,"]",sep=""))])

#load("205K_samples_TOP100_sites.RData")
plot(samples[, c(paste("beta[", 1:p, "]", sep = ""),"sigma.sq", "sigma.sq.w", "phi")], density = FALSE)
# 
head(samples[, c(paste("y[",1:n,",", 1:p, "]", sep = ""))])

#plot(USA)

require(bayesplot)
require(coda)

#plot(samples[, "mu[1,2]"], density = FALSE)
color_scheme_set("mix-blue-red")
mcmc_trace(samples, pars = c("b_0","b_1","b_2", "sigma.sq","sigma.sq.w","phi"))
mcmc_trace(samples, pars =  c(paste("beta[", 1:p, "]", sep = "")))


################################
# CHECK THIS 1471###############
mu.samples <- samples[,grep("y", colnames(samples[[1]]), value = TRUE)]
################################
################################

head(mu.samples)
mu.sub.samps <- as.matrix(window(mu.samples, start=2500))


####
# GET SUMMER MONTHS ##
mu.sub.samples <- colMeans(mu.sub.samps)
### CHECK THIS #############
a <- matrix(mu.sub.samples,52,27)

mu_data <- rowMeans(a)

## ------------ NE Region -------------- ##
states    <- c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Connecticut', 'New York', 'New Jersey', 'Delaware', 'Rhode Island', 'Maryland', 'Pennsylvania','West Virginia','Virginia')

us <- getData("GADM",country="USA",level=1)
us_border <- getData("GADM", country="USA", level=0)
ne.states <- us[us$NAME_1 %in% states,]
ne.bbox <- bbox(ne.states)
xlim <- c(min(ne.box[1,1],ne.box[1,1]),max(ne.box[1,2],ne.box[1,2]))
ylim <- c(min(ne.box[2,1],ne.box[2,1]),max(ne.box[2,2],ne.box[2,2]))
#plot(us_border)
plot(ne.states, xlim=xlim, ylim=ylim)
points(coords, col='red')
dev.off()
library(rgeos)
library(maptools)
NE_border <- gIntersection(ne.states,us_border) #clip polygon 2 with polygon 1
plot(NE_border, col = "lightblue")

#################
mu_data
dev.off()
par(mfrow=c(1,1))

surf <- mba.surf(cbind(coords, mu_data), no.X=1000, no.Y=1000, extend=TRUE, b.box=ne.bbox)$xyz.est

r <- raster(surf)
rr <- mask(r,NE_border)
cls <- cellFromPolygon(rr, NE_border, weights = TRUE)
rr[][-cls[[1]]] <- NA
plot(trim(rr))

# ---- Surf plot ---- #
require(scales)
#colr <- colorRampPalette(brewer.pal(11, 'BuYlRd'))
pal <- choose_palette()
levelplot(trim(rr), 
          # xlim=c(-85,-65), 
          # ylim=c(35,50),
          colorkey=list(
            space='right',                   # plot legend at bottom
            labels=list(font=4)     # legend ticks and labels 
          ),
          margin=FALSE, 
          maxpixels = 10000000, 
          par.settings=BuRdTheme(),
          col.regions=pal,
          scales=list(draw=TRUE),   
          main="SUMMER NPS Visitation Posterior Estimates - NE Region sites") + 
   layer(sp.polygons(ne.states, lwd=1, col="darkgrey"))

dev.off()

# ------------------- #

w.sub.samps <- as.matrix(window(samples[, paste("w[", 1:n, "]", sep = "")], start=2500))
w.sub.samps
w.hat <- colMeans(w.sub.samps)
ne.bbox=c(bbox(ne.states)[1], bbox(ne.states)[2], bbox(ne.states)[3], bbox(ne.states)[4])

w.surf <- mba.surf(cbind(coords, w.hat), no.X=1000, no.Y=1000, extend=TRUE, b.box=ne.bbox)$xyz.est
image.plot(w.surf, main="fitted")
r.w <- raster(w.surf)
rr.w <-  mask(r.w,NE_border)

cls.w <- cellFromPolygon(rr.w, NE_border, weights = TRUE)
rr.w[][-cls.w[[1]]] <- NA
pal.w <- choose_palette()
levelplot(trim(rr.w), 
          # xlim=c(-85,-65), 
          # ylim=c(35,50),
          colorkey=list(
            space='right',                   # plot legend at bottom
            labels=list(font=4)     # legend ticks and labels 
          ),
          margin=FALSE, 
          maxpixels = 10000000, 
          par.settings=BuRdTheme(),
          col.regions=pal.w,
          scales=list(draw=TRUE),   
          main="Spatial Random Effects - NE Region") + 
  layer(sp.polygons(ne.states, lwd=1, col="darkgrey"))

####


# converged: 
gelman.diag(samples[,c("b_0", "b_1", "b_2", paste("beta[",1:p,"]",sep=""))])
gelman.plot(samples[,c("b_0", "b_1", "b_2", paste("beta[",1:p,"]",sep=""))])


#### Observations-fitted
dic.samples(fit, n.iter=10000, thin=100)


##-----------------------------------------------##
# fitted
#------------------------------------------------##
y.fitted <- function(omega, X){
  n <- nrow(X)
  p <- ncol(X)

  beta <- omega[1:p]
  sigma.sq <- 4000

  rnorm(n, X%*%beta, sqrt(sigma.sq))
}
#


###
pred.samps <- samples[,2:11]
head(pred.samps)
predictors.posterior <- as.matrix(colMeans(as.matrix(pred.samps)))

length(predictors.posterior)
dim(predictors.model)

y.fitted(predictors.posterior,predictors.model)

#
# ### predicted values ####
round(summary(window(pred.samps, start = burn.in))$quantiles[,c(3,1,5)],2)
######
visitation.fitted <- apply(predictors.model, 1, y.fitted, predictors.posterior)
visitation.fitted
visitation.fitted <- apply(visitation.fitted, 1, mean)
visitation.fitted
resids <- rowMeans(Y) - visitation.fitted
rowMeans(Y)
summary(resids)

#
par(mfrow=c(1,1))
resid.surf <- mba.surf(cbind(coords, resids), no.X=1000, no.Y=1000, extend=TRUE, b.box=ne.bbox)$xyz.est

r.resid <- raster(resid.surf)
cls.resid <- cellFromPolygon(r.resid, NE_border, weights = TRUE)
r.resid[][-cls.resid[[1]]] <- NA

pal.resid <- choose_palette()
levelplot(trim(r.resid), 
          # xlim=c(-85,-65), 
          # ylim=c(35,50),
          colorkey=list(
            space='right',                   # plot legend at bottom
            labels=list(font=4)     # legend ticks and labels 
          ),
          margin=FALSE, 
          maxpixels = 10000000, 
          par.settings=BuRdTheme(),
          col.regions=pal.resid,
          scales=list(draw=TRUE),   
          main="NPS Visitation Residuals - NE Region sites") + 
  layer(sp.polygons(ne.states, lwd=1, col="darkgrey"))


### PLOT OVER PADUS ###

surf.pad <- mba.surf(cbind(coords, mu_data), no.X=2000, no.Y=2000, extend=TRUE, b.box=ne.bbox)$xyz.est

r.pad <- raster(surf.pad)
rr.pad <- mask(r.pad,PADUS_NE_region)
cls <- cellFromPolygon(rr.pad, PADUS_NE_region, weights = TRUE)
rr[][-cls[[1]]] <- NA
plot(trim(rr.pad))

# ---- Surf plot ---- #
require(scales)
#colr <- colorRampPalette(brewer.pal(11, 'BuYlRd'))
pal <- choose_palette()
levelplot(trim(rr.pad), 
          # xlim=c(-85,-65), 
          # ylim=c(35,50),
          colorkey=list(
            space='right',                   # plot legend at bottom
            labels=list(font=4)     # legend ticks and labels 
          ),
          margin=FALSE, 
          maxpixels = 10000000, 
          par.settings=BuRdTheme(),
          col.regions=pal,
          scales=list(draw=TRUE),   
          main="SUMMER NPS Visitation Posterior Estimates - PADUS polygons") + 
  layer(sp.polygons(ne.states, lwd=1, col="darkgrey"))

dev.off()


#---- variogram -----#

max.dist <- 0.5*max(dist(coords))
bins <- 10
v <- variog(coords=coords, data=resids, uvec=(seq(0, max.dist, length=bins)))

fit.variogram <- variofit(v)
fit.variogram
plot(v, main="Variogram of residuals")
lines(fit.variogram)
abline(h=fit.variogram$nugget, col="blue")##nugget
abline(h=fit.variogram$cov.pars[1]+fit.variogram$nugget, col="green")##sill
abline(v=log(0.05)*fit.variogram$cov.pars[2],
       col="red3")##effective range

fit.variogram

###################################################
### code chunk number 10: fitted
###################################################
resid.samples <- samples[,c("b_0", "b_1", "b_2", paste("beta[",1:p,"]",sep=""), "sigma.sq")]
X.pred.summer <- as.matrix(cbind(1,data.step.summer[,2:ncol(data.step.summer)]))


y.fitted <- function(omega, analysis){
  n <- nrow(analysis)
  p <- ncol(analysis)
  
  beta <- omega[1:p]
  sigma.sq <- omega[p+1]
  rnorm(n, analysis%*%beta, sqrt(sigma.sq))
}


bio.fitted <- apply(as.matrix(window(resid.samples, start = burn.in)), 1, y.fitted, analysis = X.pred.summer)
bio.fitted <- apply(bio.fitted, 1, mean)
