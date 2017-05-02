rm(list=ls())
for (package in c('sp', 
                  'raster', 
                  'cluster',
                  'Hmisc',
                  'car',
                  'Epi',
                  'stats',
                  'verification',
                  'ROCR',
                  'ggplot2',
                  'MuMIn',
                  'lme4',
                  'piecewiseSEM',
                  'sandwich',
                  'rgdal')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}

dir <- "F:/Parks_paper" 
setwd(dir)
list.files()

##--------------##
#- Load Rasters -#
##--------------##
# ------------------- #
# Dependent Variables #
# ------------------- #

## These are rasters of the raw PPL point locations - 

# ... Nah ....

## These are summed PPL frequencies joined to PAD polys that contain NRRS points 
PAD_PPL_1 <- raster("raster/PAD_PPL_1")
PAD_PPL_2 <- raster("raster/PAD_PPL_2")
PAD_PPL_3 <- raster("raster/PAD_PPL_3")
PAD_PPL_pooled <- raster("raster/PADPPL_pooled")

nrow(as.data.frame(getValues(PAD_PPL_2)))

## These are the PADUS polygons which contain NRRS facilities - 
PAD_Pano <- raster("raster/PAD_PANO")

###load protected areas ??
# Mparks_10km <- raster("PADUS1_4Arc10gdb/mangpark10kmZ")
# Mparks_5km <- raster("PADUS1_4Arc10gdb/mangpark5kmZ")
# Mparks_1km <- raster("PADUS1_4Arc10gdb/mangpark1kmZ")

# --------------------- #
# Independent Variables #
# --------------------- #
###transportation/distance
cst_distLG10km <- raster("raster/Cstdist_largN.tif")
cst_distSM10km <- raster("raster/sml_cstN.tif")
road_dist10km <- raster("raster/dist_rdsN.tif")

###population
pop2010 <- raster("raster/Pop_2010Ndiv.tif")

###terrain
slope10km <- raster("raster/us_elev_riseN.tif")
elev10km <- raster("raster/US_elevN.tif")

##amenities
EUparks <- raster("raster/EUCProtectN.tif")
distCoast <- raster("raster/Coast_EdistN.tif")
distwater <- raster("raster/Eu_WaterN.tif")

###land use
Urban10km <- raster("raster/Urb_2011N.tif")
pctUrban10km <- raster("raster/Urb10kmUSN.tif")
frst10km <- raster("raster/frt_US10kmN.tif")
ag10km <- raster("raster/Ag_US10kmN.tif")

###sociodemographic 
pctSeasonHomes10km <- raster("raster/sec00s1v1_seas_hu_pct.tif")
pctPvrty10km <- raster("raster/sec00_pop_below_pov_pct.tif")
pctRetired10km <- raster("raster/sec00s1v1_pop_ge65_pct.tif")
pctBachdegree <- raster("raster/sec00_bachplus_ofge25_pct.tif")

###possible regional levels
state <- raster("raster/StatesN.tif")
region <- raster("raster/DivisionsN.tif")
division <- raster("raster/RegionsN.tif")

dsn_basic <- "F:\\Parks_Paper\\PADUS1_4Arc10gdb\\"
setwd(dsn_basic)
list.files()
ogrListLayers(dsn=getwd())
states_basic <- readOGR(dsn=getwd(), layer="tl_2014_us_state_AlbersUSGS") 
###PPL
# PPL <- read.csv(paste0(dir,"/PPL/PPL_reservationdata.csv"))
# PPL_1 <- read.csv(paste0(dir,"/PPL/PPL_1.csv"))
# PPL_2 <- read.csv(paste0(dir,"/PPL/PPL_2.csv"))
# PPL_3 <- read.csv(paste0(dir,"/PPL/PPL_3.csv"))

##---------##
#-  Stack  -#
##---------##
##10km
x_res = 10000
y_res = 10000
jet.colors <- colorRampPalette(c("grey", "lightyellow"," yellow", "orange", "red", "brown"))
plot(PAD_PPL_1, axes=T, box=T, legend=T, maxpixels= x_res * y_res, col=jet.colors(16), main="PPL frequencies within PADUS Polygons, 0 to 50 miles (Bin 1 travel distance), n=268392")
plot(states_basic, add=TRUE)
plot(PAD_PPL_2, axes=T, box=T, legend=T, maxpixels= x_res * y_res, col=jet.colors(16), main="PPL frequencies within PADUS Polygons, 50 to 600 miles (Bin 2 travel distance), n=3366845")
plot(states_basic, add=TRUE)
plot(PAD_PPL_3, axes=T, box=T, legend=T, maxpixels= x_res * y_res, col=jet.colors(16), main="PPL frequencies within PADUS Polygons, greater than 600 miles (Bin 3 travel distance), n=527731")
plot(states_basic, add=TRUE)
plot(PAD_PPL_pooled, axes=T, box=T, legend=T, maxpixels= x_res * y_res, col=jet.colors(16), main="PPL frequencies within PADUS Polygons, Pooled travel distance, n=12473816")
plot(states_basic, add=TRUE)
plot(PAD_Pano, axes=T, box=T, legend=T, maxpixels= x_res * y_res, col=jet.colors(16), main="Panoramio frequencies within PAD US Polygons")
plot(states_basic, add=TRUE)
dev.off()

PAD_PPL_1[PAD_PPL_1 < 0.1] <- NA
PAD_PPL_2[PAD_PPL_2 < 0.1] <- NA 
PAD_PPL_3[PAD_PPL_3 < 0.1] <- NA
PAD_PPL_Pool[PAD_PPL_Pool < 0.1] <- NA

# Resolution - 10 km
stk_full <- stack(PAD_PPL_1, PAD_PPL_2, PAD_PPL_3, PAD_PPL_pooled, PAD_Pano, cst_distLG10km, cst_distSM10km, road_dist10km, pop2010, slope10km, elev10km, EUparks, distCoast, distwater, Urban10km, pctUrban10km, frst10km, ag10km, pctSeasonHomes10km, pctPvrty10km, pctRetired10km, pctBachdegree, state, region, division)
stk_sub <- stack(PAD_PPL_1, PAD_PPL_2, PAD_PPL_3, PAD_Pano)
plot(stk_sub, axes=T, box=T, legend=T, maxpixels= x_res * y_res, col=jet.colors(12))

##simply correlation calculation
jnk_rast <- layerStats(stk_sub, 'pearson', na.rm=T)
corr_matrix_10km <- jnk_rast$'pearson correlation coefficient'
corr_matrix_10km

## Extract all values into a matrix
valuetable_full <- getValues(stk_full)
valuetable_full <- as.data.frame(valuetable_full)
valuetable_full <- na.omit(valuetable_full)
summary(valuetable_full)

valuetable_sub <- getValues(stk_sub)
valuetable_sub <- na.omit(valuetable_sub)
summary(valuetable_sub)

# wtf
# datatable <- as.data.frame(stk)
# datatable <- na.omit(datatable)
# summary(datatable)

###change into a dataframe
stk_10km <- as.data.frame(valuetable_full)

summary(stk_10km)

###need to omit pano zeros to use a log transformation
# stk.df <-subset(stk.df,  PAD_Pano != 0)

###plot the log transformed correlations
###intialization
# rbPal <- colorRampPalette(c('red','blue'))
#This adds a column of color values
# based on the y values

#--------------------#
#--- Linear model ---#
#--------------------#
# Bin 1
fit_10km_1 <- lm(log(PAD_PPL_1) ~  log(pad_pano), data=stk_10km)
fit_10km_1$coefficients # these are your parameters
summary(fit_10km_1)

# Bin 2
fit_10km_2 <- lm(log(PAD_PPL_2) ~  log(pad_pano), data=stk_10km)
fit_10km_2$coefficients # these are your parameters
summary(fit_10km_2)

# Bin 3
fit_10km_3 <- lm(log(PAD_PPL_3) ~  log(pad_pano), data=stk_10km)
fit_10km_3$coefficients # these are your parameters
summary(fit_10km_3)

# Pooled
fit_10km_pooled <- lm(log(PADPPL_pooled) ~  log(pad_pano), data=stk_10km)
fit_10km_pooled$coefficients # these are your parameters
summary(fit_10km_pooled)

# Plots
par(mfrow=c(2,2)) 
plot(log(stk_10km$pad_pano), log(stk_10km$PAD_PPL_1), xlab="Log-transformed Panoramio data", ylab="Log-transformed PPL reservation data", main="Pano / PPL lm plot, bin 1 (0-50 miles)") 
abline(fit_10km_1, col='red')
plot(log(stk_10km$pad_pano), log(stk_10km$PAD_PPL_2), xlab="Log-transformed Panoramio data", ylab="Log-transformed PPL reservation data", main="Pano / PPL lm plot, bin 2 (50-600 miles)") 
abline(fit_10km_2, col='red')
plot(log(stk_10km$pad_pano), log(stk_10km$PAD_PPL_3), xlab="Log-transformed Panoramio data", ylab="Log-transformed PPL reservation data", main="Pano / PPL lm plot, bin 3 (> 600 miles)") 
abline(fit_10km_3, col='red')
plot(log(stk_10km$pad_pano), log(stk_10km$PADPPL_pooled), xlab="Log-transformed Panoramio data", ylab="Log-transformed PPL reservation data", main="Pano / PPL lm plot, pooled") 
abline(fit_10km_pooled, col='red')
dev.off()

# PCA
fit <- princomp(stk_10km, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components

biplot(fit)
par(mfrow=c(1,2))
biplot(fit, choices = 1:2)
biplot(fit, choices = 3:4)
dev.off()

##-------##
#-- GLM --#
##-------##
# Bin 1
glm_fit_10km_1 <- glm(log(PAD_PPL_1) ~  log(pad_pano) + StatesN + DivisionsN + RegionsN, data=stk_10km, family=poisson())
glm_fit_10km_1$coefficients # these are your parameters
summary(glm_fit_10km_1)


# data prep #
# one way to fit a power law is to log10 transformal both variables.  
# However, we want to use the natural log as this was the Nature paper's approach
# e <- exp(1) 
# plot(y ~ x, data=DF.mean, 
#      xlab='pano', 
#      ylab='nrrs', 
#      log = "yx",
#      main = "Linear fit to natural log plot (mean) - B=0.3221555 Yo=474.9223")
# 
# linear.fit <- lm(log(y) ~ log(x), data=DF.mean) # natural log
# linear.fit$coefficients # these are you parameters
# 
# B <- as.numeric(linear.fit$coefficients[2])  # B
# Y_0 <- as.numeric(e^linear.fit$coefficients[1])  # Y_0
# x <- DF.mean$x
# print(paste("B:", B))
# print(paste("Yo:", Y_0))
# head(x)
# ypred <- Y_0*x^B   # this is the predicted curve 
# DF.results <- cbind(ypred, x)
# head(DF.results)
# lines(ypred ~ x, data=DF.results, xlab='pano', ylab='ypred - nrrs',  col='red')


####getting rid of outliers
##stk_2008_10km <- subset(stk_2008_10km, Park_2008_10km <15000)
##stk_2008_10km <- subset(stk_2008_10km, Pano2008_10km <250)

##Plot correlation and other dependencies
#This adds a column of color values
rbPal <- colorRampPalette(c('red','blue'))
# based on the y values
###cost distance
stk_10km$Col <- rbPal(10)[as.numeric(cut(stk_10km$Cstdist_largN,breaks = 10))]
stk_10km$Col <- rbPal(10)[as.numeric(cut(stk_10km$Ag_US10kmN,breaks = 10))]
stk_10km$Col <- rbPal(10)[as.numeric(cut(stk_10km$sec00_bachplus_ofge25_pct,breaks = 10))]


plot(log(stk_10km$pad_pano), log(stk_10km$PAD_PPL_1), col=stk_10km$Col)
fit_10km <- lm(Pano_10km ~ Park_10km , data=stk_10km)
r10km <-summary(fit_10km)$r.squared


###PCA for choosing variables for Multilevel model
Cluster <- stk_2008_10km[,c("Pano2008_10km","Park_2008_10km","mangpark10kmZ_COUNT", "Cstdist_largN", "sml_cstN","us_elev_riseN", "Urb_2011N","dist_rdsN", "EUCProtectN","Coast_EdistN","Eu_WaterN", "sec00s1v1_seas_hu_pct", "sec00_pop_below_pov_pct","sec00s1v1_pop_ge65_pct", "sec00_bachplus_ofge25_pct", "frt_US10kmN","Ag_US10kmN","Urb10kmUSN"    )]
mydata <- as.data.frame(na.omit(Cluster)) # listwise deletion of missing
mydata <- scale(mydata)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
biplot(fit, choices = 1:2)
biplot(fit, choices = 3:4)

###correlation matrix
res <- cor(mydata)
round(res, 2)


hist(stk_2008_10km$Pano2008_10km)
###Model examples that you might want to test and look at
###I actually think we should pool the data for this analysis
###linear model
lm_2008_10km <- lm(log(Pano2008_10km) ~ log(Park_2008_10km) + mangpark10kmZ_COUNT + Urb_2011N + Eu_WaterN + sec00s1v1_seas_hu_pct + dist_rdsN + Cstdist_largN + frt_US10kmN + Ag_US10kmN, data=stk_2008_10km)
lm_2008_10km <- lm(log(Pano2008_10km) ~ log(Park_2008_10km) + Ag_US10kmN + sec00_bachplus_ofge25_pct, data=stk_2008_10km)
summary(lm_2008_10km)
vif(lm_2008_10km)

##Poisson regression Panoramio 
poisson_2008_10km <- glm(Pano2008_10km ~ Park_2008_10km + Urb_2011N + Eu_WaterN + sec00s1v1_seas_hu_pct + dist_rdsN + Cstdist_largN + frt_US10kmN + Ag_US10kmN, family="poisson", data=stk_2008_10km)
summary(poisson_2008_10km)

##robust standard errors Cameron and Trivedi (2009)
cov.poisson_2008_10km <- vcovHC(poisson_2008_10km, type="HC0")
std.err <- sqrt(diag(cov.poisson_2008_10km))
r.est <- cbind(Estimate= coef(poisson_2008_10km), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson_2008_10km)/std.err), lower.tail=FALSE),
               LL = coef(poisson_2008_10km) - 1.96 * std.err,
               UL = coef(poisson_2008_10km) + 1.96 * std.err)

r.est

###appropriateness of poisson model; If the test is statistically significant, 
##it would indicate that the data do not fit the model well. In that situation, 
##we may try to determine if there are omitted predictor variables, 
##if our linearity assumption holds and/or if there is an issue of over-dispersion.
with(poisson_2008_10km, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

poisson_2008_10km2 <- update(poisson_2008_10km, . ~ . - Urb_2011N)
anova(poisson_2008_10km2, poisson_2008_10km, test="Chisq")
##marginal & conditional R2
sem.model.fits(poisson_2008_10km)

##Mixed effects Poisson regression 
poisson_lvl_2008_10km <- glmer(Pano2008_10km ~ Park_2008_10km  + Urb_2011N + Eu_WaterN + sec00s1v1_seas_hu_pct + dist_rdsN + Cstdist_largN + frt_US10kmN + Ag_US10kmN + (1|mangpark10kmZ_COUNT), family="poisson",data = stk_2008_10km)
summary(poisson_lvl_2008_10km)

## Negative Binomial Regression Modeling if there is over-dispersion
NegativeB_2008_10km <- glm.nb(Pano2008_10km ~ Park_2008_10km + mangpark10kmZ_COUNT + Urb_2011N + Eu_WaterN + sec00s1v1_seas_hu_pct + dist_rdsN + Cstdist_largN + frt_US10kmN + Ag_US10kmN, data=stk_2008_10km)
summary(NegativeB_2008_10km)

### we might also nees to check for spatial autocorreslation


##2009
Park_2009_10km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2009_10km.tif")
Park_2009_10km[Park_2009_10km < 0.1] <- NA 
plot(Park_2009_10km) 
stk_2009_10km <- stack(Pano2009_10km, Park_2009_10km)
jnk=layerStats(stk_2009_10km, 'pearson', na.rm=T)
corr_matrix2009_10km=jnk$'pearson correlation coefficient'
corr_matrix2009_10km
stk_2009_10km <- as.data.frame(stk_2009_10km)
plot(stk_2009_10km$Pano2009_10km, stk_2009_10km$Park_2009_10km)
fit2009_10km <- lm(Pano2009_10km ~ Park_2009_10km, data=stk_2009_10km)
r10km2009<-summary(fit2009_10km)$r.squared


pearsons1km<-cbind(corr_matrix2008_1km,corr_matrix2009_1km,corr_matrix2010_1km,corr_matrix2011_1km,corr_matrix2012_1km,corr_matrix2013_1km,corr_matrix2014_1km)
colnames(rsqrd1km)<-c("corr_matrix2008_1km","corr_matrix2009_1km","corr_matrix2010_1km","corr_matrix2011_1km","corr_matrix2012_1km","corr_matrix2013_1km","corr_matrix2014_1km")
pearsons1km

pearsons5km<-cbind(corr_matrix2008_5km,corr_matrix2009_5km,corr_matrix2010_5km,corr_matrix2011_5km,corr_matrix2012_5km,corr_matrix2013_5km,corr_matrix2014_5km)
colnames(rsqrd5km)<-c("corr_matrix2008_5km","corr_matrix2009_5km","corr_matrix2010_5km","corr_matrix2011_5km","corr_matrix2012_5km","corr_matrix2013_5km","corr_matrix2014_5km")
pearsons5km

pearsons10km<-cbind(corr_matrix2008_10km,corr_matrix2009_10km,corr_matrix2010_10km,corr_matrix2011_10km,corr_matrix2012_10km,corr_matrix2013_10km,corr_matrix2014_10km)
colnames(rsqrd10km)<-c("corr_matrix2008_10km","corr_matrix2009_10km","corr_matrix2010_10km","corr_matrix2011_10km","corr_matrix2012_10km","corr_matrix2013_10km","corr_matrix2014_10km")
pearsons10km
##rsqrd30m<-cbind(r30m2008,r30m2009,r30m2010,r30m2011,r30m2012,r30m2013,r30m2014)
##rsqrd30m<-cbind(r30m2008,r30m2009,r30m2010,r30m2011,r30m2012,r30m2013,r30m2014)
rsqrd1km<-cbind(r1km2008,r1km2009,r1km2010,r1km2011,r1km2012,r1km2013,r1km2014)
colnames(rsqrd1km)<-c("r1km2008","r1km2009","r1km2010","r1km2011","r1km2012","r1km2013","r1km2014")
rsqrd1km
rsqrd5km<-cbind(r5km2008,r5km2009,r5km2010,r5km2011,r5km2012,r5km2013,r5km2014)
colnames(rsqrd5km)<-c("r5km2008","r5km2009","r5km2010","r5km2011","r5km2012","r5km2013","r5km2014")
rsqrd5km
rsqrd10km<-cbind(r10km2008,r10km2009,r10km2010,r10km2011,r10km2012,r10km2013,r10km2014)
colnames(rsqrd10km)<-c("r10km2008","r10km2009","r10km2010","r10km2011","r10km2012","r10km2013","r10km2014")
rsqrd10km
write.csv(rsqrd10km, "rsqrd10km.csv" )

###MAPS
stk_2008_10km_map<-layerStats(stk_2008_10km, 'pearson')

###test cluster analysis
# Determine number of clusters
wss <- (nrow(stk_2008_10km)-1)*sum(apply(stk_2008_10km,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(stk_2008_10km, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

fit <- kmeans(stk_2008_10km, 5)
clusplot(stk_2008_10km, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)


plot(stk_2008_10km)

fit <- princomp(stk_2008_10km, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)
biplot(fit, choices = 1:2)
biplot(fit, choices = 3:4)


plot()
fit2008_1km <- lm(Pano2008_10km ~ Park_2008_10km, data=stk_2008_10km)


plot(Pano2008_10km ~ Park_2008_10km, data=stk_2008_10km)
abline(fit2008_1km)
r2_fit2008_1km <- round(summary(fit2008_1km)$r.squared, 2)
r2_fit2008_1km

####Stack####
stk_2009_10km <- as.data.frame(stack(Pano2009_10km, Park_2009_10km))
stk_2009_10km <- subset(stk_2009_10km, Park_2009_10km > 0)
plot(stk_2009_10km)
fit2009_1km <- lm(Pano2009_10km ~ Park_2009_10km, data=stk_2009_10km)
plot(Pano2009_10km ~ Park_2009_10km, data=stk_2009_10km)
abline(fit2009_1km)
r2_fit2009_1km <- round(summary(fit2009_1km)$r.squared, 2)
r2_fit2009_1km
