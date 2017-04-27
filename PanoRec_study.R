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
###load protected areas
# Mparks_10km <- raster("PADUS1_4Arc10gdb/mangpark10kmZ")
# Mparks_5km <- raster("PADUS1_4Arc10gdb/mangpark5kmZ")
# Mparks_1km <- raster("PADUS1_4Arc10gdb/mangpark1kmZ")

ppl_freq_1 <- raster("raster/ppl_freq_1")
ppl_freq_2 <- raster("raster/ppl_freq_2")
ppl_freq_3 <- raster("raster/ppl_freq_3")

##Variables
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

###PPL
PPL_1 <- read.csv(paste0(dir,"/PPL/PPL_1.csv"))
PPL_2 <- read.csv(paste0(dir,"/PPL/PPL_2.csv"))
PPL_3 <- read.csv(paste0(dir,"/PPL/PPL_3.csv"))

##---------##
#-  Stack  -#
##---------##
##10km
ppl_freq_1[ppl_freq_1 < 0.1] <- NA 
ppl_freq_2[ppl_freq_2 < 0.1] <- NA 
ppl_freq_3[ppl_freq_3 < 0.1] <- NA 

plot(ppl_freq_1, col=topo.colors(100))

plot(pctSeasonHomes10km, col=topo.colors(100))

# Resolution - 10 km

ppl_freq_1_stack <- stack(ppl_freq_1, cst_distLG10km, cst_distSM10km, road_dist10km, pop2010, slope10km, elev10km, EUparks, distCoast, distwater, Urban10km, pctUrban10km, frst10km, ag10km, pctSeasonHomes10km, pctPvrty10km, pctRetired10km, pctBachdegree, state, region, division)
ppl_freq_2_stack <- stack(ppl_freq_2, cst_distLG10km, cst_distSM10km, road_dist10km, pop2010, slope10km, elev10km, EUparks, distCoast, distwater, Urban10km, pctUrban10km, frst10km, ag10km, pctSeasonHomes10km, pctPvrty10km, pctRetired10km, pctBachdegree, state, region, division)
ppl_freq_3_stack <- stack(ppl_freq_3, cst_distLG10km, cst_distSM10km, road_dist10km, pop2010, slope10km, elev10km, EUparks, distCoast, distwater, Urban10km, pctUrban10km, frst10km, ag10km, pctSeasonHomes10km, pctPvrty10km, pctRetired10km, pctBachdegree, state, region, division)

##simply correlation calculation
jnk_1=layerStats(ppl_freq_1_stack, 'pearson', na.rm=T)
jnk_2=layerStats(ppl_freq_2_stack, 'pearson', na.rm=T)
jnk_3=layerStats(ppl_freq_3_stack, 'pearson', na.rm=T)

corr_matrix_1_10km=jnk_1$'pearson correlation coefficient'
corr_matrix_2_10km=jnk_2$'pearson correlation coefficient'
corr_matrix_3_10km=jnk_3$'pearson correlation coefficient'

corr_matrix_1_10km
corr_matrix_2_10km
corr_matrix_3_10km

###change into a dataframe
stk_ppl_freq_1_10km <- as.data.frame(ppl_freq_1_stack)
stk_ppl_freq_2_10km <- as.data.frame(ppl_freq_2_stack)
stk_ppl_freq_3_10km <- as.data.frame(ppl_freq_3_stack)
stk_ppl_freq_1_10km <-na.omit(stk_ppl_freq_1_10km)
stk_ppl_freq_2_10km <-na.omit(stk_ppl_freq_2_10km)
stk_ppl_freq_3_10km <-na.omit(stk_ppl_freq_3_10km)

###need to omit pano zeros to use a log transformation
stk_2008_10km <-subset(stk_2008_10km,  Pano2008_10km != 0)

###plot the log transformed correlations
###intialization
rbPal <- colorRampPalette(c('red','blue'))
#This adds a column of color values
# based on the y values
stk_2008_10km$Col <- rbPal(5)[as.numeric(cut(stk_2008_10km$Cstdist_largN,breaks = 5))]
##plot
plot(log(stk_2008_10km$Pano2008_10km), log(stk_2008_10km$Park_2008_10km)) 

###model
fit2008_10km <- lm(log(stk_2008_10km$Pano2008_10km) ~ log(stk_2008_10km$Park_2008_10km), data=stk_2008_10km)
fit2008_10km$coefficients # these are you parameters
summary(fit2008_10km)

# data prep #
# one way to fit a power law is to log10 transformal both variables.  
# However, we want to use the natural log as this was the Nature paper's approach
e <- exp(1) 
plot(y ~ x, data=DF.mean, 
     xlab='pano', 
     ylab='nrrs', 
     log = "yx",
     main = "Linear fit to natural log plot (mean) - B=0.3221555 Yo=474.9223")

linear.fit <- lm(log(y) ~ log(x), data=DF.mean) # natural log
linear.fit$coefficients # these are you parameters

B <- as.numeric(linear.fit$coefficients[2])  # B
Y_0 <- as.numeric(e^linear.fit$coefficients[1])  # Y_0
x <- DF.mean$x
print(paste("B:", B))
print(paste("Yo:", Y_0))
head(x)
ypred <- Y_0*x^B   # this is the predicted curve 
DF.results <- cbind(ypred, x)
head(DF.results)
lines(ypred ~ x, data=DF.results, xlab='pano', ylab='ypred - nrrs',  col='red')


####getting rid of outliers
##stk_2008_10km <- subset(stk_2008_10km, Park_2008_10km <15000)
##stk_2008_10km <- subset(stk_2008_10km, Pano2008_10km <250)

##Plot correlation and other dependencies
#This adds a column of color values
rbPal <- colorRampPalette(c('red','blue'))
# based on the y values
###cost distance
stk_2008_10km$Col <- rbPal(10)[as.numeric(cut(stk_2008_10km$Cstdist_largN,breaks = 10))]

stk_2008_10km$Col <- rbPal(10)[as.numeric(cut(stk_2008_10km$Ag_US10kmN,breaks = 10))]

stk_2008_10km$Col <- rbPal(10)[as.numeric(cut(stk_2008_10km$sec00_bachplus_ofge25_pct,breaks = 10))]


plot(log(stk_2008_10km$Pano2008_10km), log(stk_2008_10km$Park_2008_10km), col=stk_2008_10km$Col)
fit2008_10km <- lm(Pano2008_10km ~ Park_2008_10km , data=stk_2008_10km)
r10km2008<-summary(fit2008_10km)$r.squared


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

##2010
Park_2010_10km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2010_10km.tif")
Park_2010_10km[Park_2010_10km < 0.1] <- NA 
plot(Park_2010_10km) 
stk_2010_10km <- stack(Pano2010_10km, Park_2010_10km)
jnk=layerStats(stk_2010_10km, 'pearson', na.rm=T)
corr_matrix2010_10km=jnk$'pearson correlation coefficient'
corr_matrix2010_10km
stk_2010_10km <- as.data.frame(stk_2010_10km)
plot(stk_2010_10km$Pano2010_10km, stk_2010_10km$Park_2010_10km)
fit2010_10km <- lm(Pano2010_10km ~ Park_2010_10km, data=stk_2010_10km)
r10km2010<-summary(fit2010_10km)$r.squared

##2011
Park_2011_10km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2011_10km.tif")
Park_2011_10km[Park_2011_10km < 0.1] <- NA 
plot(Park_2011_10km) 
stk_2011_10km <- stack(Pano2011_10km, Park_2011_10km)
jnk=layerStats(stk_2011_10km, 'pearson', na.rm=T)
corr_matrix2011_10km=jnk$'pearson correlation coefficient'
corr_matrix2011_10km
stk_2011_10km <- as.data.frame(stk_2011_10km)
plot(stk_2011_10km$Pano2011_10km, stk_2011_10km$Park_2011_10km)
fit2011_10km <- lm(Pano2011_10km ~ Park_2011_10km, data=stk_2011_10km)
r10km2011<-summary(fit2011_10km)$r.squared

##2012
Park_2012_10km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2012_10km.tif")
Park_2012_10km[Park_2012_10km < 0.1] <- NA 
plot(Park_2012_10km) 
stk_2012_10km <- stack(Pano2012_10km, Park_2012_10km)
jnk=layerStats(stk_2012_10km, 'pearson', na.rm=T)
corr_matrix2012_10km=jnk$'pearson correlation coefficient'
corr_matrix2012_10km
stk_2012_10km <- as.data.frame(stk_2012_10km)
plot(stk_2012_10km$Pano2012_10km, stk_2012_10km$Park_2012_10km)
fit2012_10km <- lm(Pano2012_10km ~ Park_2012_10km, data=stk_2012_10km)
r10km2012<-summary(fit2012_10km)$r.squared

##2013
Park_2013_10km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2013_10km.tif")
Park_2013_10km[Park_2013_10km < 0.1] <- NA 
plot(Park_2013_10km) 
stk_2013_10km <- stack(Pano2013_10km, Park_2013_10km)
jnk=layerStats(stk_2013_10km, 'pearson', na.rm=T)
corr_matrix2013_10km=jnk$'pearson correlation coefficient'
corr_matrix2013_10km
stk_2013_10km <- as.data.frame(stk_2013_10km)
plot(stk_2013_10km$Pano2013_10km, stk_2013_10km$Park_2013_10km)
fit2013_10km <- lm(Pano2013_10km ~ Park_2013_10km, data=stk_2013_10km)
r10km2013<-summary(fit2013_10km)$r.squared

##2014
Park_2014_10km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2014_10km.tif")
Park_2014_10km[Park_2014_10km < 0.1] <- NA 
plot(Park_2014_10km) 
stk_2014_10km <- stack(Pano2014_10km, Park_2014_10km)
jnk=layerStats(stk_2014_10km, 'pearson', na.rm=T)
corr_matrix2014_10km=jnk$'pearson correlation coefficient'
corr_matrix2014_10km
stk_2014_10km <- as.data.frame(stk_2014_10km)
plot(stk_2014_10km$Pano2014_10km, stk_2014_10km$Park_2014_10km)
fit2014_10km <- lm(Pano2014_10km ~ Park_2014_10km, data=stk_2014_10km)
r10km2014<-summary(fit2014_10km)$r.squared

###5km
##2008
Park_2008_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2008_5km.tif")
Park_2008_5km[Park_2008_5km < 0.1] <- NA 
plot(Park_2008_5km) 
stk_2008_5km <- stack(Pano2008_5km, Park_2008_5km)
jnk=layerStats(stk_2008_5km, 'pearson', na.rm=T)
corr_matrix2008_5km=jnk$'pearson correlation coefficient'
corr_matrix2008_5km
stk_2008_5km <- as.data.frame(stk_2008_5km)
plot(stk_2008_5km$Pano2008_5km, stk_2008_5km$Park_2008_5km)
fit2008_5km <- lm(Pano2008_5km ~ Park_2008_5km, data=stk_2008_5km)
r5km2008<-summary(fit2008_5km)$r.squared


##2009
Park_2009_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2009_5km.tif")
Park_2009_5km[Park_2009_5km < 0.1] <- NA 
plot(Park_2009_5km) 
stk_2009_5km <- stack(Pano2009_5km, Park_2009_5km)
jnk=layerStats(stk_2009_5km, 'pearson', na.rm=T)
corr_matrix2009_5km=jnk$'pearson correlation coefficient'
corr_matrix2009_5km
stk_2009_5km <- as.data.frame(stk_2009_5km)
plot(stk_2009_5km$Pano2009_5km, stk_2009_5km$Park_2009_5km)
fit2009_5km <- lm(Pano2009_5km ~ Park_2009_5km, data=stk_2009_5km)
r5km2009<-summary(fit2009_5km)$r.squared

##2010
Park_2010_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2010_5km.tif")
Park_2010_5km[Park_2010_5km < 0.1] <- NA 
plot(Park_2010_5km) 
stk_2010_5km <- stack(Pano2010_5km, Park_2010_5km)
jnk=layerStats(stk_2010_5km, 'pearson', na.rm=T)
corr_matrix2010_5km=jnk$'pearson correlation coefficient'
corr_matrix2010_5km
stk_2010_5km <- as.data.frame(stk_2010_5km)
plot(stk_2010_5km$Pano2010_5km, stk_2010_5km$Park_2010_5km)
fit2010_5km <- lm(Pano2010_5km ~ Park_2010_5km, data=stk_2010_5km)
r5km2010<-summary(fit2010_5km)$r.squared

##2011
Park_2011_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2011_5km.tif")
Park_2011_5km[Park_2011_5km < 0.1] <- NA 
plot(Park_2011_5km) 
stk_2011_5km <- stack(Pano2011_5km, Park_2011_5km)
jnk=layerStats(stk_2011_5km, 'pearson', na.rm=T)
corr_matrix2011_5km=jnk$'pearson correlation coefficient'
corr_matrix2011_5km
stk_2011_5km <- as.data.frame(stk_2011_5km)
plot(stk_2011_5km$Pano2011_5km, stk_2011_5km$Park_2011_5km)
fit2011_5km <- lm(Pano2011_5km ~ Park_2011_5km, data=stk_2011_5km)
r5km2011<-summary(fit2011_5km)$r.squared

##2012
Park_2012_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2012_5km.tif")
Park_2012_5km[Park_2012_5km < 0.1] <- NA 
plot(Park_2012_5km) 
stk_2012_5km <- stack(Pano2012_5km, Park_2012_5km)
jnk=layerStats(stk_2012_5km, 'pearson', na.rm=T)
corr_matrix2012_5km=jnk$'pearson correlation coefficient'
corr_matrix2012_5km
stk_2012_5km <- as.data.frame(stk_2012_5km)
plot(stk_2012_5km$Pano2012_5km, stk_2012_5km$Park_2012_5km)
fit2012_5km <- lm(Pano2012_5km ~ Park_2012_5km, data=stk_2012_5km)
r5km2012<-summary(fit2012_5km)$r.squared

##2013
Park_2013_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2013_5km.tif")
Park_2013_5km[Park_2013_5km < 0.1] <- NA 
plot(Park_2013_5km) 
stk_2013_5km <- stack(Pano2013_5km, Park_2013_5km)
jnk=layerStats(stk_2013_5km, 'pearson', na.rm=T)
corr_matrix2013_5km=jnk$'pearson correlation coefficient'
corr_matrix2013_5km
stk_2013_5km <- as.data.frame(stk_2013_5km)
plot(stk_2013_5km$Pano2013_5km, stk_2013_5km$Park_2013_5km)
fit2013_5km <- lm(Pano2013_5km ~ Park_2013_5km, data=stk_2013_5km)
r5km2013<-summary(fit2013_5km)$r.squared

##2014
Park_2014_5km <- raster("S:/cga/dbvanber/Pano_Parks/RecDotGov/Panoramio_Yearly_Raster/Pano/Park_2014_5km.tif")
Park_2014_5km[Park_2014_5km < 0.1] <- NA 
plot(Park_2014_5km) 
stk_2014_5km <- stack(Pano2014_5km, Park_2014_5km)
jnk=layerStats(stk_2014_5km, 'pearson', na.rm=T)
corr_matrix2014_5km=jnk$'pearson correlation coefficient'
corr_matrix2014_5km
stk_2014_5km <- as.data.frame(stk_2014_5km)
plot(stk_2014_5km$Pano2014_5km, stk_2014_5km$Park_2014_5km)
fit2014_5km <- lm(Pano2014_5km ~ Park_2014_5km, data=stk_2014_5km)
r5km2014<-summary(fit2014_5km)$r.squared

###1km
##2008
Park_2008_1km[Park_2008_1km < 0.1] <- NA 
plot(Park_2008_1km) 
stk_2008_1km <- stack(Pano2008_1km, Park_2008_1km)
jnk=layerStats(stk_2008_1km, 'pearson', na.rm=T)
corr_matrix2008_1km=jnk$'pearson correlation coefficient'
corr_matrix2008_1km
stk_2008_1km <- as.data.frame(stk_2008_1km)
plot(stk_2008_1km$Pano2008_1km, stk_2008_1km$Park_2008_1km)
fit2008_1km <- lm(Pano2008_1km ~ Park_2008_1km, data=stk_2008_1km)
r1km2008<-summary(fit2008_1km)$r.squared


##2009
Park_2009_1km[Park_2009_1km < 0.1] <- NA 
plot(Park_2009_1km) 
stk_2009_1km <- stack(Pano2009_1km, Park_2009_1km)
jnk=layerStats(stk_2009_1km, 'pearson', na.rm=T)
corr_matrix2009_1km=jnk$'pearson correlation coefficient'
corr_matrix2009_1km
stk_2009_1km <- as.data.frame(stk_2009_1km)
plot(stk_2009_1km$Pano2009_1km, stk_2009_1km$Park_2009_1km)
fit2009_1km <- lm(Pano2009_1km ~ Park_2009_1km, data=stk_2009_1km)
r1km2009<-summary(fit2009_1km)$r.squared

##2010
Park_2010_1km[Park_2010_1km < 0.1] <- NA 
plot(Park_2010_1km) 
stk_2010_1km <- stack(Pano2010_1km, Park_2010_1km)
jnk=layerStats(stk_2010_1km, 'pearson', na.rm=T)
corr_matrix2010_1km=jnk$'pearson correlation coefficient'
corr_matrix2010_1km
stk_2010_1km <- as.data.frame(stk_2010_1km)
plot(stk_2010_1km$Pano2010_1km, stk_2010_1km$Park_2010_1km)
fit2010_1km <- lm(Pano2010_1km ~ Park_2010_1km, data=stk_2010_1km)
r1km2010<-summary(fit2010_1km)$r.squared

##2011
Park_2011_1km[Park_2011_1km < 0.1] <- NA 
plot(Park_2011_1km) 
stk_2011_1km <- stack(Pano2011_1km, Park_2011_1km)
jnk=layerStats(stk_2011_1km, 'pearson', na.rm=T)
corr_matrix2011_1km=jnk$'pearson correlation coefficient'
corr_matrix2011_1km
stk_2011_1km <- as.data.frame(stk_2011_1km)
plot(stk_2011_1km$Pano2011_1km, stk_2011_1km$Park_2011_1km)
fit2011_1km <- lm(Pano2011_1km ~ Park_2011_1km, data=stk_2011_1km)
r1km2011<-summary(fit2011_1km)$r.squared

##2012
Park_2012_1km[Park_2012_1km < 0.1] <- NA 
plot(Pano2012_1km) 
stk_2012_1km <- stack(Pano2012_1km, Park_2012_1km)
jnk=layerStats(stk_2012_1km, 'pearson', na.rm=T)
corr_matrix2012_1km=jnk$'pearson correlation coefficient'
corr_matrix2012_1km

stk_2012_1km <- as.data.frame(stk_2012_1km)
stk_2012_1km <- na.omit(stk_2012_1km)
plot(stk_2012_1km$Pano2012_1km, stk_2012_1km$Park_2012_1km)
fit2012_1km <- lm(Pano2012_1km ~ Park_2012_1km, data=stk_2012_1km, na.rm=T)
r1km2012<-summary(fit2012_1km)$r.squared


cor(stk_2012_1km$Pano2012_1km, stk_2012_1km$Park_2012_1km)

##2013
Park_2013_1km[Park_2013_1km < 0.1] <- NA 
plot(Park_2013_1km) 
stk_2013_1km <- stack(Pano2013_1km, Park_2013_1km)
jnk=layerStats(stk_2013_1km, 'pearson', na.rm=T)
corr_matrix2013_1km=jnk$'pearson correlation coefficient'
corr_matrix2013_1km
stk_2013_1km <- as.data.frame(stk_2013_1km)
plot(stk_2013_1km$Pano2013_1km, stk_2013_1km$Park_2013_1km)
fit2013_1km <- lm(Pano2013_1km ~ Park_2013_1km, data=stk_2013_1km)
r1km2013<-summary(fit2013_1km)$r.squared

##2014
Park_2014_1km[Park_2014_1km < 0.1] <- NA 
plot(Park_2014_1km) 
stk_2014_1km <- stack(Pano2014_1km, Park_2014_1km)
jnk=layerStats(stk_2014_1km, 'pearson', na.rm=T)
corr_matrix2014_1km=jnk$'pearson correlation coefficient'
corr_matrix2014_1km
stk_2014_1km <- as.data.frame(stk_2014_1km)
plot(stk_2014_1km$Pano2014_1km, stk_2014_1km$Park_2014_1km)
fit2014_1km <- lm(Park_2014_1km ~ Pano2014_1km, data=stk_2014_1km)
r1km2014<-summary(fit2014_1km)$r.squared
r1km2014

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
