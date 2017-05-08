rm(list=ls())
for (package in c('lubridate', 
                  'tibble', 
                  'data.table',
                  'sp',
                  'rgdal',
                  'sqldf',
                  'raster')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}

source("shp_2_raster.R")

dir <- "F:\\Parks_Paper\\PPL"
setwd(dir)

# read PPL reservation data
PPL <- read.csv('PPL_reservationdata.csv')
# PPL.rast <- rasterize(as.data.frame(PPL[,c("parklon", "parklat")]))
# coordinates(PPL) <- c("parklon", "parklat")
# proj4string(PPL) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# writeOGR(PPL, dsn=dir ,layer="PPL",driver="ESRI Shapefile")

# Bin dataframe
travel <- as.data.frame(PPL$traveldist)
# summary(travel)
# nrow(travel) = 12473816
# NA's = 470524

# 12473816 - 470524 = **12003292**

# CUrrent cuts are at 50 miles and 600 miles (in km), leaving three bins, 0-50,50-600, 600-Inf
travel.cuts <- as.data.frame(apply(travel, 2, cut, c(-Inf,c(80.4672, 965.606), Inf), labels=1:3))
PPL <- cbind(PPL,travel.cuts)
colnames(PPL)[20] <- c("TravelBins")

# subset by bin
PPL.1 <- subset(PPL, TravelBins %in% c("1"))
PPL.2 <- subset(PPL, TravelBins %in% c("2"))
PPL.3 <- subset(PPL, TravelBins %in% c("3"))

# nrow(PPL.1) = 4732053
# nrow(PPL.2) = 6220545
# nrow(PPL.3) = 1050694

# 4732053 + 6220545 + 1050694 = **12003292**

# check travel distnaces in bins
hist(PPL.1$traveldist)
hist(PPL.2$traveldist)
hist(PPL.3$traveldist)

# separate StartDate into day, month, and year
PPL.1 <- add_column(PPL.1, year(dmy(PPL.1$StartDate)), .after = "StartDate")
PPL.1 <- add_column(PPL.1, month(dmy(PPL.1$StartDate)), .after = "StartDate")
PPL.1 <- add_column(PPL.1, day(dmy(PPL.1$StartDate)), .after = "StartDate")
setnames(PPL.1, old = c("day(dmy(PPL.1$StartDate))","month(dmy(PPL.1$StartDate))","year(dmy(PPL.1$StartDate))"), new = c("StartDay","StartMonth","StartYear"))

PPL.2 <- add_column(PPL.2, year(dmy(PPL.2$StartDate)), .after = "StartDate")
PPL.2 <- add_column(PPL.2, month(dmy(PPL.2$StartDate)), .after = "StartDate")
PPL.2 <- add_column(PPL.2, day(dmy(PPL.2$StartDate)), .after = "StartDate")
setnames(PPL.2, old = c("day(dmy(PPL.2$StartDate))","month(dmy(PPL.2$StartDate))","year(dmy(PPL.2$StartDate))"), new = c("StartDay","StartMonth","StartYear"))

PPL.3 <- add_column(PPL.3, year(dmy(PPL.3$StartDate)), .after = "StartDate")
PPL.3 <- add_column(PPL.3, month(dmy(PPL.3$StartDate)), .after = "StartDate")
PPL.3 <- add_column(PPL.3, day(dmy(PPL.3$StartDate)), .after = "StartDate")
setnames(PPL.3, old = c("day(dmy(PPL.3$StartDate))","month(dmy(PPL.3$StartDate))","year(dmy(PPL.3$StartDate))"), new = c("StartDay","StartMonth","StartYear"))

# make list of binned data frames
# THIS REDUCES THE NUMBER OF RECORDS IN THE OVERALL DATABASE
# BECASUE WE'RE ONLY INTERSTED IN YEARS 2008-2014
# BECASUE THAT'S WHAT WE HAVE PANORAMIO DATA AVAILABLE FOR!
PPL.list <- list(PPL.1,PPL.2,PPL.3)
PPL.years <- c(2008:2014)

# subset by year
PPL.1 <- subset(PPL.1, StartYear %in% PPL.years)
PPL.2 <- subset(PPL.2, StartYear %in% PPL.years)
PPL.3 <- subset(PPL.3, StartYear %in% PPL.years)

# nrow(PPL.1) = 4032209
# nrow(PPL.2) = 5064453
# nrow(PPL.3) = 801423
# 4032209 + 5064453 + 801423 = 9898085 

# Loop through each year of each binned data frame and write csv
# for(i in 1:length(PPL.list)){
#   PPL.df <- PPL.list[[i]]
#   for(j in PPL.years){
#     PPL.year <- PPL.df[PPL.df$StartYear == j,]
#     PPL.file <- paste0("PPL_",i, "_", j, ".csv")
#     print(paste0("writing file: ", PPL.file))
#     write.csv(PPL.year, paste0(dir,"\\PPL_binned_years\\",PPL.file))
#   }
# }


for(i in 1:length(PPL.list)){
  PPL.df <- PPL.list[[i]]
  PPL.file <- paste0("PPL_",i, ".csv")
  print(paste0("writing file: ", PPL.file))
  write.csv(PPL.df, paste0(dir, "\\", PPL.file), row.names=FALSE)
}

##--------------------##
## Pooled Raster code ##
##--------------------##

dir2 <- "F:\\Parks_Paper\\NRRS_PPL\\NRRS_PPL.gdb"

ogrListLayers(dir2)
NRRS_PPL_1 <- readOGR("NRRS_PPL_1", dsn=dir2)
NRRS_PPL_2 <- readOGR("NRRS_PPL_2", dsn=dir2)
NRRS_PPL_3 <- readOGR("NRRS_PPL_3", dsn=dir2)

NRRS_PPL_1.dat <- as.data.frame(NRRS_PPL_1@data)
NRRS_PPL_2.dat <- as.data.frame(NRRS_PPL_2@data)
NRRS_PPL_3.dat <- as.data.frame(NRRS_PPL_3@data)

NRRS_PPL.dat <- NRRS_PPL_1.dat
NRRS_PPL.dat$PPL_Freq2 <- NRRS_PPL_2.dat[,c("PPL_Freq")]
NRRS_PPL.dat$PPL_Freq3 <- NRRS_PPL_3.dat[,c("PPL_Freq")]
head(NRRS_PPL.dat)

NRRS_PPL.dat$PPL_Freq_Full <- rowSums(NRRS_PPL.dat[,c("PPL_Freq", "PPL_Freq2", "PPL_Freq3")])
drops <- c("PPL_Freq","PPL_Freq2","PPL_Freq3")
NRRS_PPL.dat <- NRRS_PPL.dat[ , !(names(NRRS_PPL.dat) %in% drops)]
NRRS_PPL_1@data <- NRRS_PPL.dat
NRRS_PPL.pooled <- NRRS_PPL_1
head(NRRS_PPL.pooled)

writeOGR(NRRS_PPL.pooled, dsn=dir, layer="NRRS_PPL_pooled", driver="ESRI Shapefile")

