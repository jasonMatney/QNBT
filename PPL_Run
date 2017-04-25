rm(list=ls())
for (package in c('lubridate', 
                  'tibble', 
                  'data.table',
                  'sp',
                  'rgdal',
                  'sqldf')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}
list.files()
dir <- "C:\\Users\\jamatney\\Desktop\\PhD\\PPL_reservationdata_csv"
setwd(dir)


PPL_1 <- read.csv('PPL_binned\\PPL_1.csv')
PPL_2 <- read.csv('PPL_binned\\PPL_2.csv')
PPL_3 <- read.csv('PPL_binned\\PPL_3.csv')

head(PPL.1)

dir2 <- "C:\\Users\\jamatney\\Desktop\\PhD\\NRRS_Summarized_byPark_Shapefile"
ogrListLayers(dir2)
NRRS <- readOGR(dsn=dir2 ,layer="NRRS_byPark_fromPython")
NRRS.dat <- NRRS@data

# Load the package
library(sqldf)

sqldf("select ID from PPL_1 limit 10")
