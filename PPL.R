rm(list=ls())
for (package in c('lubridate', 
                  'tibble', 
                  'data.table')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}
list.files()
setwd("C:\\Users\\jamatney\\Desktop\\PhD\\PPL_reservationdata_csv")

# read PPL reservation data
PPL <- read.csv('PPL_reservationdata.csv')

# Bin dataframe
travel <- as.data.frame(PPL$traveldist)
# CUrrent cuts are at 50 miles and 600 miles (in km), leaving three bins, 0-50,50-600, 600-Inf
travel.cuts <- as.data.frame(apply(travel, 2, cut, c(-Inf,c(80.4672, 965.606), Inf), labels=1:3))
PPL <- cbind(PPL,travel.cuts)
colnames(PPL)[20] <- c("TravelBins")

# subset by bin
PPL.1 <- subset(PPL, TravelBins %in% c("1"))
PPL.2 <- subset(PPL, TravelBins %in% c("2"))
PPL.3 <- subset(PPL, TravelBins %in% c("3"))

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


