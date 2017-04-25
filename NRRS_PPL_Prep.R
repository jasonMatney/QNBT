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

# read in PPL binned data
PPL_1 <- read.csv('PPL_binned\\PPL_1.csv')
PPL_2 <- read.csv('PPL_binned\\PPL_2.csv')
PPL_3 <- read.csv('PPL_binned\\PPL_3.csv')

# read in NNRS data
dir2 <- "C:\\Users\\jamatney\\Desktop\\PhD\\NRRS_Summarized_byPark_Shapefile"
ogrListLayers(dir2)
NRRS <- readOGR(dsn=dir2 ,layer="NRRS_byPark_fromPython")

# Load the package
year_list <- c(2008:2014)
bin_list <- list(PPL_1,PPL_2,PPL_3)


# Loop through each year of each binned data frame and write csv
for(i in 1:length(bin_list)){
  PPL_bin <- bin_list[[i]]
  
  PPL_bin_table <- as.data.frame(table(PPL_bin$Park))
  names(PPL_bin_table) <- c("Park", "Freq")
  NRRS_dat_select <- sqldf("SELECT NRRS_dat.Park,
                             Cnt_Park,
                             First_Agen,
                             First_Site,
                             Last_SiteT,
                             First_FacZ,
                             First_Park,
                             Min_ParkLo,
                             Max_ParkLo,
                             ParkLat,
                             Freq
                             FROM NRRS_dat
                             LEFT JOIN PPL_bin_table
                             ON NRRS_dat.Park = PPL_bin_table.Park")
    
  NRRS@data <- cbind(NRRS@data,NRRS_dat_select[,ncol(NRRS_dat_select)])
  names(NRRS@data)[names(NRRS@data) == "NRRS_dat_select[, ncol(NRRS_dat_select)]"] <- paste0("Frq_",i)
  print(paste0("writing file: ", paste0("NRRS_PPL_",i)))
  writeOGR(NRRS, dsn=dir, paste0("NRRS_PPL\\NRRS_PPL_",i), driver="ESRI Shapefile")
  NRRS <- readOGR(dsn=dir2 ,layer="NRRS_byPark_fromPython")
}

# INDIVIDUAL YEARS (NEEDS WORK)
# # Loop through each year of each binned data frame and write csv
# for(i in 1:length(bin_list)){
#   PPL_bin <- bin_list[[i]]
#   for(j in year_list){
#     PPL_bin_year <- PPL_bin[PPL_bin$StartYear == j,]
#     PPL_bin_year_table <- as.data.frame(table(PPL_bin_year$Park))
#     names(PPL_bin_year_table) <- c("Park", "Freq")
#     NRRS_dat_select <- sqldf("SELECT NRRS_dat.Park,
#                              Cnt_Park,
#                              First_Agen,
#                              First_Site,
#                              Last_SiteT,
#                              First_FacZ,
#                              First_Park,
#                              Min_ParkLo,
#                              Max_ParkLo,
#                              ParkLat,
#                              Freq
#                              FROM NRRS_dat
#                              LEFT JOIN PPL_bin_year_table
#                              ON NRRS_dat.Park = PPL_bin_year_table.Park")
#     
#     NRRS@data <- cbind(NRRS@data,NRRS_dat_select[,ncol(NRRS_dat_select)])
#     names(NRRS@data)[names(NRRS@data) == "NRRS_dat_select[, ncol(NRRS_dat_select)]"] <- paste0("Frq_",i,"_",j)
#   }
#   
#   NRRS@data <- cbind(NRRS@data,as.data.frame(rowSums(NRRS@data[,11:17])))
#   names(NRRS@data)[names(NRRS@data) == "rowSums(NRRS@data[, 11:17])"] <- paste0("Freq_",i,"_S")
#   print(paste0("writing file: ", paste0("NRRS_PPL_",i)))
#   writeOGR(NRRS, dsn=dir, paste0("NRRS_PPL\\NRRS_PPL_",i), driver="ESRI Shapefile")
# }
