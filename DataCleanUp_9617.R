#Shorebird-Aquaculture Model written for BUGS modeling language
#code written by Curtis Burkhalter


#call in required libraries
library('janitor')  #package required to clean up column names automatically

#set working directory
setwd("~/Personal GHub/Shorebird_1617")
pathtofiles <- "E:/Shorebird Project/ShorebirdModel2017/"

#read in raw census data file
RawCensus <- read.delim(file=paste(pathtofiles,"CensusFlatFile.txt",sep=""),header=TRUE) %>% clean_names()

#remove columns corresponding to observations of less frequent species
RawCensus <- RawCensus[,c(1:46,67:72)]

#Need to clean up the column headings
#add suffix "_REKN" to columns corresponding to REKN data (i.e., RawCensus[12:18])
names(RawCensus)[12:18] <- paste(colnames(RawCensus[12:18]),"_REKN",sep="")
View(RawCensus)

#change column names so that any column name ending with "_1" to "_RUTU"
#change column names so that any column name ending with "_2" to "_SESA"
#change column names so that any column name ending with "_3" to "_SAND"
#change column names so that any column name ending with "_4" to "_DUNL"


names(RawCensus) <- gsub("\\_1","_RUTU",names(RawCensus))
names(RawCensus) <- gsub("\\_2","_SESA",names(RawCensus))
names(RawCensus) <- gsub("\\_3","_SAND",names(RawCensus))
names(RawCensus) <- gsub("\\_4","_DUNL",names(RawCensus))
names(RawCensus) <- gsub("shoreline_.*","shore_hab",names(RawCensus))
names(RawCensus) <- gsub("date_.*","date",names(RawCensus))
names(RawCensus) <- gsub("time_.*","time",names(RawCensus))
names(RawCensus) <- gsub("tide_.*","tide",names(RawCensus))
names(RawCensus) <- gsub("wind_direction.*","windD",names(RawCensus))
names(RawCensus) <- gsub("wind_speed.*","windS",names(RawCensus))
names(RawCensus) <- gsub("flock_edge_location","FEL",names(RawCensus))
names(RawCensus) <- gsub("flock_edge_distance_to_racks","FEDR",names(RawCensus))
names(RawCensus) <- gsub("flock_edge_distance_to_tending","FEDT",names(RawCensus))
names(RawCensus) <- gsub("tending_window","TW",names(RawCensus))
names(RawCensus) <- gsub("total_shorebirds.*","TS",names(RawCensus))
names(RawCensus) <- gsub("air_temp.*","AT",names(RawCensus))
names(RawCensus) <- gsub("total_in_segment","TIS",names(RawCensus))
names(RawCensus) <- gsub("flock_center_location","FCL",names(RawCensus))
names(RawCensus) <- gsub("flock_center_distance_to_racks","FCDR",names(RawCensus))
names(RawCensus) <- gsub("flock_center_distance_to_tending","FCDT",names(RawCensus))
names(RawCensus) <- gsub("tending_RUTU.*","tending",names(RawCensus))
names(RawCensus) <- gsub("x_oystermen.*","nOM",names(RawCensus))
names(RawCensus) <- gsub("x_non_oyster.*","nOtherP",names(RawCensus))
names(RawCensus) <- gsub("x_dog.*","dog",names(RawCensus))
names(RawCensus) <- gsub("x_rapt.*","raptor",names(RawCensus))
names(RawCensus) <- gsub("x_low_fly.*","plane",names(RawCensus))

RawCensus$x_SESA <- NULL
RawCensus$x_RUTU <- NULL
RawCensus$x <- NULL




