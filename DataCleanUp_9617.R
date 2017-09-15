#Shorebird-Aquaculture Model written for BUGS modeling language
#code written by Curtis Burkhalter


#call in required libraries
library('janitor')  #package required to clean up column names automatically

#set working directory
setwd("~/Personal GHub/Shorebird_1617")
pathtofiles <- "E:/Shorebird Project/ShorebirdModel2017/"

#read in raw census data file
RawCensus <- read.delim(file=paste(pathtofiles,"CensusFlatFile.txt",sep=""),header=TRUE, stringsAsFactors = FALSE) %>% clean_names()

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

#remove records that contain only a blank space for total shorebirds
#there is no other useful data in these records
RawCensus <- RawCensus[!(RawCensus$TS == ""),]
RawCensus <- RawCensus[!(RawCensus$TIS_SESA == ""),]
RawCensus <- RawCensus[!(RawCensus$TIS_RUTU == ""),]

#for some reason some of the REKN and RUTU counts include negative numbers
RawCensus <- RawCensus[!(RawCensus$TIS_REKN < 0|RawCensus$TIS_RUTU < 0|RawCensus$TIS_DUNL < 0), ]

#Remove record where TIS_SESA = "4/4"
RawCensus$TIS_SESA <- sub("4/4",NA,RawCensus$TIS_SESA)
RawCensus <- RawCensus[!is.na(RawCensus$TIS_SESA),]

#in one entry of 'raptor' column the species was identified
#this drops the species id
RawCensus$raptor <- gsub("\\ BAEA","",RawCensus$raptor)

#in the plane column there are entries with letters
#remove all letters
RawCensus$plane <- gsub("1.*","1",RawCensus$plane)

#convert certain character vectors to factors
RawCensus$segment <- as.factor(RawCensus$segment)
RawCensus$shore_hab <- as.factor(RawCensus$shore_hab)
RawCensus$tide <- as.factor(RawCensus$tide)
RawCensus$date <- as.factor(RawCensus$date)
RawCensus$windD <- as.factor(RawCensus$windD)

#change time so that only numeric
RawCensus$time <- sub(":","",RawCensus$time)

#drop all flock edge location and flock center location columns
RawCensus <- RawCensus[, -grep("FEL.*|FCL.*", colnames(RawCensus))]

#subset records to only include those during the tending window
#tending window times correspond to "TW" = 1
OnlyTending<- RawCensus[!(RawCensus$TW==0|is.na(RawCensus$TW)),]

#remove tending window column now that data is subset
OnlyTending$TW <- NULL

#create new date format for date and remove old one
OnlyTending$date <- as.POSIXct(OnlyTending$date, format = "%m/%d/%Y")

#convert the date to a numeric day from 1-365

OnlyTending$day <- strptime(OnlyTending$date, format = "%Y-%m-%d",tz = "")$yday+1

#create year column
OnlyTending$year <- substr(OnlyTending$date, 1,4)

OnlyTending$date <- NULL

#write function that converts data structures from factors to a numeric sequence
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

#convert segment ids to simple integer value ids
OnlyTending$Nsegment <- as.numeric.factor(OnlyTending$segment)

##convert shore_hab into a numeric id
#Bulkhead=2;Dune=4,Phragmites=6;Marsh=5;Creek=3;Woodland=7
OnlyTending$NSH <- as.numeric.factor(OnlyTending$shore_hab)

#remove all white space from tide column and convert to numeric
#Falling=1;Low=2,Rising=3
OnlyTending$tide <- as.factor(gsub("\\s", "", OnlyTending$tide)) 
OnlyTending$Ntide <- as.numeric.factor(OnlyTending$tide)

#drop unused levels from factors
OnlyTending$windD<-droplevels(OnlyTending$windD)

##convert wind direction into a numeric id
OnlyTending$NWD <- as.numeric.factor(OnlyTending$windD)

#drop non-numeric categorical variable columns
OnlyTending$segment <- OnlyTending$shore_hab <- OnlyTending$tide <- OnlyTending$date <- OnlyTending$windD <- NULL

#drop distance to rack variables for the abundance estimation
OnlyTending <- OnlyTending[, -grep("FEDR.*|FEDT.*|FCDR.*|FCDT.*", colnames(OnlyTending))]

#create 2 datasets that we will be used to fit the models and 
#test predictions of the models

#to make the fit sets comparable in terms of the number 
#of records that actually have tending occurring we look 
#at the number of times that tending equals "1"

tending_events <- length(OnlyTending$tending[OnlyTending$tending == 1])
tending_events

#tending events make up roughly 10% of all records
#so when I create the prediction set records with tending need
#to make up about 10% of all samples; I'm going to subset
#1000 of the records in 'OnlyTending' so 100 should contain
#records with tending in them

TendingRecs <- OnlyTending[OnlyTending$tending == 1,]
sampOT_tendRecs <- TendingRecs[sample(nrow(TendingRecs),100,replace = F),]

NonTendingRecs <- OnlyTending[OnlyTending$tending == 0,]
sampOT_NonTendRecs <- NonTendingRecs[sample(nrow(NonTendingRecs),900,replace = F),]

predict_set <- rbind(sampOT_tendRecs,sampOT_NonTendRecs)
fit_set <- OnlyTending[!rownames(OnlyTending) %in% rownames(predict_set),]

#look at differences in # observations between years and pad
#respective data frames with NAs if they differ
diffNRows <-nrow(fit_set[fit_set$year == "2016",])-
  nrow(fit_set[fit_set$year == "2017",])
diffNRows

NA_mat <-as.data.frame(matrix(nrow=-diffNRows,ncol=ncol(fit_set),NA))
colnames(NA_mat) <- colnames(fit_set)
NA_mat$year <- 2016

fit_set <-rbind(fit_set,NA_mat)

#create species specific datasets from fit_set
REKN <- fit_set[, -grep("RUTU.*|SESA.*|SAND.*|DUNL.*", colnames(fit_set))]
RUTU <- fit_set[, -grep("REKN.*|SESA.*|SAND.*|DUNL.*", colnames(fit_set))]
SESA <- fit_set[, -grep("RUTU.*|REKN.*|SAND.*|DUNL.*", colnames(fit_set))]
SAND <- fit_set[, -grep("RUTU.*|SESA.*|REKN.*|DUNL.*", colnames(fit_set))]
DUNL <- fit_set[, -grep("RUTU.*|SESA.*|SAND.*|REKN.*", colnames(fit_set))]

#look at differences in # observations between years and pad
#respective data frames with NAs if they differ
diffNRows <-nrow(fit_set[fit_set$year == "2016",])-
nrow(fit_set[fit_set$year == "2017",])
diffNRows

#to make the 2016 and 2017 samples the same size we
#randomly sampled rows from the data frame 'OnlyTending'
#OnlyTending16 <- OnlyTending[OnlyTending$year == 2016,]
#OnlyTending17 <- OnlyTending[OnlyTending$year == 2017,]

#sampOT17 <- OnlyTending17[sample(nrow(OnlyTending17),nrow(OnlyTending16),replace = F),]

#OnlyTending <- rbind(OnlyTending16,sampOT17)

#again look at differences in # observations between years and pad
#respective data frames with NAs if they differ
#diffNRows_1 <-nrow(OnlyTending[OnlyTending$year == "2016",])-
#  nrow(OnlyTending[OnlyTending$year == "2017",])
#diffNRows_1
#create year specific datasets from species specific datasets
REKN16 <- REKN[REKN$year == "2016",]
REKN17 <- REKN[REKN$year == "2017",]

RUTU16 <- RUTU[RUTU$year == "2016",]
RUTU17 <- RUTU[RUTU$year == "2017",]

SESA16 <- SESA[SESA$year == "2016",]
SESA17 <- SESA[SESA$year == "2017",]

SAND16 <- SAND[SAND$year == "2016",]
SAND17 <- SAND[SAND$year == "2017",]

DUNL16 <- DUNL[DUNL$year == "2016",]
DUNL17 <- DUNL[DUNL$year == "2017",]

#set up combined spp data structure to be a 2-D array of the following
#form y[i,j,k], where i = observation; j = species; k = year

y <- cbind(REKN16[,grep("TIS.*",colnames(REKN16))],
          REKN17[,grep("TIS.*",colnames(REKN17))],
          RUTU16[,grep("TIS.*",colnames(RUTU16))],
          RUTU17[,grep("TIS.*",colnames(RUTU17))],
          SESA16[,grep("TIS.*",colnames(SESA16))],
          SESA17[,grep("TIS.*",colnames(SESA17))],
          SAND16[,grep("TIS.*",colnames(SAND16))],
          SAND17[,grep("TIS.*",colnames(SAND17))],
          DUNL16[,grep("TIS.*",colnames(DUNL16))],
          DUNL17[,grep("TIS.*",colnames(DUNL17))])

colnames(y) <- c("y[,1,1]","y[,1,2]",
              "y[,2,1]","y[,2,2]",
              "y[,3,1]","y[,3,2]",
              "y[,4,1]","y[,4,2]",
              "y[,5,1]","y[,5,2]")

View(y)
