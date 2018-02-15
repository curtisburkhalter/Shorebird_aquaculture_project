#THIS SCRIPT IS AN EDA SCRIPT
#DATA IS READ IN FIRST AND CLEANED UP AND THEN SEPARATED INTO
#SPECIES SPECIFIC DATASETS
#code written by Curtis Burkhalter


#call in required libraries
suppressMessages(library('janitor'))  #package required to clean up column names automatically
suppressMessages(library('arm')) #package written by A. Gelman that will automatically
#standardize all data by dividing by 2sd
suppressMessages(library('tidyverse'))
suppressMessages(library('lme4'))

#set working directory
setwd("~/GitHub/Shorebird_aquaculture_project")
pathtofiles <- "G:/Shorebird Project/ShorebirdModel2017/"

#read in raw census data file
RawCensus <- read.delim(file=paste(pathtofiles,"Census201617_Updated.txt",sep=""),header=TRUE, stringsAsFactors = FALSE) %>% clean_names()
View(RawCensus)

#need to attach species names to column headings for each set of observations
#REKN columns 14-20
#RUTU columsn 21-27
#SESA columns 28-34
#SAND columns 35-41
#DUNL columns 42-48

RawCensus[1,14:20] <- paste("REKN",RawCensus[1,14:20],sep=" ")
RawCensus[1,21:27] <- paste("RUTU",RawCensus[1,21:27],sep=" ")
RawCensus[1,28:34] <- paste("SESA",RawCensus[1,28:34],sep=" ")
RawCensus[1,35:41] <- paste("SAND",RawCensus[1,35:41],sep=" ")
RawCensus[1,42:48] <- paste("DUNL",RawCensus[1,42:48],sep=" ")

#replace header column with column entries from row 1; these
#should have been the column headers in the file
colnames(RawCensus) <- RawCensus[1,]

#Remove all column entries for row 1
RawCensus <- RawCensus[-1,]

#remove columns corresponding to observations of less frequent species 
#columns 50-69; also remove notes columns 77-79
RawCensus <- RawCensus[,-(c(50:69,77:79))]


#change column names to format that I like

names(RawCensus) <- gsub("Shoreline.*","shore_hab",names(RawCensus))
names(RawCensus) <- gsub("Date.*","date",names(RawCensus))
names(RawCensus) <- gsub("Time.*","time",names(RawCensus))
names(RawCensus) <- gsub("Tide.*","tide",names(RawCensus))
names(RawCensus) <- gsub("Wind Direction.*","windD",names(RawCensus))
names(RawCensus) <- gsub("Wind Speed.*","windS",names(RawCensus))
names(RawCensus) <- gsub("Flock EDGE Location","FEL",names(RawCensus))
names(RawCensus) <- gsub("Flock EDGE Distance to RACKS","FEDR",names(RawCensus))
names(RawCensus) <- gsub("Flock EDGE Distance to TENDING","FEDT",names(RawCensus))
names(RawCensus) <- gsub(".*Window","TW",names(RawCensus))
names(RawCensus) <- gsub("Total # Shorebirds.*","TS",names(RawCensus))
names(RawCensus) <- gsub("Air Temp.*","AT",names(RawCensus))
names(RawCensus) <- gsub("Total # in segment","TIS",names(RawCensus))
names(RawCensus) <- gsub("Flock CENTER Location","FCL",names(RawCensus))
names(RawCensus) <- gsub("Flock CENTER Distance to RACKS","FCDR",names(RawCensus))
names(RawCensus) <- gsub("Flock CENTER Distance to TENDING","FCDT",names(RawCensus))
names(RawCensus) <- gsub("Tending?.*","tending",names(RawCensus))
names(RawCensus) <- gsub("#oystermen.*","nOM",names(RawCensus))
names(RawCensus) <- gsub("#non-oyster.*","nOtherP",names(RawCensus))
names(RawCensus) <- gsub("#dog.*","dog",names(RawCensus))
names(RawCensus) <- gsub("# rapt.*","raptor",names(RawCensus))
names(RawCensus) <- gsub("#low-fly.*","plane",names(RawCensus))
names(RawCensus) <- gsub("#Activ.*","activities",names(RawCensus))
names(RawCensus) <- gsub("Segment #.*","segment",names(RawCensus))
names(RawCensus) <- gsub("#Gulls.*","nGulls",names(RawCensus))

#remove records that contain only a blank space or other character entries
#for total shorebirds|REKN TIS|RUTU TIS|SESA TIS|SAND TIS|DUNL TIS
#there is no other useful data in these records, also any place where there is 
#the character entry "N/A" or "2=4"
RawCensus <- RawCensus[!(RawCensus$TS == ""|RawCensus$TS == "N/A"|RawCensus$TS == "2=4"),]
RawCensus <- RawCensus[!(RawCensus$`SESA TIS` == "20/10"|RawCensus$`SESA TIS` == " 4/4"|RawCensus$`SESA TIS` == ""),]
RawCensus <- RawCensus[!(RawCensus$`RUTU TIS` == ""|RawCensus$`RUTU TIS` == "~"),]

#need to convert all of the bird counts to numeric values
RawCensus$TS <- as.numeric(RawCensus$TS)
RawCensus$`REKN TIS` <- as.numeric(RawCensus$`REKN TIS`)
RawCensus$`SESA TIS` <- as.numeric(RawCensus$`SESA TIS`)
RawCensus$`RUTU TIS` <- as.numeric(RawCensus$`RUTU TIS`)
RawCensus$`SAND TIS` <- as.numeric(RawCensus$`SAND TIS`)
RawCensus$`DUNL TIS` <- as.numeric(RawCensus$`DUNL TIS`)

#for some reason some of the REKN,DUNL and RUTU counts include negative numbers
RawCensus <- RawCensus[!(RawCensus$`REKN TIS` < 0|RawCensus$`RUTU TIS` < 0|RawCensus$`DUNL TIS` < 0), ]

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

#drop all flock center location columns
RawCensus <- RawCensus[, -grep("FCL.*", colnames(RawCensus))]
RawCensus <- RawCensus[, -grep("FCDR.*", colnames(RawCensus))]
RawCensus <- RawCensus[, -grep("FCDT.*", colnames(RawCensus))]

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
#Bulkhead=1;Dune=3,Phragmites=5;Marsh=4;Creek=2;Woodland=6
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
OnlyTending$segment <- OnlyTending$shore_hab <- OnlyTending$tide <- OnlyTending$date <- OnlyTending$windD <- OnlyTending$SegmentInt <- NULL
OnlyTending$`Julian Day` <- NULL
OnlyTending$`Census #` <- NULL

fit_set <- OnlyTending

#need to code the categorical variables as dummy variables 
#to ease their coding in WinBUGS; This requires
#code each level of the categorical variable as 0/1;
#for this dataset it is NSH, Ntide
#Bulkhead=1;Dune=3,Phragmites=5;Marsh=4;Creek=2;Woodland=6
#Falling=1;Low=2,Rising=3;
#in doing the coding this way the regression coefficients
#represent the mean value for each of the respective shoreline types
#and the model intercept represents the common slope across all habitat types
fit_set$Bulk <- ifelse(fit_set$NSH == 1,1,0)
fit_set$Dune <- ifelse(fit_set$NSH == 3,1,0)
fit_set$Phrag <- ifelse(fit_set$NSH == 5,1,0)
fit_set$Marsh <- ifelse(fit_set$NSH == 4,1,0)
fit_set$Creek <- ifelse(fit_set$NSH == 2,1,0)
fit_set$Woodland <- ifelse(fit_set$NSH == 6,1,0)

fit_set$FT <- ifelse(fit_set$Ntide == 1, 1,0)
fit_set$LT <- ifelse(fit_set$Ntide == 2, 1,0)
fit_set$RT <- ifelse(fit_set$Ntide == 3, 1,0)


#need to standardize the data before using it in modeling
#but before make sure all the covariate data is numeric
#and not character; see Gelman 2008 for reasoning
#behind scaling regression coefficients for numeric variables
#that are not binary

str(fit_set)

fit_set$time <- as.numeric(fit_set$time)
fit_set$TS <- as.integer(fit_set$TS)
fit_set$nOM <- as.integer(fit_set$nOM)
fit_set$raptor <- as.integer(fit_set$raptor)
fit_set$plane <- as.integer(fit_set$plane)
fit_set$activities <- as.integer(fit_set$activities)
fit_set$nGulls <- as.integer(fit_set$nGulls)
fit_set$year <- as.integer(fit_set$year)
fit_set$nOtherP <- as.integer(fit_set$nOtherP)
fit_set$dog <- as.integer(fit_set$dog)
fit_set$tending <- as.integer(fit_set$tending)
fit_set$windS <- as.numeric(fit_set$windS)
fit_set$AT <- as.numeric(fit_set$AT)

#drop all FEL and FEDT columns from fit_set
fit_set <- fit_set[, -grep("FEL.*", colnames(fit_set))]
fit_set <- fit_set[, -grep("FEDT.*", colnames(fit_set))]

#create species specific datasets from fit_set
REKN <- fit_set[, -grep("RUTU.*|SESA.*|SAND.*|DUNL.*", colnames(fit_set))]
RUTU <- fit_set[, -grep("REKN.*|SESA.*|SAND.*|DUNL.*", colnames(fit_set))]
SESA <- fit_set[, -grep("RUTU.*|REKN.*|SAND.*|DUNL.*", colnames(fit_set))]
SAND <- fit_set[, -grep("RUTU.*|SESA.*|REKN.*|DUNL.*", colnames(fit_set))]
DUNL <- fit_set[, -grep("RUTU.*|SESA.*|SAND.*|REKN.*", colnames(fit_set))]

#use dummy variable coding for each species to create a series of distance
#classes; "~" = DC1; "+" = DC2; "0" = DC3, "1" = DC4;
#"2" = DC5; "3" = DC6; "4" = DC7, "5" = DC8;
REKN$`REKN FEDR` <- ifelse(REKN$`REKN FEDR`=="O","0",REKN$`REKN FEDR`)

colnames(REKN)[6] <- "FEDR"
colnames(RUTU)[6] <- "FEDR"
colnames(SESA)[6] <- "FEDR"
colnames(SAND)[6] <- "FEDR"
colnames(DUNL)[6] <- "FEDR"


REKN$FEDR <- ifelse(REKN$FEDR == "~", "6", REKN$FEDR)
REKN$FEDR <- ifelse(REKN$FEDR == "+", "7", REKN$FEDR)

RUTU$FEDR <- ifelse(RUTU$FEDR == "~", "6", RUTU$FEDR)
RUTU$FEDR <- ifelse(RUTU$FEDR == "+", "7", RUTU$FEDR)

SESA$FEDR <- ifelse(SESA$FEDR == "~", "6", SESA$FEDR)
SESA$FEDR <- ifelse(SESA$FEDR == "+", "7", SESA$FEDR)

SAND$FEDR <- ifelse(SAND$FEDR == "~", "6", SAND$FEDR)
SAND$FEDR <- ifelse(SAND$FEDR == "+", "7", SAND$FEDR)

DUNL$FEDR <- ifelse(DUNL$FEDR == "~", "6", DUNL$FEDR)
DUNL$FEDR <- ifelse(DUNL$FEDR == "+", "7", DUNL$FEDR)

#rename fit_set to something more intuitive
combined_spp <- fit_set

#rename column with name 'REKN TIS' to 'totREKN'
colnames(REKN)[5] <- "totREKN"

#look at range of counts
rngREKN <- range(REKN$totREKN)
rngRKEN

#create plot of counts by time of day
ggplot(REKN, aes(x = time, y = totREKN)) +
  geom_point() +
  scale_y_continuous(limits = c(0,rngREKN[2])) +
  ylab("Total number of REKN counted") +
  xlab("Time of day")+
  facet_wrap(~year)