#Shorebird-Aquaculture Model written for BUGS modeling language
#code written by Curtis Burkhalter


#call in required libraries
library('janitor')  #package required to clean up column names automatically
library('arm') #package written by A. Gelman that will automatically
              #standardize all data by dividing by 2sd

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

#there are a different number of records between 2016 and 2017
#so I will randomly subsampling the fitset records from 2017 so that
#the number of records from 2016 and 2017 is the same
fit_set16<- fit_set[fit_set$year==2016,]
fit_set17 <- fit_set[fit_set$year==2017,]
sampFS17 <- fit_set17[sample(nrow(fit_set17),nrow(fit_set16),replace = F),]

#build final fit_set by combining the records from fit_set16
#and sampFS17

fit_set<- rbind(fit_set16,sampFS17)
fit_set16<-NULL
fit_set17<-NULL

#need to code the categorical variables as dummy variables 
#to ease their coding in WinBUGS; This requires
#code each level of the categorical variable as 0/1;
#for this dataset it is NSH, Ntide
#Bulkhead=2;Dune=4,Phragmites=6;Marsh=5;Creek=3;Woodland=7
#Falling=1;Low=2,Rising=3
fit_set$Bulk <- ifelse(fit_set$NSH == 2,1,0)
fit_set$Dune <- ifelse(fit_set$NSH == 4,1,0)
fit_set$Phrag <- ifelse(fit_set$NSH == 6,1,0)
fit_set$Marsh <- ifelse(fit_set$NSH == 5,1,0)
fit_set$Creek <- ifelse(fit_set$NSH == 3,1,0)
fit_set$Woodland <- ifelse(fit_set$NSH == 7,1,0)

fit_set$FT <- ifelse(fit_set$Ntide == 1, 1,0)
fit_set$LT <- ifelse(fit_set$Ntide == 2, 1,0)
fit_set$RT <- ifelse(fit_set$Ntide == 3, 1,0)

#create species specific datasets from fit_set
REKN <- fit_set[, -grep("RUTU.*|SESA.*|SAND.*|DUNL.*", colnames(fit_set))]
RUTU <- fit_set[, -grep("REKN.*|SESA.*|SAND.*|DUNL.*", colnames(fit_set))]
SESA <- fit_set[, -grep("RUTU.*|REKN.*|SAND.*|DUNL.*", colnames(fit_set))]
SAND <- fit_set[, -grep("RUTU.*|SESA.*|REKN.*|DUNL.*", colnames(fit_set))]
DUNL <- fit_set[, -grep("RUTU.*|SESA.*|SAND.*|REKN.*", colnames(fit_set))]


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

#set up 2 combined spp data structures to be a 2-D arrays of the following
#form y[i,j], where i = observation; j = species; y16 holds
#the 2016 data and y17 holds the 2017 data; need two separate arrays
#because their is more data in 2017 than 2016 which leads to 
#unbalanced array

y16 <- cbind(REKN16[,grep("TIS.*",colnames(REKN16))],
          RUTU16[,grep("TIS.*",colnames(RUTU16))],
          SESA16[,grep("TIS.*",colnames(SESA16))],
          SAND16[,grep("TIS.*",colnames(SAND16))],
          DUNL16[,grep("TIS.*",colnames(DUNL16))])
          

y17 <-  cbind(REKN17[,grep("TIS.*",colnames(REKN17))],
              RUTU17[,grep("TIS.*",colnames(RUTU17))],
              SESA17[,grep("TIS.*",colnames(SESA17))],
              SAND17[,grep("TIS.*",colnames(SAND17))],
              DUNL17[,grep("TIS.*",colnames(DUNL17))])
             
colnames(y16) <- c("y[,1,1]",
              "y[,2,1]",
              "y[,3,1]",
              "y[,4,1]",
              "y[,5,1]")
              

colnames(y17) <- c("y[,1,2]",
                   "y[,2,2]",
                   "y[,3,2]",
                   "y[,4,2]",
                   "y[,5,2]")
                   

yComb <- cbind(y16,y17)

#need to standardize the data before using it in modeling
#but before make sure all the covariate data is numeric
#and not character; see Gelman 2008 for reasoning
#behind scaling regression coefficients for numeric variables
#that are not binary

str(fit_set)
fit_set$TS <- sub("2=4","24",fit_set$TS)

fit_set$time <- as.numeric(fit_set$time)
fit_set$TS <- as.integer(fit_set$TS)
fit_set$nOM <- as.integer(fit_set$nOM)
fit_set$raptor <- as.integer(fit_set$raptor)
fit_set$plane <- as.integer(fit_set$plane)

#subset the relevant covariates
fit_set_covar <- fit_set[,c(2,3,4,5,11:19,22:31)]

#the rescaling leaves the binary variables, including dummies,
#as 0/1 and divides the continuous variables by 2sd
FS_covarSTD <- apply(fit_set_covar[,c(1:11,13:23)],2,rescale, binary.inputs = "0/1")

FS_covarSTD <- cbind(fit_set_covar[,12],FS_covarSTD)
colnames(FS_covarSTD)[1] <- "year"
FS_covarSTD <- as.data.frame(FS_covarSTD)

#create two year specific covariate data sets
Cov16 <- FS_covarSTD[FS_covarSTD$year == 2016,]
Cov17 <- FS_covarSTD[FS_covarSTD$year == 2017,]
Cov16$year <- NULL
Cov17$year <- NULL


covariate_names <- names(Cov16) 
new_names16 <- new_names17 <- character()

for (name in covariate_names) {
  new_names16[name] <- c(paste(name,"[,1]",sep=""))
  new_names17[name] <- c(paste(name,"[,2]",sep=""))
  names(new_names16) <- names(new_names17) <- NULL
}

names(Cov16) <- new_names16
names(Cov17) <- new_names17

CovComb <- cbind(Cov16,Cov17)

#for modeling in BUGS the binary variables cannot take on a
#value of 0 so change all 0 to 1 and all 1 to 2.
var <- colnames(CovComb)[c(5,14:22,27,36:44)]
CovComb[,var] <- lapply(CovComb[,var],function(x) ifelse(x==1,2,1))

#write out the cleaned count data sets that will be used for 
#fitting the community models
write.table(yComb,file=paste(pathtofiles,"DataForFitting/fit_countsCM.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)


#write out the cleaned, standardized covariate data sets that will
#be used for fitting the community models
write.table(CovComb,file=paste(pathtofiles,"DataForFitting/fit_covarsCM.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)

#subset the data to only include that of the REKN
yREKN <- yComb[,c(1,6)]
covREKN <- CovComb

#combine the covariate data with the count data
REKNfit<- cbind(yREKN,covREKN)

#write out the cleaned, standardized covariate data sets that will
#be used for fitting the single species REKN models
write.table(REKNfit,file=paste(pathtofiles,"DataForFitting/fit_REKN.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)


