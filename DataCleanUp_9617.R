#Shorebird-Aquaculture Model written for BUGS modeling language
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
OnlyTending$segment <- OnlyTending$shore_hab <- OnlyTending$tide <- OnlyTending$date <- OnlyTending$windD <- OnlyTending$SegmentInt <- NULL
OnlyTending$`Julian Day` <- NULL
OnlyTending$`Census #` <- NULL

fit_set <- OnlyTending

#need to code the categorical variables as dummy variables 
#to ease their coding in WinBUGS; This requires
#code each level of the categorical variable as 0/1;
#for this dataset it is NSH, Ntide
#Bulkhead=2;Dune=4,Phragmites=6;Marsh=5;Creek=3;Woodland=7
#Falling=1;Low=2,Rising=3;
#in doing the coding this way the regression coefficients
#represent the mean value for each of the respective shoreline types
#and the model intercept represents the common slope across all habitat types
fit_set$Bulk <- ifelse(fit_set$NSH == 2,1,0)
fit_set$Dune <- ifelse(fit_set$NSH == 4,1,0)
fit_set$Phrag <- ifelse(fit_set$NSH == 6,1,0)
fit_set$Marsh <- ifelse(fit_set$NSH == 5,1,0)
fit_set$Creek <- ifelse(fit_set$NSH == 3,1,0)
fit_set$Woodland <- ifelse(fit_set$NSH == 7,1,0)

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

RUTU$DC1 <- ifelse(RUTU$`RUTU FEDR` == "~",1,0)
RUTU$DC2 <- ifelse(RUTU$`RUTU FEDR` == "+",1,0)
RUTU$DC3 <- ifelse(RUTU$`RUTU FEDR` == "0",1,0)
RUTU$DC4 <- ifelse(RUTU$`RUTU FEDR` == "1",1,0)
RUTU$DC5 <- ifelse(RUTU$`RUTU FEDR` == "2",1,0)
RUTU$DC6 <- ifelse(RUTU$`RUTU FEDR` == "3",1,0)
RUTU$DC7 <- ifelse(RUTU$`RUTU FEDR` == "4",1,0)
RUTU$DC8 <- ifelse(RUTU$`RUTU FEDR` == "5",1,0)

SESA$DC1 <- ifelse(SESA$`SESA FEDR` == "~",1,0)
SESA$DC2 <- ifelse(SESA$`SESA FEDR` == "+",1,0)
SESA$DC3 <- ifelse(SESA$`SESA FEDR` == "0",1,0)
SESA$DC4 <- ifelse(SESA$`SESA FEDR` == "1",1,0)
SESA$DC5 <- ifelse(SESA$`SESA FEDR` == "2",1,0)
SESA$DC6 <- ifelse(SESA$`SESA FEDR` == "3",1,0)
SESA$DC7 <- ifelse(SESA$`SESA FEDR` == "4",1,0)
SESA$DC8 <- ifelse(SESA$`SESA FEDR` == "5",1,0)

SAND$DC1 <- ifelse(SAND$`SAND FEDR` == "~",1,0)
SAND$DC2 <- ifelse(SAND$`SAND FEDR` == "+",1,0)
SAND$DC3 <- ifelse(SAND$`SAND FEDR` == "0",1,0)
SAND$DC4 <- ifelse(SAND$`SAND FEDR` == "1",1,0)
SAND$DC5 <- ifelse(SAND$`SAND FEDR` == "2",1,0)
SAND$DC6 <- ifelse(SAND$`SAND FEDR` == "3",1,0)
SAND$DC7 <- ifelse(SAND$`SAND FEDR` == "4",1,0)
SAND$DC8 <- ifelse(SAND$`SAND FEDR` == "5",1,0)

DUNL$DC1 <- ifelse(DUNL$`DUNL FEDR` == "~",1,0)
DUNL$DC2 <- ifelse(DUNL$`DUNL FEDR` == "+",1,0)
DUNL$DC3 <- ifelse(DUNL$`DUNL FEDR` == "0",1,0)
DUNL$DC4 <- ifelse(DUNL$`DUNL FEDR` == "1",1,0)
DUNL$DC5 <- ifelse(DUNL$`DUNL FEDR` == "2",1,0)
DUNL$DC6 <- ifelse(DUNL$`DUNL FEDR` == "3",1,0)
DUNL$DC7 <- ifelse(DUNL$`DUNL FEDR` == "4",1,0)
DUNL$DC8 <- ifelse(DUNL$`DUNL FEDR` == "5",1,0)

REKN$DC1 <- ifelse(REKN$`REKN FEDR` == "~",1,0)
REKN$DC2 <- ifelse(REKN$`REKN FEDR` == "+",1,0)
REKN$DC3 <- ifelse(REKN$`REKN FEDR` == "0",1,0)
REKN$DC4 <- ifelse(REKN$`REKN FEDR` == "1",1,0)
REKN$DC5 <- ifelse(REKN$`REKN FEDR` == "2",1,0)
REKN$DC6 <- ifelse(REKN$`REKN FEDR` == "3",1,0)
REKN$DC7 <- ifelse(REKN$`REKN FEDR` == "4",1,0)
REKN$DC8 <- ifelse(REKN$`REKN FEDR` == "5",1,0)

#subset the relevant covariates for each species
REKN_set_covar <- REKN[,c(1:4,7:17,20:37)]
RUTU_set_covar <- RUTU[,c(1:4,7:17,20:37)]
SESA_set_covar <- SESA[,c(1:4,7:17,20:37)]
SAND_set_covar <- SAND[,c(1:4,7:17,20:37)]
DUNL_set_covar <- DUNL[,c(1:4,7:17,20:37)]
  
#the rescaling leaves the binary variables, including dummies,
#as 0/1 and divides the continuous variables by 2sd, exclude random effect variables
REKN_covarSTD <- apply(REKN_set_covar[,c(1:13,17:33)],2,rescale, binary.inputs = "0/1")
RUTU_covarSTD <- apply(RUTU_set_covar[,c(1:13,17:33)],2,rescale, binary.inputs = "0/1")
SESA_covarSTD <- apply(SESA_set_covar[,c(1:13,17:33)],2,rescale, binary.inputs = "0/1")
SAND_covarSTD <- apply(SAND_set_covar[,c(1:13,17:33)],2,rescale, binary.inputs = "0/1")
DUNL_covarSTD <- apply(DUNL_set_covar[,c(1:13,17:33)],2,rescale, binary.inputs = "0/1")

REKN_covarSTD <- cbind(REKN_set_covar[,14:16],REKN_covarSTD)
colnames(REKN_covarSTD)[1:3] <- c("year","Nsegment","NWD")

RUTU_covarSTD <- cbind(RUTU_set_covar[,14:16],RUTU_covarSTD)
colnames(RUTU_covarSTD)[1:3] <- c("year","Nsegment","NWD")

SESA_covarSTD <- cbind(SESA_set_covar[,14:16],SESA_covarSTD)
colnames(SESA_covarSTD)[1:3] <- c("year","Nsegment","NWD")

SAND_covarSTD <- cbind(SAND_set_covar[,14:16],SAND_covarSTD)
colnames(SAND_covarSTD)[1:3] <- c("year","Nsegment","NWD")

DUNL_covarSTD <- cbind(DUNL_set_covar[,14:16],DUNL_covarSTD)
colnames(DUNL_covarSTD)[1:3] <- c("year","Nsegment","NWD")

#the rescaling process resulted in certain variables being converted to NaN
#b/c they only had 0's as the value; need to fix these for each of the 
#standardized covariate dataframes
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

REKN_covarSTD[is.nan.data.frame(REKN_covarSTD)] <- 0
RUTU_covarSTD[is.nan.data.frame(RUTU_covarSTD)] <- 0
SESA_covarSTD[is.nan.data.frame(SESA_covarSTD)] <- 0
SAND_covarSTD[is.nan.data.frame(SAND_covarSTD)] <- 0
DUNL_covarSTD[is.nan.data.frame(DUNL_covarSTD)] <- 0


#for modeling in JAGS the binary variables cannot take on a
#value of 0 so change all 0 to 1 and all 1 to 2.
#write function that checks whether a column is binary
#specifically the functions says 'are all values in 'x' within the sequence 0 to 1
binary_check <- function(x) { all(x %in% 0:1) }

#apply the binary_check function to subset the column names of a species
#standardized covariate dataframe if they are binary
REKN_Vars <- colnames(REKN_covarSTD)[apply(REKN_covarSTD,2,binary_check)]

#Using only the subsetted columns names for those binary variables now
#change the 0's to 1's and 1's to 2's
REKN_covarSTD[,REKN_Vars] <- lapply(REKN_covarSTD[,REKN_Vars],function(x) ifelse(x==1,2,1))

#repeat for the other 4 species
RUTU_Vars <- colnames(RUTU_covarSTD)[apply(RUTU_covarSTD,2,binary_check)]
RUTU_covarSTD[,RUTU_Vars] <- lapply(RUTU_covarSTD[,RUTU_Vars],function(x) ifelse(x==1,2,1))

SESA_Vars <- colnames(SESA_covarSTD)[apply(SESA_covarSTD,2,binary_check)]
SESA_covarSTD[,SESA_Vars] <- lapply(SESA_covarSTD[,SESA_Vars],function(x) ifelse(x==1,2,1))

SAND_Vars <- colnames(SAND_covarSTD)[apply(SAND_covarSTD,2,binary_check)]
SAND_covarSTD[,SAND_Vars] <- lapply(SAND_covarSTD[,SAND_Vars],function(x) ifelse(x==1,2,1))

DUNL_Vars <- colnames(DUNL_covarSTD)[apply(DUNL_covarSTD,2,binary_check)]
DUNL_covarSTD[,DUNL_Vars] <- lapply(DUNL_covarSTD[,DUNL_Vars],function(x) ifelse(x==1,2,1))

#combine the species-specific counts with their standardized covariates
REKN_final <- cbind(REKN$`REKN TIS`,REKN_covarSTD)
colnames(REKN_final)[1] <- "REKN"

RUTU_final <- cbind(RUTU$`RUTU TIS`,RUTU_covarSTD)
colnames(RUTU_final)[1] <- "RUTU"

SESA_final <- cbind(SESA$`SESA TIS`,SESA_covarSTD)
colnames(SESA_final)[1] <- "SESA"

SAND_final <- cbind(SAND$`SAND TIS`,SAND_covarSTD)
colnames(SAND_final)[1] <- "SAND"

DUNL_final <- cbind(DUNL$`DUNL TIS`,DUNL_covarSTD)
colnames(DUNL_final)[1] <- "DUNL"

#write out the cleaned data sets that will
#be used for fitting the models
write_tsv(REKN_final,path=paste(pathtofiles,"DataForFitting/REKN_fitting012818.txt",sep=""),col_names=TRUE)
write_tsv(RUTU_final,path=paste(pathtofiles,"DataForFitting/RUTU_fitting012818.txt",sep=""),col_names=TRUE)
write_tsv(SESA_final,path=paste(pathtofiles,"DataForFitting/SESA_fitting012818.txt",sep=""),col_names=TRUE)
write_tsv(SAND_final,path=paste(pathtofiles,"DataForFitting/SAND_fitting012818.txt",sep=""),col_names=TRUE)
write_tsv(DUNL_final,path=paste(pathtofiles,"DataForFitting/DUNL_fitting012818.txt",sep=""),col_names=TRUE)

#write up some initial values for fit statistic "NREKN.new","seg_rand","WD_rand"
NREKN.new <- as.data.frame(rep(1,times=nrow(REKN_final)))
colnames(NREKN.new)<-"NREKN.new[]"

NRUTU.new <- as.data.frame(rep(1,times=nrow(RUTU_final)))
colnames(NRUTU.new)<-"NRUTU.new[]"

NSESA.new <- as.data.frame(rep(1,times=nrow(SESA_final)))
colnames(NSESA.new)<-"NSESA.new[]"

NSAND.new <- as.data.frame(rep(1,times=nrow(SAND_final)))
colnames(NSAND.new)<-"NSAND.new[]"

NDUNL.new <- as.data.frame(rep(1,times=nrow(DUNL_final)))
colnames(NDUNL.new)<-"NDUNL.new[]"

seg_rand <- as.data.frame(as.integer(rep(1,times=length(unique(REKN_final$Nsegment)))))
colnames(seg_rand) <- "seg_rand[]"

WD_rand <- as.data.frame(as.integer(rep(1,time=length(unique(REKN_final$NWD)))))
colnames(WD_rand) <- "WD_rand[]"

w <- as.data.frame(rep(1,times=nrow(REKN_final)))
colnames(w)<-"w[]"

eps <- as.data.frame(rep(1,times=nrow(REKN_final)))
colnames(eps)<-"eps[]"

write.table(NREKN.new,file=paste(pathtofiles,"InitialValuesFiles/NREKN.new_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(NRUTU.new,file=paste(pathtofiles,"InitialValuesFiles/NRUTU.new_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(NSESA.new,file=paste(pathtofiles,"InitialValuesFiles/NSESA.new_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(NSAND.new,file=paste(pathtofiles,"InitialValuesFiles/NSAND.new_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(NDUNL.new,file=paste(pathtofiles,"InitialValuesFiles/NDUNL.new_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)

write.table(seg_rand,file=paste(pathtofiles,"InitialValuesFiles/seg_rand_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(WD_rand,file=paste(pathtofiles,"InitialValuesFiles/WD_rand_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(w,file=paste(pathtofiles,"InitialValuesFiles/w_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)
write.table(eps,file=paste(pathtofiles,"InitialValuesFiles/eps_inits.txt",sep=""),sep="\t",col.names=TRUE,row.names=F)

#BEFORE ANY OF THE FILES CAN BE USED IN JAGS THE WORD "END" HAS TO BE ADDED
#TO THE LAST ROW AND THE FILES SHOULD BE LEFT JUSTIFIED
