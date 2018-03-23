#Shorebird-Aquaculture Model written for BUGS modeling language
#code written by Curtis Burkhalter


#call in required libraries
suppressMessages(library('janitor'))  #package required to clean up column names automatically
suppressMessages(library('arm'))      #package written by A. Gelman that will automatically
                                      #standardize all data by dividing by 2sd
suppressMessages(library('tidyverse'))
suppressMessages(library('lme4'))
suppressMessages(library('lubridate'))
suppressMessages(library('here'))


#read in raw census data file
RawCensus <- read_csv(here("Shorebird_aquaculture_project","Census2016-17_March2018.csv")) %>%
  clean_names()

View(RawCensus)

#need to remove columns 47, 48
RawCensus <- RawCensus[,1:46]

#change column names to format that I like
names(RawCensus) <- gsub("shoreline.*","shore_hab",names(RawCensus))
names(RawCensus) <- gsub("date.*","date",names(RawCensus))
names(RawCensus) <- gsub("time.*","time",names(RawCensus))
names(RawCensus) <- gsub("tide.*","tide",names(RawCensus))
names(RawCensus) <- gsub("*.flock_edge_location","FEL",names(RawCensus))
names(RawCensus) <- gsub("*.flock_edge_distance_to_racks","FEDR",names(RawCensus))
names(RawCensus) <- gsub("*.flock_edge_distance_to_tending","FEDT",names(RawCensus))
names(RawCensus) <- gsub(".*Window","TW",names(RawCensus))
names(RawCensus) <- gsub("total_number_shorebirds_in.*","TS",names(RawCensus))
names(RawCensus) <- gsub("*.in_segment","IS",names(RawCensus))
names(RawCensus) <- gsub("nearest_rack.*","tending",names(RawCensus))
names(RawCensus) <- gsub("number_planes.*","plane",names(RawCensus))
names(RawCensus) <- gsub("seg_int","segment",names(RawCensus))
names(RawCensus) <- gsub("number_gulls","nGulls",names(RawCensus))
names(RawCensus) <- gsub("number_segments.*","dist_from_AQ",names(RawCensus))

#change the date column to a true date using lubridate::mdy
RawCensus$date <- mdy(RawCensus$date)

#subset records where 'TS' is not an NA, there appears to be a lot of
#issues with those records
RawCensus <- RawCensus[complete.cases(RawCensus$TS),] 

#there are records where SESA are and NA and where RUTU/REKN are negative
#need to remove those records later after the data file
#has been split up into species specific dataframes;
#for now just change the negative RUTU/REKN count records to NA
#remove records that contain only a blank space or other character entries
RawCensus$total_reknIS[RawCensus$total_reknIS < 0] <- NA
RawCensus$total_rutuIS[RawCensus$total_rutuIS < 0] <- NA

#in the plane column there are 2 entries that are NA, just remove them
RawCensus <- RawCensus[complete.cases(RawCensus$plane),]

#remove column 1
RawCensus <- RawCensus[,-1]

#convert certain character vectors to factors
RawCensus$segment <- as.factor(RawCensus$segment)
RawCensus$shore_hab <- as.factor(RawCensus$shore_hab)
RawCensus$habitat <- as.factor(RawCensus$habitat)
RawCensus$tide <- as.factor(RawCensus$tide)

#drop column 'observationoccuredduring4hrtendingwindow_1_0'
RawCensus <- RawCensus[, -grep("observation_occured.*", colnames(RawCensus))]

#records have already been subsetted to include only those in 
#tending window, so now I'm going to subset only the
#columns I need for the occupancy modeling and put it in Only Tending
OnlyTending<- RawCensus[,grep("segment.*|habitat.*|shore.*|dist.*|date.*|year.*|time.*|TS.*|.*_occupancy|nGulls.*|tending.*|plane.*|tide.*", colnames(RawCensus))]

View(OnlyTending)

#convert the date to a numeric day from 1-365
OnlyTending$day <- strptime(OnlyTending$date, format = "%Y-%m-%d",tz = "")$yday+1

#write function that converts data structures from factors to a numeric sequence
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

##convert shore_hab into a numeric id
#Bulkhead=1;Dune=3,Phragmites=5;Marsh=4;Creek=2;Woodland=6
OnlyTending$NSH <- as.numeric.factor(OnlyTending$shore_hab)

#remove all white space from tide column and convert to numeric
#Falling=1;Low=2,Rising=3
OnlyTending$tide <- as.factor(gsub("\\s", "", OnlyTending$tide)) 
OnlyTending$Ntide <- as.numeric.factor(OnlyTending$tide)

#drop non-numeric categorical variable columns
OnlyTending$shore_hab <- OnlyTending$tide  <- NULL

fit_set <- OnlyTending

#remove OnlyTending and RawCensus from memory
OnlyTending <- RawCensus <- NULL

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

#create species specific datasets from fit_set
REKN <- fit_set[, -grep("rutu.*|sesa.*|sand.*|dunl.*", colnames(fit_set))]
RUTU <- fit_set[, -grep("rekn.*|sesa.*|sand.*|dunl.*", colnames(fit_set))]
SESA <- fit_set[, -grep("rutu.*|rekn.*|sand.*|dunl.*", colnames(fit_set))]
SAND <- fit_set[, -grep("rutu.*|sesa.*|rekn.*|dunl.*", colnames(fit_set))]
DUNL <- fit_set[, -grep("rutu.*|sesa.*|sand.*|rekn.*", colnames(fit_set))]

#Need to rescale the covariates prior to modelling
#the rescaling leaves the binary variables, including dummies,
#as 0/1 and divides the continuous variables by 2sd, exclude random effect variables
#the only continuous variables int the data set are 'dist_from_AQ',
#'TS', 'nGulls','plane'

REKN_covarSTD <- apply(REKN[,grep("dist_from_AQ|TS|nGulls|plane",colnames(REKN))],2,rescale, binary.inputs = "0/1")
RUTU_covarSTD <- apply(RUTU[,grep("dist_from_AQ|TS|nGulls|plane",colnames(RUTU))],2,rescale, binary.inputs = "0/1")
SESA_covarSTD <- apply(SESA[,grep("dist_from_AQ|TS|nGulls|plane",colnames(SESA))],2,rescale, binary.inputs = "0/1")
SAND_covarSTD <- apply(SAND[,grep("dist_from_AQ|TS|nGulls|plane",colnames(SAND))],2,rescale, binary.inputs = "0/1")
DUNL_covarSTD <- apply(DUNL[,grep("dist_from_AQ|TS|nGulls|plane",colnames(DUNL))],2,rescale, binary.inputs = "0/1")

REKN_covarSTD <- cbind(REKN_covarSTD,REKN[,-grep("dist_from_AQ|TS|nGulls|plane",colnames(REKN))])
RUTU_covarSTD <- cbind(RUTU_covarSTD,RUTU[,-grep("dist_from_AQ|TS|nGulls|plane",colnames(RUTU))])
SESA_covarSTD <- cbind(SESA_covarSTD,SESA[,-grep("dist_from_AQ|TS|nGulls|plane",colnames(SESA))])
SAND_covarSTD <- cbind(SAND_covarSTD,SAND[,-grep("dist_from_AQ|TS|nGulls|plane",colnames(SAND))])
DUNL_covarSTD <- cbind(DUNL_covarSTD,DUNL[,-grep("dist_from_AQ|TS|nGulls|plane",colnames(DUNL))])

#for modeling in JAGS the binary variables cannot take on a
#value of 0 so change all 0 to 1 and all 1 to 2.
#write function that checks whether a column is binary
#specifically the functions says 'are all values in 'x' within the sequence 0 to 1
binary_check <- function(x) { all(x %in% 0:1) }

#apply the binary_check function and subset the column names of a species
#standardized covariate dataframe if they are binary
REKN_Vars <- colnames(REKN_covarSTD)[apply(REKN_covarSTD,2,binary_check)]

#function works, but need to remove the response variable 'spp_occupancy'
#from the variable set
REKN_Vars <- REKN_Vars[-grep("rekn_occupancy",REKN_Vars)]

#Using only the subsetted columns names for those binary variables now
#change the 0's to 1's and 1's to 2's
REKN_covarSTD[,REKN_Vars] <- lapply(REKN_covarSTD[,REKN_Vars],function(x) ifelse(x==1,2,1))

#repeat for the other 4 species
RUTU_Vars <- colnames(RUTU_covarSTD)[apply(RUTU_covarSTD,2,binary_check)]
RUTU_Vars <- RUTU_Vars[-grep("rutu_occupancy",RUTU_Vars)]
RUTU_covarSTD[,RUTU_Vars] <- lapply(RUTU_covarSTD[,RUTU_Vars],function(x) ifelse(x==1,2,1))

SESA_Vars <- colnames(SESA_covarSTD)[apply(SESA_covarSTD,2,binary_check)]
SESA_Vars <- SESA_Vars[-grep("sesa_occupancy",SESA_Vars)]
SESA_covarSTD[,SESA_Vars] <- lapply(SESA_covarSTD[,SESA_Vars],function(x) ifelse(x==1,2,1))

SAND_Vars <- colnames(SAND_covarSTD)[apply(SAND_covarSTD,2,binary_check)]
SAND_Vars <- SAND_Vars[-grep("sand_occupancy",SAND_Vars)]
SAND_covarSTD[,SAND_Vars] <- lapply(SAND_covarSTD[,SAND_Vars],function(x) ifelse(x==1,2,1))

DUNL_Vars <- colnames(DUNL_covarSTD)[apply(DUNL_covarSTD,2,binary_check)]
DUNL_Vars <- DUNL_Vars[-grep("dunl_occupancy",DUNL_Vars)]
DUNL_covarSTD[,DUNL_Vars] <- lapply(DUNL_covarSTD[,DUNL_Vars],function(x) ifelse(x==1,2,1))

#set the final species specific dataframes
REKN_final <- REKN_covarSTD
RUTU_final <- RUTU_covarSTD
SESA_final <- SESA_covarSTD
SAND_final <- SAND_covarSTD
DUNL_final <- DUNL_covarSTD

#write out the cleaned data sets that will
#be used for fitting the models
write_tsv(REKN_final,path=here("Shorebird_aquaculture_project","DataForModelFitting","REKN_fittingPA031918.txt"),col_names=TRUE)
write_tsv(RUTU_final,path=here("Shorebird_aquaculture_project","DataForModelFitting","RUTU_fittingPA031918.txt"),col_names=TRUE)
write_tsv(SESA_final,path=here("Shorebird_aquaculture_project","DataForModelFitting","SESA_fittingPA031918.txt"),col_names=TRUE)
write_tsv(SAND_final,path=here("Shorebird_aquaculture_project","DataForModelFitting","SAND_fittingPA031918.txt"),col_names=TRUE)
write_tsv(DUNL_final,path=here("Shorebird_aquaculture_project","DataForModelFitting","DUNL_fittingPA031918.txt"),col_names=TRUE)

#write up some initial values for fit statistic "NREKN.new","seg_rand","WD_rand"
NREKN.newPA <- as.data.frame(rep(1,times=nrow(REKN_final)))
colnames(NREKN.newPA)<-"NREKN.newPA[]"

NRUTU.newPA <- as.data.frame(rep(1,times=nrow(RUTU_final)))
colnames(NRUTU.newPA)<-"NRUTU.newPA[]"

NSESA.newPA <- as.data.frame(rep(1,times=nrow(SESA_final)))
colnames(NSESA.newPA)<-"NSESA.newPA[]"

NSAND.newPA <- as.data.frame(rep(1,times=nrow(SAND_final)))
colnames(NSAND.newPA)<-"NSAND.newPA[]"

NDUNL.newPA <- as.data.frame(rep(1,times=nrow(DUNL_final)))
colnames(NDUNL.newPA)<-"NDUNL.newPA[]"

seg_randPA <- as.data.frame(as.integer(rep(1,times=length(unique(REKN_final$segment)))))
colnames(seg_randPA) <- "seg_randPA[]"

wPA <- as.data.frame(rep(1,times=nrow(REKN_final)))
colnames(wPA)<-"wPA[]"

epsPA <- as.data.frame(rep(1,times=nrow(REKN_final)))
colnames(epsPA)<-"epsPA[]"

write_tsv(NREKN.newPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","NREKN.newPA_inits.txt"),col_names=TRUE)
write_tsv(NRUTU.newPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","NRUTU.newPA_inits.txt"),col_names=TRUE)
write_tsv(NSESA.newPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","NSESA.newPA_inits.txt"),col_names=TRUE)
write_tsv(NSAND.newPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","NSAND.newPA_inits.txt"),col_names=TRUE)
write_tsv(NDUNL.newPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","NDUNL.newPA_inits.txt"),col_names=TRUE)

write_tsv(seg_randPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","seg_randPA.txt"),col_names=TRUE)
write_tsv(wPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","wPA.txt"),col_names=TRUE)
write_tsv(epsPA,path=here("Shorebird_aquaculture_project","InitialValueFiles","epsPA.txt"),col_names=TRUE)

#BEFORE ANY OF THE FILES CAN BE USED IN JAGS THE WORD "END" HAS TO BE ADDED
#TO THE LAST ROW AND THE FILES SHOULD BE LEFT JUSTIFIED
