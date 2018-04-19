#call in required libraries
suppressMessages(library(rjags))
suppressMessages(library(jagsUI))
suppressMessages(library(here))
suppressMessages(library(tidyverse))

#read in data and initial value files; using "../" moves you up one directory
seg_rand <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","seg_randPA.txt"),header=TRUE)
names(seg_rand)<-NULL
seg_rand <- as.integer(seg_rand[,1])

NRUTU.new <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","NRUTU.newPA_inits.txt"),header=TRUE)
names(NRUTU.new) <- NULL
NRUTU.new <- as.integer(NRUTU.new[,1])

eps <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","epsPA.txt"),header=TRUE)
names(eps) <- NULL
eps <- as.integer(eps[,1])

comdata <- read.delim(file=here("Shorebird_aquaculture_project","DataForModelFitting","RUTU_fittingPA032118.txt"),header=T)
comdata$time <- NULL
comdata$habitat <- as.integer(comdata$habitat)

#some of the segment indicators are negative, make positive by adding 12 to all of them
comdata$segment <- comdata$segment+12
comdata$segment[comdata$segment >=87] <- comdata$segment[comdata$segment >=87] - 1

#put the column names for the count and covariate data into a vector
comdata_names <- names(comdata) 

#number of observations
N <- as.numeric(nrow(comdata))

#number of unique segments
Nsegment <- as.integer(length(seg_rand)) 

Nyear <- as.integer(length(unique(comdata$year)))

#initial values for parameters
Beta_tending=c(NA,1)
Beta_nGulls=1
alpha.lam=1
tau.seg=3
year_rand = c(1,1)
tau.year = 3

#JAGS only accepts initial values put into a list or a function
initsFunction = function() {list(Beta_tending=Beta_tending,Beta_nGulls=Beta_nGulls,
                                 alpha.lam=alpha.lam,tau.seg=tau.seg,seg_rand=seg_rand,year_rand=year_rand,tau.year=tau.year)}

#z is a temporary list; each list element contains a single data column from comdata
z<-tapply(as.list(comdata), gl(ncol(comdata),1), as.data.frame)

#Set the list element names in z using the unique data names
names(z) <- comdata_names

#this separates each unique data set into a dataframe
for(i in comdata_names) {
  assign(i, z[[i]])
}

year <- ifelse (year == 2017, 1 , 2)

#package the data to be used in JAGS by providing the dataset names
data <- list('N','rutu_occupancy','year','segment','TS','nGulls',
             'tending','plane','day','Bulk','Dune','Phrag',
             'Marsh','Creek','Woodland','FT','LT','RT', 'dist_from_AQ','habitat','Nsegment','Nyear','NSH')

#tells JAGS which parameters to monitor
params<-c("Beta_tending","Beta_nGulls",
          "alpha.lam","fit","fit.new","tau.seg","bpvalue")

#remove variable "i" from global environment
rm("i")

start <- Sys.time()

#write out model
writeLines("
           
           model {											#Open overall model bracket
           
           ############################################################
           #Priors
           ############################################################
           
           
           
           alpha.lam~dnorm(0,0.1)

           Beta_tending[1]<-0
           Beta_tending[2]~dnorm(0,0.1)
           Beta_nGulls~dnorm(0,0.1)
           
           # j segment random effect
           for (j in 1:Nsegment) {
           seg_rand[j]~dnorm(0,tau.seg) #random segment effect
           }
           
           # y year random effect
           for (y in 1:Nyear) {
           year_rand[y]~dnorm(0,tau.year) #random segment effect
           }
           
           #hyperprior on random segment effects precision
           tau.seg ~ dgamma(0.1,0.001)
           tau.year ~ dgamma(0.1,0.001)
           
           ############################################################
           #Likelihood specification
           ############################################################
           
           for (i in 1:N){											# Open i likelihood bracket; corresponds to obs
           #n observations
           
           rutu_occupancy[i] ~ dbern(p.occ[i]) 
           
           logit(p.occ[i]) <- alpha.lam + Beta_tending[tending[i]] + Beta_nGulls*nGulls[i] + seg_rand[segment[i]] + year_rand[year[i]]
           
           # Fit assessments
           
           residual[i] <- abs(rutu_occupancy[i] - p.occ[i])
           
           
           # Generate replicate datasets
           NRUTU.new[i] ~ dbern(p.occ[i])
           residual.new[i] <- abs(NRUTU.new[i]-p.occ[i])
           
           
           } # close i likelihood bracket
           
           ############################################################
           #Derived quantities
           ############################################################
           # Add up discrepancy measures
           fit <- sum(residual[])
           fit.new <- sum(residual.new[])
           test <- step(fit.new-fit)
           bpvalue <- mean(test)
           } #close model
           
           
           
           
           ", con = here("Shorebird_aquaculture_project","RUTU_Models", "RUTU_OccupancyModels", "LogRegression_TendingandnGullsRandomSegmentandYearModel.txt"))

#Identify filepath of model file;
modfile <- here("Shorebird_aquaculture_project","RUTU_Models", "RUTU_OccupancyModels", "LogRegression_TendingandnGullsRandomSegmentandYearModel.txt")

#create JAGS model object 'out' using the jags function of package jagsUI             
out <- jags(data = data,
            parameters.to.save = params,
            inits = initsFunction,
            model.file = modfile,
            modules=c('glm','dic'),
            n.chains = 2,
            n.adapt = 100,
            n.iter = 100000,
            n.burnin = 10000,
            n.thin = 2,
            parallel=TRUE,
            seed=as.integer(Sys.time()),
            n.cores=2)

sink(file=here("Shorebird_aquaculture_project","OutputFiles","RUTU","outputLogRegression_TendingandnGulls_RandomSegmentandYearModel.txt"))
#out <- update(out,n.iter = 50000)
out
sink()

end <- Sys.time()

elapsed_time <- end - start
elapsed_time

#look at posterior predictive checks to determine general model fit;
#want this value to be as close to 0.50 as possible, but any value 0.1<p<0.9 is okay

pp.check(out, 'fit', 'fit.new')

#provides the names of each of the columns in the model object
names(out)

#Plot traces and posterior densities
plot(out)


#update more if necessary
#out <- update(out,n.iter = 30000)

