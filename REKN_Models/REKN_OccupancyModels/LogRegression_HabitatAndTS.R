#call in required libraries
suppressMessages(library(rjags))
suppressMessages(library(jagsUI))
suppressMessages(library(here))
suppressMessages(library(tidyverse))

#read in data and initial value files; using "../" moves you up one directory
seg_rand <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","seg_randPA.txt"),header=TRUE)
names(seg_rand)<-NULL

NREKN.new <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","NREKN.newPA_inits.txt"),header=TRUE)
names(NREKN.new) <- NULL
NREKN.new <- as.integer(NREKN.new[,1])

eps <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","epsPA.txt"),header=TRUE)
names(eps) <- NULL
eps <- as.integer(eps[,1])

comdata <- read.delim(file=here("Shorebird_aquaculture_project","DataForModelFitting","REKN_fittingPA032118.txt"),header=T)
comdata$time <- NULL
comdata$habitat <- as.integer(comdata$habitat)

#put the column names for the count and covariate data into a vector
comdata_names <- names(comdata) 

#number of observations
N <- as.numeric(nrow(comdata))

#number of unique segments; use minus 1 b/c the last row is "END"
segment <- as.integer(nrow(seg_rand)) 

#initial values for parameters
Beta_habitat=c(NA,1,1)
Beta_TS=1
alpha.lam=1
tau.disp=3

#JAGS only accepts initial values put into a list or a function
initsFunction = function() {list(Beta_habitat=Beta_habitat,Beta_TS=Beta_TS,
                                 alpha.lam=alpha.lam,tau.disp=tau.disp,eps=eps)}

#z is a temporary list; each list element contains a single data column from comdata
z<-tapply(as.list(comdata), gl(ncol(comdata),1), as.data.frame)

#Set the list element names in z using the unique data names
names(z) <- comdata_names

#this separates each unique data set into a dataframe
for(i in comdata_names) {
  assign(i, z[[i]])
}

#package the data to be used in JAGS by providing the dataset names
data <- list('N','rekn_occupancy','year','segment','TS','nGulls',
             'tending','plane','day','Bulk','Dune','Phrag',
             'Marsh','Creek','Woodland','FT','LT','RT', 'dist_from_AQ','habitat')

#tells JAGS which parameters to monitor
params<-c("Beta_habitat","Beta_TS",
          "alpha.lam","fit","fit.new","tau.disp","bpvalue")

start <- Sys.time()

#write out model
writeLines("
           
           model {											#Open overall model bracket
           
           ############################################################
           #Priors
           ############################################################
           
          
           
           alpha.lam~dnorm(0,0.1)
           
           Beta_habitat[1]<-0
           Beta_habitat[2]~dnorm(0,0.1)
           Beta_habitat[3]~dnorm(0,0.1)
           Beta_TS~dnorm(0,0.1)

           # i obs random effect
           for (i in 1:N) {
           eps[i]~dnorm(0,tau.disp)#I(-20,20)  #random observation effect
           }
           
           #hyperprior on random observation effects precision
           tau.disp ~ dgamma(0.1,0.001)
           
           
           ############################################################
           #Likelihood specification
           ############################################################
           
           for (i in 1:N){											# Open i likelihood bracket; corresponds to obs
           #n observations
           
           rekn_occupancy[i] ~ dbern(p.occ[i]) 
           
           logit(p.occ[i]) <- alpha.lam + Beta_habitat[habitat[i]] + Beta_TS*TS[i] + eps[i]
           
           # Fit assessments
           
           residual[i] <- abs(rekn_occupancy[i] - p.occ[i])

           
           # Generate replicate datasets
           NREKN.new[i] ~ dbern(p.occ[i])
           residual.new[i] <- abs(NREKN.new[i]-p.occ[i])
           
           
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
           
           
           
           
           ", con = here("Shorebird_aquaculture_project","REKN_Models", "LogRegression_HabitatAndTSModel.txt"))

#Identify filepath of model file;
modfile <- here("Shorebird_aquaculture_project","REKN_Models", "LogRegression_HabitatAndTSModel.txt")

#create JAGS model object 'out' using the jags function of package jagsUI             
out <- jags(data = data,
            parameters.to.save = params,
            inits = initsFunction,
            model.file = modfile,
            modules=c('glm','dic'),
            n.chains = 2,
            n.adapt = 100,
            n.iter = 40000,
            n.burnin = 10000,
            n.thin = 2,
            parallel=TRUE,
            seed=as.integer(Sys.time()),
            n.cores=2)

sink(file=here("Shorebird_aquaculture_project","OutputFiles","REKN","outputLogRegression_HabitatAndTSModel.txt"))
out <- update(out,n.iter = 60000)
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

