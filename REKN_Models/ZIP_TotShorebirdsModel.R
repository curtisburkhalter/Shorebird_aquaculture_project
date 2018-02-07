#call in required libraries
suppressMessages(library(rjags))
suppressMessages(library(jagsUI))
suppressMessages(library(here))
suppressMessages(library(tidyverse))

#read in data and initial value files; using "../" moves you up one directory
seg_rand <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","seg_rand_inits.txt"),header=TRUE)
names(seg_rand)<-NULL

WD_rand <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","WD_rand_inits.txt"),header=TRUE)
names(WD_rand)<-NULL

NREKN.new <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","NREKN.new_inits.txt"),header=TRUE)
names(NREKN.new) <- NULL
NREKN.new <- as.integer(NREKN.new[,1])
NREKN.new <- NREKN.new[1:6935]

w <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","w_inits.txt"),header=TRUE)
names(w) <- NULL
w <- as.integer(w[,1])

eps <- read.delim(file=here("Shorebird_aquaculture_project","InitialValueFiles","eps_inits.txt"),header=TRUE)
names(eps) <- NULL
eps <- as.integer(eps[,1])

comdata <- read.delim(file=here("Shorebird_aquaculture_project","DataForModelFitting","REKN_fitting012818.txt"),header=T)
#put the column names for the count and covariate data into a vector
comdata_names <- names(comdata) 

comdata$FEDR <- ifelse(comdata$FEDR == 0, "0to50", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == 1, "50to100", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == 2, "100to150", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == 3, "150to200", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == 5, "250to300", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == 6, "NoDist", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == 7, "300+", comdata$FEDR)
comdata$FEDR <- as.factor(comdata$FEDR)

comdata$FEDR <- ifelse(comdata$FEDR == "0to50", "1", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == "50to100", "2", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == "100to150", "3", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == "150to200", "4", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == "250to300", "5", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == "NoDist", "6", comdata$FEDR)
comdata$FEDR <- ifelse(comdata$FEDR == "300+","7", comdata$FEDR)
comdata$FEDR <- as.integer(comdata$FEDR)

#number of observations
N <- as.numeric(nrow(comdata))

#number of unique segments; use minus 1 b/c the last row is "END"
segment <-nrow(seg_rand) - 1

#number of unique wind directions; use minus 1 b/c the last row is "END"
WD <- nrow(WD_rand) - 1


#initial values for parameters
Beta_TS=1
alpha.lam=1
omega=1
tau.disp=3


#JAGS only accepts initial values put into a list or a function
initsFunction = function() {list(Beta_TS=Beta_TS,
                                 alpha.lam=alpha.lam,omega=omega,tau.disp=tau.disp,eps=eps,w=w)}

#package the data to be used in JAGS by providing the dataset names
data <- list('N','REKN','year','FEDR','Nsegment','NWD','time','AT','windS','TS','nGulls',
             'tending','nOM','nOtherP','dog','raptor','plane','activities','day','Bulk','Dune','Phrag',
             'Marsh','Creek','Woodland','FT','LT','RT','DC1','DC2','DC3','DC4',
             'DC5','DC6','DC7','DC8')

#z is a temporary list; each list element contains a single data column from comdata
z<-tapply(as.list(comdata), gl(ncol(comdata),1), as.data.frame)

#Set the list element names in z using the unique data names
names(z) <- comdata_names

#this separates each unique data set into a dataframe
for(i in comdata_names) {
  assign(i, z[[i]])
}


#tells JAGS which parameters to monitor
params<-c("Beta_TS",
          "alpha.lam","fit","fit.new","omega","tau.disp","bpvalue")

start <- Sys.time()

#write out model
writeLines("
           
           model {											#Open overall model bracket
           
           ############################################################
           #Priors
           ############################################################
           
           omega ~ dunif(0,1)
           
           alpha.lam~dnorm(0,0.1)
           
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
           
           w[i] ~ dbern(omega)
           REKN[i] ~ dpois(eff.lambda[i]) 
           eff.lambda[i] <- w[i]*lambda[i]
           
           log(lambda[i]) <- alpha.lam + Beta_TS*TS[i] +eps[i]
           
           # Fit assessments
           
           residual[i] <- REKN[i] - eff.lambda[i]
           predicted[i] <- eff.lambda[i]
           sq[i] <- pow(residual[i],2)
           
           
           # Generate replicate datasets
           NREKN.new[i] ~ dpois(eff.lambda[i])
           sq.new[i] <- pow(NREKN.new[i]-predicted[i],2)
           
           
           } # close i likelihood bracket
           
           ############################################################
           #Derived quantities
           ############################################################
           # Add up discrepancy measures
           fit <- sum(sq[])
           fit.new <- sum(sq.new[])
           test <- step(fit.new-fit)
           bpvalue <- mean(test)
           } #close model
           
           
           
           
           ", con = here("Shorebird_aquaculture_project","REKN_Models", "ZIP_TotShorebirdsModel.txt"))

#Identify filepath of model file;
modfile <- here("Shorebird_aquaculture_project","REKN_Models", "ZIP_TotShorebirdsModel.txt")

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

sink(file=here("Shorebird_aquaculture_project","OutputFiles","REKN","outputTotShorebirdsModel.txt"))
#out <- update(out,n.iter = 30000)
out
sink()

end <- Sys.time()

elapsed_time <- end - start

#look at posterior predictive checks to determine general model fit;
#want this value to be as close to 0.50 as possible, but any value 0.1<p<0.9 is okay

pp.check(out, 'fit', 'fit.new')

#provides the names of each of the columns in the model object
names(out)

#Plot traces and posterior densities
plot(out)


#update more if necessary
#out <- update(out,n.iter = 30000)

