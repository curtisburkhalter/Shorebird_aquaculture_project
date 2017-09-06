
#call in required libraries
library('jagsUI')   #connects R to JAGS

#set working directory
setwd("~/Personal GHub/Shorebird_1617")
pathtofiles <- "E:/Shorebird Project/ShorebirdModel2017/"

sink(paste(pathtofiles,"ZIPmodel.txt"))
cat("
    model {
    
    ####shorebird model as ZIP
    
    # Priors
    psi~dunif(0,1)
    
    # priors for fixed effect factor (i.e. tide)
    alpha~dnorm(0,0.001)
    beta[1]<-0
    beta[2]~dnorm(0,0.001)
    beta[3]~dnorm(0,0.001)
    beta[4]~dnorm(0,0001)
    beta2~dnorm(0,0.001)
    
    for (i in 1:n) {
    eps[i]~dnorm(0,sigma) #random effect on all observations
    }
    
    tau <-1/(sigma*sigma)
    sigma~dunif(0,10)
    
    # Likelihood
    for (i in 1:n) {
    w[i]~dbern(psi)
    NREKN[i] ~ dpois(lambda[i]) 
    eff.lambda[i]<-w[i]*lambda[i]
    log(lambda[i]) <-alpha+ beta[tide[i]] +beta2*wind[i]+ eps[i]
    
    
    # Fit assessments
    Presi[i] <- (NREKN[i] - lambda[i]) / sqrt(lambda[i]) # Pearson residuals
    NREKN.new[i] ~ dpois(lambda[i])		# Replicate data set
    Presi.new[i] <- (NREKN.new[i] - lambda[i]) / sqrt(lambda[i]) # Pearson residuals 																																	 #replicate data set
    D[i] <- pow(Presi[i], 2)
    D.new[i] <- pow(Presi.new[i], 2)
    
    } #i		
    
    
    # Add up discrepancy measures
    fit <- sum(D[])
    fit.new <- sum(D.new[])
    
    } #close model
    
    #data
    list(n=2225,ndate=23)
    
    #inits
    list(beta=c(NA,1,1,1),sigma=1,psi=1,alpha=1,beta2=1)
    ")  