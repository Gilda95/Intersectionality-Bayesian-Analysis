# - - - - - GENERALIZED LINEAR MODEL FOR PDWRK (JAGS) - - - - - #


#### Loading packages ####

library(coda)            # analyze the MCMC after the simulation
library(rjags)
library(plotrix)         # CIs plot
library(parallel)
library(doParallel)
library(dclone)
library(snow)
library(snowfall)
library(loo)
library(ROCR)           # pcp computation
library(MCMCvis)        # caterpillar plot (MCMCplot)


#### Organize dataset ####

# merging natives and autochtonous
work$ctzmod[work$ctzmod=='native'] <- 'autochthonous'
work$ctzmod <- droplevels(work$ctzmod)

# Columns useless...
work$chld3 <- NULL
work$chld10 <- NULL
work$domicil <- NULL
work$edumod <- NULL
work$edupmod <- NULL

#### Convert to factor ####
# factor... 
work$gndr <- as.factor(work$gndr)
work$cntry <- as.factor(work$cntry)
work$ctzmod <- as.factor(work$ctzmod)
work$brnmod <- as.factor(work$brnmod)
work$prtnr <- as.factor(work$prtnr)
work$isco08p <- as.factor(work$isco08p)
work$rgn <- as.factor(work$rgn)
work$edutre <- as.factor(work$edutre)


# choose first factor 
work$gndr <- relevel(work$gndr, 'male')
work$ctzmod <- relevel(work$ctzmod, 'autochthonous') 
work$brnmod <- relevel(work$brnmod, 'Rich')
work$prtnr <- relevel(work$prtnr, '0')
work$isco08p <- relevel(work$isco08p, '6666666666')
work$edutre <- relevel(work$edutre, '1')


#### Selecting a region ####

# data <- work[work$rgn == "North Europe",]
data <- work[work$rgn == "South Europe",]
# data <- work[work$rgn == "West Europe",]
# data <-  work[work$rgn == "East Europe",]
# data <- work[work$rgn == "Balkans",]


#### Define auxiliary variables ####

# dummy for female 
female  <- ifelse(data$gndr == 'female', 1, 0)
# dummy for partner
partner <- ifelse(data$prtnr == '1', 1, 0)


#### Model matrix ####
y <- data$pdwrk # response
z <- data[, c('agea', 'chld14', 'edutre', 'prtnr')] # beta
z.f <- data[, c('chld14', 'ctzmod', 'prtnr', 'edutre')] # theta (female)

## standardize continuous variables
# z$agea <- scale(z$agea)
# z$chld14 <- scale(z$chld14)

# Model.matrix (convert in matrix, add dummy and eventually intercept)
z    <- model.matrix(y ~ . , data = z)
z.f  <- model.matrix(y ~ . , data = z.f)

# Store the number of covariates in memory
n.z   <- dim(z)[2]
n.z.f <- dim(z.f)[2]


#### Fit the Bayesian model ####

data <- list(y = y, 
             z = z, z.f = z.f, 
             female = female, 
             n = length(y), 
             n.z = n.z, n.z.f = n.z.f)

n.adapt <- 5000
n.chains <- 4
n.burnin <- 7000
n.iter <- 7000
thin <- 10
variable.names <- c("beta", "theta", "loglik") # parameters that will have their values recorded

# parallel computation
timer <- proc.time()
n_cores <- n.chains
cl <- makePSOCKcluster(n_cores)

temp <- clusterEvalQ(cl,library(dclone))
parLoadModule(cl,"glm")
parLoadModule(cl,"lecuyer")
parLoadModule(cl,"dic")
outputRegress <- jags.parfit(cl = cl, data = data, params = variable.names, 
                             model = "Regression_Models/Final_regression/pdwrkSE_model.txt",
                             n.chains = n.chains, n.adapt = n.adapt, n.update = n.burnin, 
                             n.iter = n.iter , thin = thin)
stopCluster(cl)
time.taken <- proc.time() - timer

save(outputRegress, time.taken, z, z.f, file='Regression_Models/Final_regression/pdwrkSE_output.Rdata')

