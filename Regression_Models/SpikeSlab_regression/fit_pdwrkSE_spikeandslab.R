# - - - - - GENERALIZED LINEAR MODEL FOR PDWRK (JAGS) - - - - - #

                                        
#### Loading packages ####

library(coda)            # analize the MCMC after the simulation
library(rjags)
library(plotrix)         # CIs plot
library(parallel)
library(doParallel)
library(dclone)
library(snow)
library(snowfall)
library(loo)
library(ROCR)            # pcp computation
library(MCMCvis)         # caterpillar plot (MCMCplot)
library(ggplot2)
library(dplyr)

#### Loading the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)

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

# response
y <- data$pdwrk

# dummy for female
female <- ifelse (data$gndr == 'female', 1, 0)

# dummy for partner 
partner <- ifelse(data$prtnr == '1', 1, 0)

# dummy for working partner
partner <- ifelse(data$pdwrkp == '1', 1, 0)

#### Model matrix ####

# defining variables 
y <- data$pdwrk # response
z <- data[, c("agea", "chld14", "ctzmod", "edutre")] # beta
z.p <- data[, c("eduptre", "pdwrkp", "isco08p")] # alpha
z.f <- data[, c("agea", "chld14", "ctzmod", "prtnr", "edutre")] # theta

# standardize continuous variables
z$agea <- scale(z$agea)
z$chld14 <- scale(z$chld14)
z.f$agea <- scale(z.f$agea)
z.f$chld14 <- scale(z.f$chld14)

# model.matrix (convert to matrix, add dummy e add intercept)
z  <- model.matrix(y ~ . , data = z)
z.f  <- model.matrix(y ~ .-1 , data = z.f)
z.p <- model.matrix(y ~ isco08p, data = data)

# store the number of covariates in memory
n.z <- dim(z)[2]
n.z.p <- dim(z.p)[2]
n.z.f <- dim(z.f)[2]

#### Fit the Bayesian model ####
data <- list(y = y, 
             z = z, z.p = z.p, z.f = z.f,
             female = female, partner = partner, 
             n = length(y), 
             n.z = n.z, n.z.p = n.z.p, n.z.f = n.z.f)

n.adapt <- 5000
n.chains <- 4
n.burnin <- 80000
n.iter <- 80000
thin <- 10

# parameters that will have their values recorded
variable.names <-  c("beta", "theta", 'alpha', 
                     "gamma.b", "gamma.t", "gamma.a", 
                     "loglik")

# parallel computing
timer <- proc.time()
n_cores <- n.chains
cl <- makePSOCKcluster(n_cores)

temp <- clusterEvalQ(cl,library(dclone))
parLoadModule(cl,"glm")
parLoadModule(cl,"lecuyer")
parLoadModule(cl,"dic")
outputRegress <- jags.parfit(cl = cl, data = data, params = variable.names, 
                             model = "Regression_Models/SpikeSlab_regression/pdwrkSE_spikeandslab_model.txt",
                             n.chains = n.chains, n.adapt = n.adapt, n.update = n.burnin,
                             n.iter = n.iter , thin = thin)
stopCluster(cl)
time.taken <- proc.time() - timer

save(outputRegress, time.taken, z, z.f, z.p, file='Regression_Models/SpikeSlab_regression/pdwrkSE_spikeandslab_output.Rdata')





