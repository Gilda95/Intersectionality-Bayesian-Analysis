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

#### Loading useful functions ####

load('Regression_Models/Functions.RData')

# setting colors 
orangepoli <- rgb(1, 0.59, 0)
midbluepoli <- rgb(0.33, 0.47, 0.62)
darkbluepolimi <- rgb(0, 0.18, 0.40)
lightbluepoli <- rgb(0.53, 0.63, 0.73)


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

# save(outputRegress, time.taken, z, z.f, file='Regression_Models/Final_regression/pdwrkSE_output.Rdata')

load('Regression_Models/Final_regression/pdwrkSE_output.Rdata')


#### Export the results ####
data.out <- as.matrix(outputRegress) # trasform the mcmc.list into a matrix
data.out <- data.frame(data.out)
n.chain <- dim(data.out)[1] 

names <- c(colnames(z), paste('female', colnames(z.f)))

# Export beta, theta
beta  <- data.out[, grep('beta',  colnames(data.out), value = FALSE)]
theta <- data.out[, grep('theta', colnames(data.out), value = FALSE)]

param <- cbind(beta, theta)
n.param  = dim(param)[2]

#### Plotting the results ####
# Traceplots
par(mfrow = c(3,2))
for (i in 1:dim(param)[2]){
  plot(ts(param[,i]), lwd = 3, main = paste('Traceplot - ', names[i]))
}

# Autocorrelation plots
par(mfrow = c(3,2))
for (i in 1:dim(param)[2]){
  acf(param[,i], lwd=3, col="red3", main = paste('Autocorrelation -', names[i]))
}

# Marginal posterior probabilities
par(mfrow = c(3,2))
for (i in 1:n.param){
  plot(density(param[,i], adj=2), lwd=3, main = paste('Posterior -', names[i]))
  abline(v = quantile(param[,i], prob = c(0.025, 0.975)), col = 'blue', lwd = 2)
  abline(v = 0, col = 'red', lwd = 2)
}

# quant <- NULL
# for (i in 1:n.param){
#   quant[i] <- sum(param[,i]<=0)/length(param[,i])
#   print(paste('Posterior -', names[i], ':', quant[i]))
# }


#### Caterpillar plot ####

colo <- c(rep('red', times = 4), 'grey' , rep('red', times = 2) ,              #colori beta
          rep('blue', times = 7),                                              #colori theta
          'grey', rep('orange', times = 2) ,rep('grey', times = 3), 'orange')  #colori beta.p

lab.beta <- names[1:6]
lab.theta <- names[7:13]

lab.beta <- c('(intercept)', 'age', 'children', 
              'education 2', 'education 3', 'partner')
lab.theta <- c('female: (intercept)', 'female: children', 
               'female: ethnic minorities', 'female: immigrant', 
               'female: partner', 'female: education 2', 'female: education 3')

lab <- c(lab.beta, lab.theta)
par(mfrow = c(1,1))
a <- 1

# all the parameters
MCMCplot(outputRegress,
         params = c('beta', 'theta'), 
         #col = col.theta,
         labels = lab, 
         guide_lines = T, 
         xlab = '',
         xlim = c(-a,a))

# beta parameter 
col.beta <- c('blue', 'gray60', 'blue', 'blue', 'blue', 'blue')
MCMCplot(outputRegress,
         params = 'beta', 
         col = col.beta,
         labels = lab.beta, 
         guide_lines = T,
         xlab = '',
         xlim = c(-a,a))

# theta parameter
col.theta <- c('red', 'red', 'gray60', 'red', 'red', 'red', 'red')
MCMCplot(outputRegress,
         params = 'theta', 
         col = col.theta,
         labels = lab.theta,
         xlab = '',
         xlim = c(-a,a),
         guide_lines = T)


#### Computing PCP ####

dataset <- cbind(z,z.f)
threshold = 0.7
pcp <- compute_pcp_Jackman(dataset, param, y, threshold)

hist(pcp, 
     col = orangepoli,
     main = 'Percent Correctly Predicted',
     cex.axis = 2, 
     cex.main = 2,
     xlab = '', ylab = '', 
     bty = 'n', 
     yaxt = 'n', 
     breaks = 10)

