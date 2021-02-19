#### Loading packages ####

library(coda)           # analyze the MCMC after the simulation
library(rjags)
library(plotrix)        # CIs plot 
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

#### Loading the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)

#### Loading the output ####

load('Regression_Models/Lasso_regression/pdwrkSE_lasso_output.Rdata')


#### Export the results ####

# export the results of the simulation
data.out <- as.matrix(outputRegress) # trasform the mcmc.list into a matrix
data.out <- data.frame(data.out)
n.chain <- dim(data.out)[1] 

names <- c(colnames(z), paste('female', colnames(z.f)),paste('partner', colnames(z.p)))

# export beta, theta
beta  <- data.out[, grep('beta',  colnames(data.out), value = FALSE)]
theta <- data.out[, grep('theta', colnames(data.out), value = FALSE)]
alpha <- data.out[, grep('alpha', colnames(data.out), value = FALSE)]

param <- cbind(beta, theta, alpha)
n.param <- dim(param)[2]


#### Plotting the results ####

# traceplots
par(mfrow = c(3,2))
for (i in 1:dim(param)[2]){
  plot(ts(param[,i]), lwd = 3, main = paste('Traceplot - ', names[i]))
}

# autocorrelation plots
par(mfrow = c(3,2))
for (i in 1:dim(param)[2]){
  acf(param[,i], lwd=3, col="red3", main = paste('Autocorrelation -', names[i]))
}

# marginal posterior probabilities
par(mfrow = c(3,2))
for (i in 1:n.param){
  plot(density(param[,i], adj=2), lwd=3, main = paste('Posterior -', names[i]))
  abline(v = quantile(param[,i], prob = c(0.025, 0.975)), col = 'blue', lwd = 2)
  abline(v = 0, col = 'red', lwd = 2)
}


#### Checking posterior credibility intervals ####

CI_beta = apply(beta, 2, quantile, c(0.025, 0.975)) 
CI_beta
CI_theta = apply(theta, 2, quantile, c(0.025, 0.975)) 
CI_theta
CI_alpha = apply(alpha, 2, quantile, c(0.025, 0.975)) 
CI_alpha

# If the credibility interval does not contain 0 then I keep the variable

# overall population - beta
pippo = NULL
for(l in 1:dim(beta)[2]){
  if(CI_beta[1,l]<0 && CI_beta[2,l]>0){
    cat("*** GENERAL: ", colnames(z)[l], " excluded \n")
  }
  else{
    cat("*** GENERAL: ", colnames(z)[l], " included \n")
    pippo = c(pippo, l)
  }
  
}

# female - theta
pluto = NULL
for(l in 1:dim(theta)[2]){
  if(CI_theta[1,l]<0 && CI_theta[2,l]>0){
    cat("*** FEMALE: ", colnames(z.f)[l], " excluded \n")
  }
  else{
    cat("*** FEMALE: ", colnames(z.f)[l], " included \n")
    pluto = c(pluto, l)
  }
}

# partner - alpha
paperino = NULL
for(l in 1:dim(alpha)[2]){
  if(CI_alpha[1,l]<0 && CI_alpha[2,l]>0){
    cat("*** PARTNER: ", colnames(z.p)[l], " excluded \n")
  }
  else{
    cat("*** PARTNER: ", colnames(z.p)[l], " included \n")
    paperino = c(paperino, l)
  }
}


#### Caterpillar plots ####

# labels
lab.beta <- names[1:7]
lab.theta <- names[8:15]
lab.alpha <- names[16:21]

lab.beta <- c('(intercept)', 'age', 'children', 'ethnic minorities', 
              'immigrant', 'education 2', 'education 3')
lab.theta <- c('female: age', 'female: children', 'female: autochthonous', 
               'female: ethnic minorities', 'female: immigrant', 'female: partner',
               'female: education 2', 'female: education 3')
lab.alpha <- c('partner: (intercept)', 'partner: elementary occupations', 
               'partner: manager', 'partner: professionals and employees', 
               'partner: services and sales workers', 'partner: skilled manual workers')
  
lab.cat <- c(lab.beta, lab.theta, lab.alpha)

par(mfrow = c(1,1))
a <- 1

# beta parameter
col.beta <- c('dodgerblue3', 'dodgerblue3', 'dodgerblue3', 
              'dodgerblue3', 'gray60', 'dodgerblue3', 'dodgerblue3')
MCMCplot(outputRegress,
         params = 'beta', 
         col = col.beta,
         xlab = 'beta estimate',
         labels = lab.beta,
         guide_lines = T,
         xlim = c(-a,a))

# theta parameter
col.theta <- c('red', 'red', 'red', 'gray60', 
               'red', 'red', 'red', 'red')
MCMCplot(outputRegress,
         params = 'theta', 
         col = col.theta,
         xlab = 'theta estimate',
         labels = lab.theta,
         guide_lines = T,
         xlim = c(-a,a))

# alpha parameter
col.alpha <- c('forestgreen', 'forestgreen', 'gray60', 
               'forestgreen', 'forestgreen', 'gray60')
MCMCplot(outputRegress,
         params = 'alpha', 
         col = col.alpha,
         xlab = 'alpha estimate',
         labels= lab.alpha,
         guide_lines = T, 
         xlim = c(-a,a))


#### Computing PCP ####

dataset = cbind(z, z.f, z.p)
pcp <- compute_pcp_Jackman(dataset, param[,1:length(names)], y, threshold=0.7)

hist(pcp, 
     col = 'orange',
     main = 'Percent Correctly Predicted',
     cex.axis = 2, 
     xlab = '', ylab = '', 
     bty = 'n', 
     yaxt = 'n', 
     breaks = 10)
