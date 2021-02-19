
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

#### Loading the output ####

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
