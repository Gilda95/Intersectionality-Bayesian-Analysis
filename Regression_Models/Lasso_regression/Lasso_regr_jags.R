# - - - - - GENERALIZED LINEAR MODEL FOR PDWRK LASSO REGR (JAGS) - - - - - #

#### loading libraries ####
library(coda)            # analyze the MCMC after the simulation
library(rjags)
library(plotrix)         
library(parallel)
library(doParallel)
library(dclone)
library(snow)
library(snowfall)
library(loo)
library(ROCR)           # it needs for PCP
library(MCMCvis)        # caterpillar plot (MCMCplot)
#### loading useful functions ####

load('Regression_Models/Functions.Rdata')

#### loading the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)


#### organize dataset ####

# merging native and autochthonous
work$ctzmod[work$ctzmod=='native'] <- 'autochthonous'
work$ctzmod <- droplevels(work$ctzmod)

# Columns useless...
work$chld3 <- NULL
work$chld10 <- NULL
work$domicil <- NULL
work$edumod <- NULL
work$edupmod <- NULL

#### convert to factor ####

work$gndr <- as.factor(work$gndr)
work$cntry <- as.factor(work$cntry)
work$ctzmod <- as.factor(work$ctzmod)
work$brnmod <- as.factor(work$brnmod)
work$prtnr <- as.factor(work$prtnr)
#work$pdwrkp <- as.factor(work$pdwrkp)
work$isco08p <- as.factor(work$isco08p)
work$rgn <- as.factor(work$rgn)
work$edutre <- as.factor(work$edutre)

#### choose first factor ####
work$gndr <- relevel(work$gndr, 'male')
work$ctzmod <- relevel(work$ctzmod, 'autochthonous') 
work$brnmod <- relevel(work$brnmod, 'Rich')
work$prtnr <- relevel(work$prtnr, '0')
work$isco08p <- relevel(work$isco08p, '6666666666')
work$edutre <- relevel(work$edutre, '1')

#### region selection ####
# data <- work[work$rgn == "North Europe",]
data <- work[work$rgn == "South Europe",]
# data <- work[work$rgn == "West Europe",]
# data <-  work[work$rgn == "East Europe",]
# data <- work[work$rgn == "Balkans",]


#### define auxiliary variables ####

# response variable
y <- data$pdwrk

# female/male dummy
female <- ifelse (data$gndr == 'female', 1, 0)

# partner dummy
partner <- ifelse(data$prtnr == '1', 1, 0)

# partner working dummy
partner <- ifelse(data$pdwrkp == '1', 1, 0)

#### model matrix ####

y <- data$pdwrk
z <- data[, c("agea", "chld14", "ctzmod", "prtnr", "edutre")]
z.p <- data[, c("eduptre", "pdwrkp", "isco08p")]
z.f <- data[, c("agea", "chld14", "ctzmod", "prtnr", "edutre")]

# standardize continuous variables
z$agea <- scale(z$agea)
z$chld14 <- scale(z$chld14)

z.f$agea <- scale(z.f$agea)
z.f$chld14 <- scale(z.f$chld14)

# Model.matrix (Adding dummies and intercept)
z  <- model.matrix(y ~ . , data = z)
z.f  <- model.matrix(y ~ .-1 , data = z.f)
z.p <- model.matrix(y ~ isco08p - 1 , data = data)


# Store the number of covariates in memory
n.z <- dim(z)[2]
n.z.p <- dim(z.p)[2]
n.z.f <- dim(z.f)[2]

#### fit the Bayesian model ####
data <- list(y = y, 
             z = z, z.p = z.p, z.f = z.f,
             female = female, partner = partner, 
             n = length(y), 
             n.z = n.z, n.z.p = n.z.p, n.z.f = n.z.f)

n.adapt <- 5000
n.chains <- 4
n.burnin <- 7000
n.iter <- 7000
thin <- 10

variable.names <-  c("beta", "theta", 'alpha', "loglik") # parameters that will have their values recorded

# parallel computation
timer <- proc.time()
n_cores <- n.chains
cl <- makePSOCKcluster(n_cores)

temp <- clusterEvalQ(cl,library(dclone))
parLoadModule(cl,"glm")
parLoadModule(cl,"lecuyer")
parLoadModule(cl,"dic")
outputRegress <- jags.parfit(cl = cl, data = data, params = variable.names, model = "pdwrk_model_LASSO.txt",
                             n.chains = n.chains, n.adapt = n.adapt, n.update = n.burnin, n.iter = n.iter , thin = thin)
stopCluster(cl)
time.taken <- proc.time() - timer

save(outputRegress, time.taken, file='Regression_Models/Lasso_regression/pdwrkSE_LASSO_output.Rdata')

load('Regression_Models/Lasso_regression/pdwrkSE_LASSO_output.Rdata')

#### export the results ####

# export the results of the simulation
data.out <- as.matrix(outputRegress) # trasform the mcmc.list into a matrix,
data.out <- data.frame(data.out)
n.chain <- dim(data.out)[1] 

names <- c(colnames(z), paste('female', colnames(z.f)),paste('partner', colnames(z.p)))

# export beta, theta
beta <- data.out[,grep('beta', colnames(data.out), value = FALSE)]
theta <- data.out[,grep('theta', colnames(data.out), value = FALSE)]
alpha <- data.out[,grep('alpha', colnames(data.out), value = FALSE)]

param <- cbind(beta, theta, alpha)
n.param <- dim(param)[2]


#### plots ####

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


#### checking posterior credibility intervals ####

CI_beta = apply(beta, 2, quantile, c(0.025, 0.975)) 
CI_beta
CI_theta = apply(theta, 2, quantile, c(0.025, 0.975)) 
CI_theta
CI_alpha = apply(alpha, 2, quantile, c(0.025, 0.975)) 
CI_alpha

# If the credibility interval does not contain 0 then I keep the variable

# GENERAL
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

# FEMALE
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

# PARTNER
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



#### caterpillar plots ####

 # labels
lab.beta <- names[1:8]
lab.theta <- names[9:16]
lab.alpha <- names[17:22]
  
lab.cat <- c(lab.beta, lab.theta, lab.alpha)

# plots
a <- 1

par(mfrow = c(1,1))
# beta
MCMCplot(outputRegress,
         params = 'beta', 
         #col = col.bt,
         # sz_labels = 1,
         # rank = TRUE, 
         labels = lab.beta,
         #labels = names0[1:16], 
         guide_lines = T,
         xlim = c(-a,a))

# theta
MCMCplot(outputRegress,
         params = 'theta', 
         #col = col.bt,
         # sz_labels = 1,
         # rank = TRUE, 
         labels = lab.theta,
         guide_lines = T,
         xlim = c(-a,a))

# alpha
MCMCplot(outputRegress,
         params = 'alpha', 
         # col = col.bt,
         # rank = TRUE, 
         labels= lab.alpha,
         guide_lines = T, 
         xlim = c(-a,a))

#### computing PCP ####

dataset = cbind(z, z.f, z.p)
pcp <- compute_pcp_Jackman(dataset, param[,1:length(names)], y)


hist(pcp, 
     # col = orangepoli,
     # main = 'Percent Correctly Predicted',
     main = '',
     # cex.axis = 1.25, 
     cex.axis = 2, 
     xlab = '', ylab = '', 
     bty = 'n', 
     yaxt = 'n', 
     breaks = 10)
