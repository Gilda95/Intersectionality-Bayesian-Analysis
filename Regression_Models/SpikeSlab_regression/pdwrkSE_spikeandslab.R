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

#### Loading useful functions ####

load('Regression_Models/Functions.RData')

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
y <- data$pdwrk
z <- data[, c("agea", "chld14", "ctzmod", "edutre")]
z.p <- data[, c("eduptre", "pdwrkp", "isco08p")]
z.f <- data[, c("agea", "chld14", "ctzmod", "prtnr", "edutre")]

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

# save(outputRegress, time.taken, file='Regression_Models/SpikeSlab_regression/pdwrkSE_spikeandslab_output.Rdata')

load('Regression_Models/SpikeSlab_regression/pdwrkSE_spikeandslab_output.Rdata')

#### Export the results ####

# Export the results of the simulation
data.out <- as.matrix(outputRegress) # trasform the mcmc.list into a matrix,
data.out <- data.frame(data.out)
n.chain <- dim(data.out)[1] 

names0 <- c(colnames(z), paste('female', colnames(z.f)), paste('partner', colnames(z.p)))
names <- c(names0, paste('gamma', names0))

# Export beta, theta
alpha <- data.out[, grep('alpha', colnames(data.out), value = FALSE)]
beta  <- data.out[, grep('beta',  colnames(data.out), value = FALSE)]
theta <- data.out[, grep('theta', colnames(data.out), value = FALSE)]

gamma.b <- data.out[, grep('gamma.b', colnames(data.out), value = FALSE)]
gamma.t <- data.out[, grep('gamma.t', colnames(data.out), value = FALSE)]
gamma.a <- data.out[, grep('gamma.a', colnames(data.out), value = FALSE)]

param <- cbind(beta, theta, alpha, gamma.b, gamma.t, gamma.a)
n.param <- dim(param)[2]

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

# Marginal posterior of parameters
par(mfrow = c(3,2))
for (i in 1:n.param){
  plot(density(param[,i], adj=2), lwd=3, main = paste('Posterior -', names[i]))
  abline(v = quantile(param[,i], prob = c(0.025, 0.975)), col = 'blue', lwd = 2)
  abline(v = 0, col = 'red', lwd = 2)
}


#### Posterior Inclusion Probabilities ####

gamma <- cbind(gamma.b, gamma.t, gamma.a)
post.gamma_mean <- apply(gamma,2,"mean")

lab.beta <- names0[1:7]     
lab.theta <- names0[8:15]   
lab.alpha <- names0[16:21]  

lab.beta <- c('(intercept)', 'age', 'children', 'ethnic minorities', 
              'immigrant', 'education 2', 'education 3')
lab.theta <- c('female: age', 'female: children', 'female: autochthonous', 
               'female: ethnic minorities', 'female: immigrant', 'female: partner',
               'female: education 2', 'female: education 3')
lab.alpha <- c('partner: (intercept)', 'partner: elementary occupations', 
               'partner: manager', 'partner: professionals and employees', 
               'partner: services and sales workers', 'partner: skilled manual workers')

lab.gg <- names0

# all parameters
p2 <- data.frame(value = post.gamma_mean, var = lab.gg) %>%
  ggplot(aes(y = value, x = var, fill = var)) +
  geom_bar(stat="identity") +
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="none") +
  ylab("posterior inclusion probabilities") +
  xlab("")
p2

# beta parameters
p.beta <- data.frame(value = apply(gamma.b, 2, mean), var = lab.beta) %>%
  ggplot(aes(y = value, x = var, fill = var)) +
  geom_bar(stat="identity") +
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() +
  ylim(0,1) + 
  theme_minimal() +
  theme(legend.position="none", axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  ylab("posterior inclusion probabilities") +
  ylab('')+
  xlab("")
p.beta

# theta parameters
p.theta <- data.frame(value = apply(gamma.t, 2, mean), var = lab.theta) %>%
  ggplot(aes(y = value, x = var, fill = var)) +
  geom_bar(stat="identity") +
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() +
  ylim(0,1) + 
  theme_minimal() +
  theme(legend.position="none", axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  ylab("posterior inclusion probabilities") +
  ylab('')+
  xlab("")
p.theta

# alpha parameters
p.alpha <- data.frame(value = apply(gamma.a, 2, mean), var = lab.alpha) %>%
  ggplot(aes(y = value, x = var, fill = var)) +
  geom_bar(stat="identity") +
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() +
  ylim(0,1) + 
  theme_minimal() +
  theme(legend.position="none", axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  ylab("posterior inclusion probabilities") +
  ylab('')+
  xlab("")
p.alpha


#### Caterpillar plot ####

lab.beta <- names0[1:7]     
lab.theta <- names0[8:15]   
lab.alpha <- names0[16:21]  

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
              'gray60', 'gray60', 'dodgerblue3', 'dodgerblue3')
MCMCplot(outputRegress,
         params = 'beta', 
         col = col.beta,
         labels = lab.beta,
         xlab = '',
         xlim = c(-a,a),
         guide_lines = T)

# theta parameter
col.theta <- c('gray60', 'red', 'red', 'gray60', 
               'red', 'red', 'gray60', 'red')
MCMCplot(outputRegress,
         params = 'theta', 
         col = col.theta,
         xlab = '',
         labels = lab.theta,
         xlim = c(-a,a),
         guide_lines = T)

# alpha parameter
col.alpha <- c('forestgreen', 'gray60', 'gray60', 
               'forestgreen', 'forestgreen', 'gray60')
MCMCplot(outputRegress,
         params = 'alpha',
         col = col.alpha,
         xlab = '',
         labels =lab.alpha, 
         guide_lines = T,
         xlim = c(-a,a))


#### Computation of PCP ####

dataset = cbind(z, z.f, z.p)
pcp <- compute_pcp_Jackman(dataset, param[,1:length(names0)], y)

hist(pcp, 
     col = 'orange',
     main = 'Percent Correctly Predicted',
     cex.axis = 2, 
     xlab = '', ylab = '', 
     bty = 'n', 
     yaxt = 'n', 
     breaks = 10)




