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

#### Loading the output ####

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
