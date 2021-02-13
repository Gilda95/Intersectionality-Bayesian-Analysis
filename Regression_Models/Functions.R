
rm(list = ls())
setwd('/home/alessandro/Documents/Bay Project')


# COMPUTE_PCP   -------------------------------------------------------------

compute_pcp_Jackman <- function(dataset, param, obs){
  # Computes the percentage of correct predictions
  # (as suggested by Jackman's book at page 386)
  # choosing empirical mean of the observations as threshold.
  #
  # CALL:
  # dataset <- cbind(z, z.female, h)
  # pcp <- compute_pcp_Jackman(dataset, param, y)
  # 
  # INPUT:
  # - dataset = dataset with  all observations
  # - param = matrix with all parameters
  # - obs = observations of the response
  #
  # OUTPUT:
  # pcp = vector with pcp for each person
  
  # Auxiliary variables
  n.draws <- dim(param)[1] # number of draws of the MCMC
  n.obs <- dim(dataset)[1] # number of observations
  
  # Compute samples from the posterior predictive distribution
  pred <- dataset %*% t(param) # linear model
  pred <- pnorm(pred) # probit link
  
  # Compute empirical mean
  threshold <- mean(obs)
  
  # Compute PCP
  pcp <- NULL
  for(i in 1:n.obs){
    
    if(obs[i] == 1){ # if i works, we want high pred
      pcp[i] <- sum(pred[i,] > threshold)
    }
    if(obs[i] == 0){ # if i doesn't work, we want low pred
      pcp[i] <- sum(pred[i,] < threshold)
    }
    
  }
  
  # pcp now is the total number of corrected precitions
  # for each individual i (at most it can be n.draws).
  # To recover the probabilities we have to rescale it
  pcp <- pcp/n.draws
  
  # Return...
  return(pcp)
}

# COMPUTE_PPRED -------------------------------------------------------------

compute_pred <- function(dataset, param, obs){
  # Computes the percentage of correct predictions
  # (as suggested by Jackman's book at page 386)
  # choosing empirical mean of the observations as threshold.
  #
  # CALL:
  # dataset <- cbind(z, z.female, h)
  # pred <- compute_pred(dataset, param, y)
  # 
  # INPUT:
  # - dataset = dataset with  all observations
  # - param = matrix with all parameters
  # - obs = observations of the response
  #
  # OUTPUT:
  # pred = matrix with the posterior predictive distributions
  
  # Auxiliary variables
  n.draws <- dim(param)[1] # number of draws of the MCMC
  n.obs <- dim(dataset)[1] # number of observations
  
  # Compute samples from the posterior predictive distribution
  pred <- dataset %*% t(param) # linear model
  pred <- pnorm(pred) # probit link
  
  # Return...
  return(pred)
}



# COMPUTE_PPRED_INT -------------------------------------------------------------

compute_pred_int <- function(pred, quantiles = c(0.025, 0.975)){
  # Computes the percentage of correct predictions
  # (as suggested by Jackman's book at page 386)
  # choosing empirical mean of the observations as threshold.
  #
  # CALL:
  # pred_int <- compute_pred_int(pred)
  # 
  # INPUT:
  # - pred = output of compute_pred function
  # - quantiles: list of required quantiles for PCI
  #              by default it computes 2.5% and 97.5% quantiles
  #
  # OUTPUT:
  # pred_int = matrix with posterior predictive empirical quantiles 
  #            with one row for each person ad one column for each quantile
  
  n.obs <- dim(pred)[1]
  
  empirical_quantiles <- matrix(0, nrow = n.obs, ncol = length(quantiles))
  # Compute quantiles
  for(i in 1:n.obs){
    empirical_quantiles[i,] <- quantile(pred[i,], prob = quantiles)
  }
  
  # Return...
  return(empirical_quantiles)
}


# COMPUTE_ERROR_RATES -----------------------------------------------------

compute_error_rates <- function(pred_CI, obs, threshold = NULL){
  # Computes false/true positive/negative rates and adds them in a matrix
  # 
  # CALL:
  # error_rates <- compute_error_rates(pred, y)
  #
  # INPUT:
  # - pred = output of compute_pred function
  # - obs = observations of the response
  # - threshold = a vector of candidate threshold to evaluate
  #               if not provided, a vector to check any 5% is automatically created
  #
  # OUTPUT:
  # error_rates <- matrix with four columns with the false/true predictions
  #                and a row for each element in threshold
  
  n.obs <- length(obs)
  n.work <- sum(obs)
  n.nowork <- n.obs - n.work
  
    # It threshold is not provided, evaluate any 5%
  if(is.null(threshold)){
    threshold <- seq(0, 1, 0.05)
  }

  n.threshold <- length(threshold)
  
  error_rates <- matrix(NA, nrow = n.threshold, ncol = 4)
  for(i in 1:n.threshold){
    
    # Working (correct prediction)
    error_rates[i,1] <- sum( (pred_CI[,1] > threshold[i]) & (obs == 1) ) / n.work
    
    # Working (wrong prediction)
    error_rates[i,2] <- sum( (pred_CI[,2] < threshold[i]) & (obs == 1) ) / n.work
    
    # Not working (correct prediction)
    error_rates[i,3] <- sum( (pred_CI[,2] < threshold[i]) & (obs == 0) ) / n.nowork
    
    # Not working (wrong prediction)
    error_rates[i,4] <- sum( (pred_CI[,1] > threshold[i]) & (obs == 0) ) / n.nowork
    
  }
  
  colnames(error_rates) <- c('working (correct)', 'working (wrong)', 'NOT working (correct)', 'NOT working (wrong)')
  
  return(error_rates)
  
}


# PLOT_ERROR_RATES -------------------------------------------------------------

plot_error_rates <- function(error_rates, threshold = NULL){
  # Plot the error rates
  #
  # CALL:
  # plot_error_rates(error_rates)
  # 
  # INPUT:
  # - error_rates = output of compute_error_rates
  # - threshold = SAME thresholds given to compute_error_rates
  
  if(is.null(threshold)){
    threshold <- seq(0, 1, 0.05)
  }
  
  par(mfrow = c(1,1))
  plot(threshold, error_rates[,1], type='l', lwd = 2, col = 'green',
       xlim = c(0, 1), ylim = c(0,1), cex.axis = 2, bty='n', 
       xlab='', ylab='')
  mtext("Threshold" , side = 1 , cex=2, line = 3)
  mtext("Error rates" , side = 2 , cex=2, line = 2.5)
  points(threshold, error_rates[,2], type='l', lwd = 2, col = 'orange')
  points(threshold, error_rates[,3], type='l', lwd = 2, col = 'blue')
  points(threshold, error_rates[,4], type='l', lwd = 2, col = 'red')
  
  legend(0, 0.8, legend=c('T pos', 'F neg', 'T neg', 'F pos'),
         col=c('green', 'red', 'blue', 'orange'), lty=1, cex=2)
  
}


# FIND_SUSPECT_INDEXES -----------------------------------------------------

find_suspect_indexes <- function(pred_CI, obs, threshold){
  # Computes false/true positive/negative rates and adds them in a matrix
  # 
  # CALL:
  # threshold <- 0.5
  # suspect.index.list <- find_suspect_indexes(pred, y, threshold)
  #
  # INPUT:
  # - pred = output of compute_pred function
  # - obs = observations of the response
  # - threshold = a number with the selected threshold
  #
  # OUTPUT:
  # suspect.index = list containing the indexes of problematic predictions 
  #                 (provided ONLY if requested)
  
  n.obs <- length(obs)
  
  working_misclassifies <- NULL
  NO_working_misclassifies <- NULL
  
  i.work <- 1
  i.NOwork <- 1
  
  for(i in 1:n.obs){
    
    # Working (wrong prediction)
    if( (pred_CI[i,2] < threshold) & (obs[i] == 1) ){
      working_misclassifies[i.work] <- i
      i.work <- i.work + 1
    }
    
    # NOT working (wrong prediction)
    if( (pred_CI[i,1] > threshold) & (obs[i] == 0) ){
      NO_working_misclassifies[i.NOwork] <- i
      i.NOwork <- i.NOwork + 1
    }
    
  }
  
  # If one of the vectors is still emptu, assign -1 so you don't get error in list creation
  if(is.null(working_misclassifies)){
    working_misclassifies <- -1
  }
  if(is.null(NO_working_misclassifies)){
    NO_working_misclassifies <- -1
  }
  
  return(list(false_negative = working_misclassifies, false_positive = NO_working_misclassifies))
  
}


# PLOT_SAMPLE_POSTERIOR_CREDIBLE_INTERVALS -------------------------------------------------------------

plot_sample_posterior_credible_intervals <- function(pred_CI, obs, threshold, n.sample, seed = 123, want.index = FALSE){
  # Computes the percentage of correct predictions
  # (as suggested by Jackman's book at page 386)
  # choosing empirical mean of the observations as threshold.
  #
  # CALL:
  # n.sample <- 500
  # sample.index <- plot_sample_posterior_credible_intervals(pred_CI, y, threshold, n.sample, want.index=T)
  # 
  # INPUT:
  # - pred_CI = output of compute_pred_int function
  # - obs = vector of actual observations
  # - threshold = NUMBER we want to use for comparision
  # - n.sample = size of the sample to plot
  # - seed = seed for selecting the same sample
  # - want.index = if TRUE, functions returns the indexes of the plotted observations
  #
  # OUTPUT:
  # sample.index = vector storing the indexes selected by the sample
  
  set.seed(seed)
  
  # Select the sample
  n.obs <- length(obs)
  sample.index <- sample(1:n.obs, size = n.sample)
  pred_CI.sample <- pred_CI[sample.index, ]
  obs.sample <- obs[sample.index]
  
  # Set colors according to the threshold
  col.plot <- rep('grey', times = n.sample)
  
  for(i in 1:n.sample){
    
    if( (pred_CI.sample[i,1] > threshold) & (obs.sample[i] == 1) ){
      col.plot[i] <- 'green'            # Classification: WORKING,         reality: WORKING
    }
    if( (pred_CI.sample[i,2] < threshold) & (obs.sample[i] == 1) ){
    col.plot[i] <- 'orange'                # Classification: NOT WORKING,     reality: WORKING
    }
    if( (pred_CI.sample[i,2] < threshold) & (obs.sample[i] == 0) ){
    col.plot[i] <- 'blue'               # Classification: NOT WORKING,     reality: NOT WORKING
    }
    if( (pred_CI.sample[i,1] > threshold) & (obs.sample[i] == 0) ){
    col.plot[i] <- 'red'             # Classification: WORKING,         reality: NOT WORKING
    }
    
  }
  
  par(mfrow = c(1,1))
  plot(c(1,1), pred_CI.sample[1,], type='l', lwd = 2, col = col.plot[1],
       xlim = c(1, 1.2*n.sample), ylim = c(0,1),
       xlab='', ylab='Posterior predictive interval')
  for(i in 2:n.sample){
    points(c(i,i), pred_CI.sample[i,], type = 'l', lwd = 2, col = col.plot[i])
  }
  
  # Add line for sample mean
  abline(h=threshold)
  
  # Legend
  legend(n.sample, 0.6, legend=c('uncertain', 'T pos', 'F neg', 'T neg', 'F pos', 'threshold'),
         col=c('grey', 'green', 'red', 'blue', 'orange', 'black'), lty=1, cex=0.8)
    
  # Return...
  if(want.index){
    return(sample.index)
  }
  
}


# EXTRACT_PARAM -----------------------------------------------------------

# extract_param <- function(outputRegress, param.names, final.names, final.order = NA){
#   
#   data.out <- as.matrix(outputRegress) # trasform the mcmc.list into a matrix,
#   data.out <- data.frame(data.out)
#   
#   n.param.to.be.ordered <- length(param.names)
#   
#   param <- NULL
#   n.param.to.be.ordered.cat <- rep(0, times = n.param.to.be.ordered)
#   for(i in 1:n.param.to.be.ordered){
#     
#     # Find the correct parameters in data.out
#     name <- param.names[i]
#     temp <- data.out[,grep(name, colnames(data.out), value = FALSE)]
#     
#     # Store in param
#     param <- cbind(param, temp)
#     
#     # Delete from data.out
#     data.out <- data.out[, -temp]
#     
#   }
#   
#   # Re-order the parameters
#   if(!is.na(final.names)){
#     
#     new.order <- rep(0, times = dim(param)[2])
#     for(i in 1:length(final.names)){
#       
#       new.order[colnames(data.out) == final.names[i]]
#       
#     }
#     
#   }
#   
#   
#   
# }
# 
# 
# 
# names <- c(colnames(z), paste('female', colnames(z)))
# 
# # Export beta, theta
# beta <- data.out[,grep('beta', colnames(data.out), value = FALSE)]
# theta <- data.out[,grep('theta', colnames(data.out), value = FALSE)]
# # beta.p <- beta[,grep('beta.p', colnames(beta), value = FALSE)]
# # theta.p <- theta[,grep('theta.p', colnames(theta), value = FALSE)]
# # beta <- beta[,-grep('beta.p', colnames(beta), value = FALSE)]
# # theta <- theta[,-grep('theta.p', colnames(theta), value = FALSE)]
# # param <- cbind(beta, beta.p, theta, theta.p)
# param <- cbind(beta, theta)

# MEANVAR_CAT -------------------------------------------------------------

meanvar_cat <- function(dataset, dataset.immigrant, dataset.partner, dataset.partner.work){
  # Computes the table of summary statistics for categorical variables in the dataset
  #
  # INPUT: tre dataframes:
  # - dataset: deve contedere le colonne 'pdwrk' e 'gndr'
  # . dataset.immigrant: deve contenere la colonna 'ctzmod'
  # - dataset.partner: deve contenere la colonna 'prtnr'
  # - dataset.partner.work: deve contenere la colonna 'pdwrkp
  #
  # CALL:
  # columns_of_interest <- c('pdwrk', 'gndr', 'ctzmod', 'edutre', 'prtnr')
  # columns_of_interest_immigrant <- c('ctzmod', 'brnmod')
  # columns_of_interest_partner <- c('prtnr', 'pdwrkp', 'eduptre')
  # columns_of_interest_partner_work <- c('pdwrkp', 'isco08p')
  # meanvar <- meanvar_cat(data[, columns_of_interest], 
  #                        data[, columns_of_interest_immigrant], 
  #                        data[, columns_of_interest_partner],
  #                        data[, columns_of_interest_partner_work])
  #
  # OUTPUT:
  # un dataframe con una colonna per ogni categoria e 6 righe:
  # - numerosità maschi/femmine
  # - media maskii/femmine
  # - varianza maschi/femmine
  #
  # NOTA 1: dataset.partner viene valutato solo se c'è un partner,
  # dataset.partner.work viene valutato solo se il partner lavora.
  #
  # NOTA 2: per convertire in LaTeX usa questi comandi:
  # library(xtable)
  # xtable(t(meanvar))
    
  library(fastDummies)
  
  # separo pdwrk e gender
  pdwrk <- dataset$pdwrk
  gender <- dataset$gndr
  dataset$pdwrk <- NULL
  dataset$gndr <- NULL
  
  # separo immigrant
  immigrant <- ifelse(dataset.immigrant$ctzmod == 'immigrant', 1, 0)
  dataset.immigrant$ctzmod <- NULL
  
  # separo partner
  partner <- dataset.partner$prtnr
  dataset.partner$prtnr <- NULL
  
  # separo pdwrkp
  partner.work <- dataset.partner.work$pdwrkp
  dataset.partner.work$pdwrkp <- NULL
  
  # divido in dummies dataset
  temp <- dummy_cols(dataset)
  dataset <- temp[,(dim(dataset)[2]+1):dim(temp)[2] ]
  n <- dim(dataset)[2]
  
  # divido in dummies dataset.immigrant
  temp <- dummy_cols(dataset.immigrant)
  dataset.immigrant <- temp[,(dim(dataset.immigrant)[2]+1):dim(temp)[2] ]
  n.immigrant <- dim(dataset.immigrant)[2]
  
  # divido in dummies dataset.partner
  temp <- dummy_cols(dataset.partner)
  dataset.partner <- temp[,(dim(dataset.partner)[2]+1):dim(temp)[2] ]
  n.partner <- dim(dataset.partner)[2]
  
  # divido in dummies dataset.partner.work
  temp <- dummy_cols(dataset.partner.work)
  dataset.partner.work <- temp[,(dim(dataset.partner.work)[2]+1):dim(temp)[2] ]
  n.partner.work <- dim(dataset.partner.work)[2]
  
  # medie e varianze
  meanvar <- matrix(nrow = 6, ncol = 1+n+n.immigrant+n.partner+n.partner.work)
  rownames(meanvar) <- c('numerosity_male', 'numerosity_female', 
                         'mean_male', 'mean_female', 
                         'variance_male', 'variance_female')
  colnames(meanvar) <- c('general', colnames(dataset), colnames(dataset.immigrant), 
                         colnames(dataset.partner), colnames(dataset.partner.work))
  
  meanvar[1,1] <- sum(pdwrk[gender == 'male'])
  meanvar[2,1] <- sum(pdwrk[gender == 'female'])
  meanvar[3,1] <- mean(pdwrk[gender == 'male'])
  meanvar[4,1] <- mean(pdwrk[gender == 'female'])
  meanvar[5,1] <- var(pdwrk[gender == 'male'])
  meanvar[6,1] <- var(pdwrk[gender == 'female'])
  for(j in 1:n){
    meanvar[1,j+1] <- sum(pdwrk[dataset[,j]==1 & gender == 'male'])
    meanvar[2,j+1] <- sum(pdwrk[dataset[,j]==1 & gender == 'female'])
    meanvar[3,j+1] <- mean(pdwrk[dataset[,j]==1 & gender == 'male'])
    meanvar[4,j+1] <- mean(pdwrk[dataset[,j]==1 & gender == 'female'])
    meanvar[5,j+1] <- var(pdwrk[dataset[,j]==1 & gender == 'male'])
    meanvar[6,j+1] <- var(pdwrk[dataset[,j]==1 & gender == 'female'])
  }
  for(j in 1:n.immigrant){
    meanvar[1,j+1+n] <- sum(pdwrk[dataset.immigrant[,j]==1 & immigrant == 1
                                  & gender == 'male'])
    meanvar[2,j+1+n] <- sum(pdwrk[dataset.immigrant[,j]==1 & immigrant == 1
                                  & gender == 'female'])
    meanvar[3,j+1+n] <- mean(pdwrk[dataset.immigrant[,j]==1 & immigrant == 1
                                   & gender == 'male'])
    meanvar[4,j+1+n] <- mean(pdwrk[dataset.immigrant[,j]==1 & immigrant == 1
                                   & gender == 'female'])
    meanvar[5,j+1+n] <- var(pdwrk[dataset.immigrant[,j]==1 & immigrant == 1
                                  & gender == 'male'])
    meanvar[6,j+1+n] <- var(pdwrk[dataset.immigrant[,j]==1 & immigrant == 1
                                  & gender == 'female'])
  }
  for(j in 1:n.partner){
    meanvar[1,j+1+n+n.immigrant] <- sum(pdwrk[dataset.partner[,j]==1 & partner == 1
                                              & gender == 'male'])
    meanvar[2,j+1+n+n.immigrant] <- sum(pdwrk[dataset.partner[,j]==1 & partner == 1
                                              & gender == 'female'])
    meanvar[3,j+1+n+n.immigrant] <- mean(pdwrk[dataset.partner[,j]==1 & partner == 1
                                               & gender == 'male'])
    meanvar[4,j+1+n+n.immigrant] <- mean(pdwrk[dataset.partner[,j]==1 & partner == 1
                                               & gender == 'female'])
    meanvar[5,j+1+n+n.immigrant] <- var(pdwrk[dataset.partner[,j]==1 & partner == 1
                                              & gender == 'male'])
    meanvar[6,j+1+n+n.immigrant] <- var(pdwrk[dataset.partner[,j]==1 & partner == 1
                                              & gender == 'female'])
  }
  for(j in 1:n.partner.work){
    meanvar[1,j+1+n+n.immigrant+n.partner] <- sum(pdwrk[dataset.partner.work[,j]==1 & partner == 1
                                                        & partner.work == 1 & gender == 'male'])
    meanvar[2,j+1+n+n.immigrant+n.partner] <- sum(pdwrk[dataset.partner.work[,j]==1 & partner == 1
                                                        & partner.work == 1 & gender == 'female'])
    meanvar[3,j+1+n+n.immigrant+n.partner] <- mean(pdwrk[dataset.partner.work[,j]==1 & partner == 1
                                                         & partner.work == 1 & gender == 'male'])
    meanvar[4,j+1+n+n.immigrant+n.partner] <- mean(pdwrk[dataset.partner.work[,j]==1 & partner == 1
                                                         & partner.work == 1 & gender == 'female'])
    meanvar[5,j+1+n+n.immigrant+n.partner] <- var(pdwrk[dataset.partner.work[,j]==1 & partner == 1
                                                        & partner.work == 1 & gender == 'male'])
    meanvar[6,j+1+n+n.immigrant+n.partner] <- var(pdwrk[dataset.partner.work[,j]==1 & partner == 1
                                                        & partner.work == 1 & gender == 'female'])
  }
  
  return(data.frame(meanvar))
}


# SAVE FUNCTIONS IN .RData FILE -------------------------------------------

save.image(file='Functions.RData')







