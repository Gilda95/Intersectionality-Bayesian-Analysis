# - - - - - FREQUENTIST MODELS FOR PDWRK - - - - - #

#### Loading the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)


#### Organize dataset ####

# merging native and autochthonous
work$ctzmod[work$ctzmod=='native'] <- 'autochthonous'
work$ctzmod <- droplevels(work$ctzmod)

# Columns useless...
work$chld3 <- NULL
work$chld10 <- NULL
work$domicil <- NULL
work$edumod <- NULL
work$edupmod <- NULL

#### Convert to factor ####

work$gndr <- as.factor(work$gndr)
work$cntry <- as.factor(work$cntry)
work$ctzmod <- as.factor(work$ctzmod)
work$brnmod <- as.factor(work$brnmod)
work$prtnr <- as.factor(work$prtnr)
#work$pdwrkp <- as.factor(work$pdwrkp)
work$isco08p <- as.factor(work$isco08p)
work$rgn <- as.factor(work$rgn)
work$edutre <- as.factor(work$edutre)
work$eduptre <- as.factor(work$eduptre)

# choose first factor
work$gndr <- relevel(work$gndr, 'male')
work$ctzmod <- relevel(work$ctzmod, 'autochthonous') 
work$brnmod <- relevel(work$brnmod, 'Rich')
work$prtnr <- relevel(work$prtnr, '0')
work$isco08p <- relevel(work$isco08p, '6666666666')
work$edutre <- relevel(work$edutre, '1')

#### Region selection ####
# data <- work[work$rgn == "North Europe",]
data <- work[work$rgn == "South Europe",]
# data <- work[work$rgn == "West Europe",]
# data <-  work[work$rgn == "East Europe",]
# data <- work[work$rgn == "Balkans",]


#### Define auxiliary variables ####

# response variable
y <- data$pdwrk

# female/male dummy
female <- ifelse (data$gndr == 'female', 1, 0)

# partner dummy
partner <- ifelse(data$prtnr == '1', 1, 0)

# partner working dummy
partner <- ifelse(data$pdwrkp == '1', 1, 0)

# immigrant dummy 
immigrant <- ifelse(data$ctzmod == 'immigrant', 1, 0)

# scaling continuous variables
data$agea <- scale(data$agea)
data$chld14 <- scale(data$chld14)

#### Frequentist GLM ####

freq_glm <- glm(y ~ 1 + ctzmod + edutre +  agea + chld14 +  prtnr + immigrant:brnmod + 
                female:(1 + ctzmod + edutre +  agea + chld14 +  prtnr + immigrant:brnmod) +
                partner:(eduptre + pdwrkp:isco08p), 
                family = binomial, 
                data = data)

summary(freq_glm)


freq_glm_reduced <- glm(y ~ 1 + ctzmod + edutre + agea + chld14 + prtnr + 
                   female:(1 + ctzmod + edutre + agea + chld14 + prtnr) + 
                   partner:(pdwrkp + pdwrkp:isco08p),
                 family = binomial, 
                 data = data)

summary(freq_glm_reduced)


