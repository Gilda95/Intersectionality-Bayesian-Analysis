# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - PARTNER - - - - - - - - - - #

# Does having a partner affect the employment rate?

#### load the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)

# Focus on south Europe
work <- work[work$rgn == 'South Europe', ]
work$rgn <- NULL

# Extract vector containing the countries labels
work$cntry <- as.factor(work$cntry)
work$cntry <- droplevels(work$cntry)
country <-  levels(work$cntry)

# Merge native and autochthonous
work$ctzmod[work$ctzmod == 'native'] <- 'autochthonous'

# Convert ctzmod in factor and drop levels
work$ctzmod <- as.factor(work$ctzmod)
work$ctzmod <- droplevels(work$ctzmod)
citizenship <- levels(work$ctzmod)

# Convert prtnr in factor and drop levels
work$prtnr <- as.factor(work$prtnr)

#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)



#### Employment rate by gender in South Europe ####

partner.male.tab <- svytable(~prtnr+pdwrk, subset(design, gndr == 'male'))
partner.female.tab <- svytable(~prtnr+pdwrk, subset(design, gndr == 'female'))
partner.male <- partner.male.tab[,2]/rowSums(partner.male.tab)*100
partner.female <- partner.female.tab[,2]/rowSums(partner.female.tab)*100

rm(partner.male.tab, partner.female.tab)



# PLOT

partner <- cbind(partner.male, partner.female)
colnames(partner) <- c('Men', 'Women')

par(mfrow = c(1,1))
barplot(partner,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'dodgerblue3'))
#mtext('Occupation rate in South Europe', side=3, line = 1)
legend('topright', legend = c('Without partner', 'With partner'), 
       fill = c('red','dodgerblue3'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')



par(mfrow = c(1,1))
barplot(partner,
        ylim = c(0,100),
        beside = T,
        col = c('lightblue1', 'blue', 'rosybrown1', 'red'))
#mtext('Occupation rate in South Europe', side=3, line = 1)
legend('topright', legend = c('male without partner', 'male with partner', 
                              'female without partner', 'female with partner'), 
       fill =c('lightblue1', 'blue', 'rosybrown1', 'red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


#### PRTNR: Difference between countries WITHOUT GENDER ####

partner.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~prtnr+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.south.NOgender <- cbind(partner.south.NOgender, tabnostd)
}
colnames(partner.south.NOgender) <- country

rm(tabnostd)



# PLOT

par(mfrow = c(1,1))
barplot(partner.south.NOgender,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Occupation rate in South Europe by country', side=3, line = 1)



#### PRTNR: Difference between countries WITH GENDER ####


partner.south.male <- partner.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~prtnr+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.south.male <- cbind(partner.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~prtnr+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.south.female <- cbind(partner.south.female, tabnostd.female)
}
colnames(partner.south.male) <- country
colnames(partner.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

par(mfrow = c(1,2))
barplot(partner.south.male,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Men', side=3, line = 0)
barplot(partner.south.female,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Women', side=3, line = 0)
mtext('Occupation rate in South Europe by country', outer=T, side=3, line = -1.5)


#### PRTNR: Difference between countries WITH citizenship ####

partner.citizenship.NOgender <- NULL
for( ctz in citizenship){
  tabnostd <- svytable(~prtnr+pdwrk, subset(design, ctzmod == ctz))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.citizenship.NOgender <- cbind(partner.citizenship.NOgender, tabnostd)
}
colnames(partner.citizenship.NOgender) <- citizenship

partner.citizenship.male <- partner.citizenship.female <- NULL
for( ctz in citizenship){
  tabnostd.male <- svytable(~prtnr+pdwrk, subset(design, gndr == 'male' & ctzmod == ctz))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.citizenship.male <- cbind(partner.citizenship.male, tabnostd.male)
  
  tabnostd.female <- svytable(~prtnr+pdwrk, subset(design, gndr == 'female' & ctzmod == ctz))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.citizenship.female <- cbind(partner.citizenship.female, tabnostd.female)
}
colnames(partner.citizenship.male) <- citizenship
colnames(partner.citizenship.female) <- citizenship

rm(tabnostd, tabnostd.male, tabnostd.female)



# PLOT

par(mfrow = c(1,2))
barplot(partner.citizenship.male,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Men', side=3, line = 0)
barplot(partner.citizenship.female,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Women', side=3, line = 0)
mtext('Occupation rate in South Europe by citizenship status', outer=T, side=3, line = -1.5)


rm(cnt, ctz)



