# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - PDWRKP - - - - - - - - - - #

# Does having the fact that the partner works affect the employment rate?

#### load the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)

# Focus on south Europe
work <- work[work$rgn == 'South Europe', ]
work$rgn <- NULL

# Focus on people with a partner.working
work <- work[work$prtnr == 1, ]
work$prtnr <- NULL

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

# Convert pdwrkp in factor and drop levels
work$pdwrkp <- as.factor(work$pdwrkp)



#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)



#### PDWRKP: Employment rate by gender in South Europe ####

partner.working.male.tab <- svytable(~pdwrkp+pdwrk, subset(design, gndr == 'male'))
partner.working.female.tab <- svytable(~pdwrkp+pdwrk, subset(design, gndr == 'female'))
partner.working.male <- partner.working.male.tab[,2]/rowSums(partner.working.male.tab)*100
partner.working.female <- partner.working.female.tab[,2]/rowSums(partner.working.female.tab)*100

rm(partner.working.male.tab, partner.working.female.tab)



# PLOT

partner.working <- cbind(partner.working.male, partner.working.female)
colnames(partner.working) <- c('Men', 'Women')

par(mfrow = c(1,1))
barplot(partner.working,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Occupation rate in South Europe', side=3, line = 1)
legend('topright', legend = c('partner.working employed', 'partner.working unemployed'), 
       fill = c('red','darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


#### PDWRKP: Difference between countries WITHOUT GENDER ####

partner.working.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~pdwrkp+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.working.south.NOgender <- cbind(partner.working.south.NOgender, tabnostd)
}
colnames(partner.working.south.NOgender) <- country

rm(tabnostd)



# PLOT

par(mfrow = c(1,1))
barplot(partner.working.south.NOgender,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Occupation rate in South Europe by country', side=3, line = 1)


#### PDWRKP: Difference between countries WITH GENDER ####


partner.working.south.male <- partner.working.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~pdwrkp+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.working.south.male <- cbind(partner.working.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~pdwrkp+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.working.south.female <- cbind(partner.working.south.female, tabnostd.female)
}
colnames(partner.working.south.male) <- country
colnames(partner.working.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

par(mfrow = c(1,2))
barplot(partner.working.south.male,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Men', side=3, line = 0)
barplot(partner.working.south.female,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Women', side=3, line = 0)
mtext('Occupation rate in South Europe by country', outer=T, side=3, line = -1.5)


#### PDWRKP: Difference between countries WITH citizenship ####

partner.working.citizenship.NOgender <- NULL
for( ctz in citizenship){
  tabnostd <- svytable(~pdwrkp+pdwrk, subset(design, ctzmod == ctz))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.working.citizenship.NOgender <- cbind(partner.working.citizenship.NOgender, tabnostd)
}
colnames(partner.working.citizenship.NOgender) <- citizenship

partner.working.citizenship.male <- partner.working.citizenship.female <- NULL
for( ctz in citizenship){
  tabnostd.male <- svytable(~pdwrkp+pdwrk, subset(design, gndr == 'male' & ctzmod == ctz))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.working.citizenship.male <- cbind(partner.working.citizenship.male, tabnostd.male)
  
  tabnostd.female <- svytable(~pdwrkp+pdwrk, subset(design, gndr == 'female' & ctzmod == ctz))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.working.citizenship.female <- cbind(partner.working.citizenship.female, tabnostd.female)
}
colnames(partner.working.citizenship.male) <- citizenship
colnames(partner.working.citizenship.female) <- citizenship

rm(tabnostd, tabnostd.male, tabnostd.female)



# PLOT

par(mfrow = c(1,2))
barplot(partner.working.citizenship.male,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Men', side=3, line = 0)
barplot(partner.working.citizenship.female,
        ylim = c(0,100),
        beside = T,
        col = c('red', 'darkgreen'))
mtext('Women', side=3, line = 0)
mtext('Occupation rate in South Europe by citizenship status', outer=T, side=3, line = -1.5)


rm(cnt, ctz)

