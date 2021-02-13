# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - PARTNER - - - - - - - - - - #

# Does having a partner affect the employment rate?

#### load the dataset ####

setwd('/home/alessandro/Documents/Bay Project/new')
setwd('/Users/gildamatteucci/OneDrive - Politecnico di Milano/PROGETTO_BAYESIANA/DataCleaning_EDA')
setwd("C:/Users/aless/Desktop/POLIMI/MSC2.1/BAYESIAN/progetto")


work <- read.csv('data_work.csv', header = T)

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
        col = c('red', 'darkgreen'))
mtext('Occupation rate in South Europe', side=3, line = 1)
legend('topright', legend = c('Without partner', 'With partner'), 
       fill = c('red','darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')

# COMMENT ......................................................................
#
#
# I find it difficult to comment on this plot,
# but the main difference between men and women in south Europe
# seems to occur when they have a partner, so gender seems important here.
# 
# 
#           => A GENDER BASED DIVISION IN THE MODEL SEEMS TO BE JUSTIFIED.
#
#


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


# COMMENT ......................................................................
#
#
# In general, the difference among the countries seems to be in an absolute sense,
# and not in the interaction with having a partner.
# 
#                            => I WOULD NOT CONSIDER A COUNTRY-ONLY DIVISION HERE.
#
#



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


# COMMENT ......................................................................
#
#
# Again, the two genders are treated differently,
# but such treatment doesn't seem to differ much across the countries.
# 
#             => I WOULD NOT CONSIDER PARTNER IN RELATION OF COUNTRY AND GENDER.
#
#



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


# COMMENT ......................................................................
#
#
# Note that different citizenship status seem to lead to different
# opportunities when having or not having a partner.
#
# In particular, notice the really low percentage of immigrant women 
# with a partner and working.
#
# Notice that it is not completely clear at this point if the difference
# induced by the citizenship status here is just because of its importnace
# or because of an interaction, a real analysis is yet to be performed!
#
# 
#           => I WOULD CONSIDER PARTNER IN RELATION OF GENDER AND citizenship.
#              HOWEVER, HERE I WOULD DISCARD THE POSSIBILITY OF A 3 LEVEL GROUP
#              DISCARDING A COUNTRY DIVISION FOR THIS PARAMETER.
#
#

rm(cnt, ctz)



