# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - CITIZIENSHIP - - - - - - - - - - #

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

#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)


#### CTZMOD: overall composition of the countries ####

# Compute composition of each country .....................................

composition.tot <- NULL
for( cnt in country){
  tabnostd <- svytable(~ctzmod, subset(design, cntry == cnt))
  composition.tot <- cbind(composition.tot, tabnostd)
}
colnames(composition.tot) <- country

# Convert in percentage
for( i in 1:dim(composition.tot)[2] ){
  composition.tot[,i] <- composition.tot[,i]/sum(composition.tot[,i])*100
}

rm(cnt)



# Add the average
composition.south.mean <- svytable(~ctzmod, design)
composition.south.mean <- composition.south.mean/sum(composition.south.mean)*100
composition.south <- cbind(composition.tot, composition.south.mean)
colnames(composition.south) <- c(country, 'Mean')

rm(composition.south.mean)



# PLOT

par(mfrow = c(1,1))
barplot(composition.south,
        col = c('darkgreen', 'darkred', 'darkblue'),
        ylim = c(0,100))
title('South Europe')


# COMMENT ......................................................................
#
#
# The composition in the five countries is quite similar:
#   - autochtonous (green) are above 80% everywhere,
#   - immigrants (blue) takes almost everything of the remaining population
#   - ethnic minorities are maybe not much significant
#
#



#### Employment rate by gender in South Europe ####

occupation.south.male.tab <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'male'))
occupation.south.female.tab <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'female'))
occupation.south.male <- occupation.south.male.tab[,2]/rowSums(occupation.south.male.tab)*100
occupation.south.female <- occupation.south.female.tab[,2]/rowSums(occupation.south.female.tab)*100

rm(occupation.south.male.tab, occupation.south.female.tab)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(occupation.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('Occupation ratio of men and women in South Europe')

par(mfrow = c(1,1))
plot(1:3, occupation.south.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:3, names(occupation.south.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, occupation.south.female, type = 'o', col = 'red', lwd = 3)
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')

# COMMENT ......................................................................
#
#
# Overall, in South Europe, men have an higher employment rate in general:
#   - difference in autochtonous is around 15%,
#   - difference in immigrant is higher, around 20%
#   - roles are swapped in ethnic minorities,
#     but we don't have much data for this group so it could be unreliable
# 
# 
#           => A GENDER BASED DIVISION IN THE MODEL SEEMS TO BE JUSTIFIED.
#
#

rm(occupation.south, occupation.south.female)


#### CTZMOD: Difference between countries WITHOUT GENDER ####

occupation.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~ctzmod+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  occupation.south.NOgender <- cbind(occupation.south.NOgender, tabnostd)
}
colnames(occupation.south.NOgender) <- country

rm(tabnostd)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(occupation.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('Occupation ratio of men and women in South Europe')


par(mfrow = c(1,1))
plot(1:5, occupation.south.NOgender[1,], type = 'o', col = 'darkgreen', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=1:5, colnames(occupation.south.NOgender))
abline(v=1:5, col = 'lightgrey')
points(1:5, occupation.south.NOgender[2,], type = 'o', col = 'darkred', lwd = 3)
points(1:5, occupation.south.NOgender[3,], type = 'o', col = 'darkblue', lwd = 3)
legend('bottomleft', legend = rownames(occupation.south.NOgender), 
       fill = c('darkgreen', 'darkred', 'darkblue'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


# COMMENT ......................................................................
#
#
# There is some fluctuation between the countries,
# but I don't know if is enough to divide the populations in countries:
#   - Cyprus behave differently than the others in everything but autochtonous,
#   - immigrants don't change much in the other countries
#   - autochtonous are stable, except for Slovenia
#   - the most unstable is ethnic minorities, but again we have few data
# 
#             => I DON'T KNOW IF I WOULD CONSIDER A COUNTRY-ONLY DIVISION HERE.
#
#



#### CTZMOD: Difference between countries WITH GENDER ####


occupation.south.male <- occupation.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  occupation.south.male <- cbind(occupation.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  occupation.south.female <- cbind(occupation.south.female, tabnostd.female)
}
colnames(occupation.south.male) <- country
colnames(occupation.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(occupation.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('Occupation ratio of men and women in South Europe')

par(mfrow = c(1,2))
plot(1:5, occupation.south.male[1,], type = 'o', col = 'darkgreen', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:5, colnames(occupation.south.male))
abline(v=1:5, col = 'lightgrey')
points(1:5, occupation.south.male[2,], type = 'o', col = 'darkred', lwd = 3)
points(1:5, occupation.south.male[3,], type = 'o', col = 'darkblue', lwd = 3)
legend('bottomleft', legend = rownames(occupation.south.male), 
       fill = c('darkgreen', 'darkred', 'darkblue'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:5, occupation.south.female[1,], type = 'o', col = 'darkgreen', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:5, colnames(occupation.south.female))
abline(v=1:5, col = 'lightgrey')
points(1:5, occupation.south.female[2,], type = 'o', col = 'darkred', lwd = 3)
points(1:5, occupation.south.female[3,], type = 'o', col = 'darkblue', lwd = 3)
legend('bottomleft', legend = rownames(occupation.south.female), 
       fill = c('darkgreen', 'darkred', 'darkblue'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1)


# COMMENT ......................................................................
#
#
# Considering country AND gender, the difference is HUGE!
# Especially for women!
# 
#             => I WOULD CONSIDER CITIZIENSHIP IN RELATION OF COUNTRY AND GENDER.
#
#

rm(cnt, i)
