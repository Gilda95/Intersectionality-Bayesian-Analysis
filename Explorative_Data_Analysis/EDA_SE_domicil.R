# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - DOMICIL - - - - - - - - - - #

# How does the domicil affect the employment rates?

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
citizienship <- levels(work$ctzmod)

# Convert domicil in factor and drop levels
work$domicil <- as.factor(work$domicil)

#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)



#### Employment rate by gender in South Europe ####

domicil.male.tab <- svytable(~domicil+pdwrk, subset(design, gndr == 'male'))
domicil.female.tab <- svytable(~domicil+pdwrk, subset(design, gndr == 'female'))
domicil.male <- domicil.male.tab[,2]/rowSums(domicil.male.tab)*100
domicil.female <- domicil.female.tab[,2]/rowSums(domicil.female.tab)*100

rm(domicil.male.tab, domicil.female.tab)


# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(domicil,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('domicil ratio of men and women in South Europe')

par(mfrow = c(1,1))
plot(1:5, domicil.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:5, names(domicil.male))
abline(v=1:5, col = 'lightgrey')
points(1:5, domicil.female, type = 'o', col = 'red', lwd = 3)
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')

# COMMENT ......................................................................
#
#
# There seems to be some difference, but it is not much clear
# 
# 
#                            => A GENDER BASED DIVISION MAYBE CAN BE JUSTIFIED.
#
#


#### domicil: Difference between countries WITHOUT GENDER ####

domicil.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~domicil+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  domicil.south.NOgender <- cbind(domicil.south.NOgender, tabnostd)
}
colnames(domicil.south.NOgender) <- country

rm(tabnostd)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(domicil.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('domicil ratio of men and women in South Europe')


par(mfrow = c(1,1))
plot(1:5, domicil.south.NOgender[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=1:5, colnames(domicil.south.NOgender))
abline(v=1:5, col = 'lightgrey')
points(1:5, domicil.south.NOgender[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, domicil.south.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(domicil.south.NOgender), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


# COMMENT ......................................................................
#
#
# Except for Cyprus, which has some missing data,
# the other curves are pretty much the same
# 
#                          => I WOULD NOT CONSIDER A COUNTRY-ONLY DIVISION HERE.
#
#



#### domicil: Difference between countries WITH GENDER ####


domicil.south.male <- domicil.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~domicil+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  domicil.south.male <- cbind(domicil.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~domicil+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  domicil.south.female <- cbind(domicil.south.female, tabnostd.female)
}
colnames(domicil.south.male) <- country
colnames(domicil.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(domicil.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('domicil ratio of men and women in South Europe')

par(mfrow = c(1,2))
plot(1:5, domicil.south.male[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:5, colnames(domicil.south.male))
abline(v=1:5, col = 'lightgrey')
points(1:5, domicil.south.male[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, domicil.south.male[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(domicil.south.male), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:5, domicil.south.female[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:5, colnames(domicil.south.female))
abline(v=1:5, col = 'lightgrey')
points(1:5, domicil.south.female[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, domicil.south.female[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(domicil.south.female), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# The curves are somehow different, but this difference is much on the intercept
# so I think the country alone is responsible (not an interaction)
# 
#              => I WOULD NOT CONSIDER domicil IN RELATION OF COUNTRY AND GENDER.
#
#



#### domicil: Difference between countries WITH CITIZIENSHIP ####

domicil.citizienship.NOgender <- NULL
for( ctz in citizienship){
  tabnostd <- svytable(~domicil+pdwrk, subset(design, ctzmod == ctz))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  domicil.citizienship.NOgender <- cbind(domicil.citizienship.NOgender, tabnostd)
}
colnames(domicil.citizienship.NOgender) <- citizienship

domicil.citizienship.male <- domicil.citizienship.female <- NULL
for( ctz in citizienship){
  tabnostd.male <- svytable(~domicil+pdwrk, subset(design, gndr == 'male' & ctzmod == ctz))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  domicil.citizienship.male <- cbind(domicil.citizienship.male, tabnostd.male)
  
  tabnostd.female <- svytable(~domicil+pdwrk, subset(design, gndr == 'female' & ctzmod == ctz))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  domicil.citizienship.female <- cbind(domicil.citizienship.female, tabnostd.female)
}
colnames(domicil.citizienship.male) <- citizienship
colnames(domicil.citizienship.female) <- citizienship

rm(tabnostd, tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(domicil.citizienship,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('domicil ratio of men and women in South Europe')

par(mfrow = c(1,3))
plot(1:3, domicil.citizienship.NOgender[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Whole population', side=3, line = 1)
axis(1, at=1:3, colnames(domicil.citizienship.NOgender))
abline(v=1:3, col = 'lightgrey')
points(1:3, domicil.citizienship.NOgender[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, domicil.citizienship.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(domicil.citizienship.NOgender), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:3, domicil.citizienship.male[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:3, colnames(domicil.citizienship.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, domicil.citizienship.male[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, domicil.citizienship.male[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(domicil.citizienship.male), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:3, domicil.citizienship.female[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:3, colnames(domicil.citizienship.female))
abline(v=1:3, col = 'lightgrey')
points(1:3, domicil.citizienship.female[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, domicil.citizienship.female[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(domicil.citizienship.female), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# As always with the citizienship, women are different from men,
# but here I don't know if I would consider this as an interaction.
# 
#                        => I WOULD CONSIDER domicil IN RELATION OF GENDER ONLY.
#


rm(cnt, ctz)


