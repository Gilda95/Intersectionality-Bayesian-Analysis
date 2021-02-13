# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - EDUCATION - - - - - - - - - - #

# How does the education level affect the employment rates?

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

# Convert edutre in factor and drop levels
work$edutre <- as.factor(work$edutre)

#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)



#### Employment rate by gender in South Europe ####

education.male.tab <- svytable(~edutre+pdwrk, subset(design, gndr == 'male'))
education.female.tab <- svytable(~edutre+pdwrk, subset(design, gndr == 'female'))
education.male <- education.male.tab[,2]/rowSums(education.male.tab)*100
education.female <- education.female.tab[,2]/rowSums(education.female.tab)*100

rm(education.male.tab, education.female.tab)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(education,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')

par(mfrow = c(1,1))
plot(1:3, education.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:3, names(education.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, education.female, type = 'o', col = 'red', lwd = 3)
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')

# COMMENT ......................................................................
#
#
# We can clearly see an overall trend,
# and we can clearly see that a higher level of education reduces the differences.
# 
# 
#           => A GENDER BASED DIVISION IN THE MODEL SEEMS TO BE JUSTIFIED.
#
#


#### EDUTRE: Difference between countries WITHOUT GENDER ####

education.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~edutre+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  education.south.NOgender <- cbind(education.south.NOgender, tabnostd)
}
colnames(education.south.NOgender) <- country

rm(tabnostd)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(education.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')


par(mfrow = c(1,1))
plot(1:5, education.south.NOgender[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=1:5, colnames(education.south.NOgender))
abline(v=1:5, col = 'lightgrey')
points(1:5, education.south.NOgender[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, education.south.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(education.south.NOgender), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


# COMMENT ......................................................................
#
#
# If we don't consider a gender division, 
# we only observe a fluctuation for the lowest level of education in Italy,
# while the rest seems to be fairly constant in changing the countries.
# 
#             => I DON'T KNOW IF I WOULD CONSIDER A COUNTRY-ONLY DIVISION HERE.
#
#



#### EDUTRE: Difference between countries WITH GENDER ####


education.south.male <- education.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~edutre+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  education.south.male <- cbind(education.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~edutre+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  education.south.female <- cbind(education.south.female, tabnostd.female)
}
colnames(education.south.male) <- country
colnames(education.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(education.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')

par(mfrow = c(1,2))
plot(1:5, education.south.male[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:5, colnames(education.south.male))
abline(v=1:5, col = 'lightgrey')
points(1:5, education.south.male[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, education.south.male[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(education.south.male), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:5, education.south.female[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:5, colnames(education.south.female))
abline(v=1:5, col = 'lightgrey')
points(1:5, education.south.female[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, education.south.female[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(education.south.female), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# This plot confirms that a gender division is justified,
# moreover it suggests that on level 1 the country should be important for women.
# 
#                => I WOULD CONSIDER EDUCATION IN RELATION OF COUNTRY AND GENDER.
#
#



#### EDUTRE: Difference between countries WITH citizenship ####

education.citizenship.NOgender <- NULL
for( ctz in citizenship){
  tabnostd <- svytable(~edutre+pdwrk, subset(design, ctzmod == ctz))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  education.citizenship.NOgender <- cbind(education.citizenship.NOgender, tabnostd)
}
colnames(education.citizenship.NOgender) <- citizenship

education.citizenship.male <- education.citizenship.female <- NULL
for( ctz in citizenship){
  tabnostd.male <- svytable(~edutre+pdwrk, subset(design, gndr == 'male' & ctzmod == ctz))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  education.citizenship.male <- cbind(education.citizenship.male, tabnostd.male)
  
  tabnostd.female <- svytable(~edutre+pdwrk, subset(design, gndr == 'female' & ctzmod == ctz))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  education.citizenship.female <- cbind(education.citizenship.female, tabnostd.female)
}
colnames(education.citizenship.male) <- citizenship
colnames(education.citizenship.female) <- citizenship

rm(tabnostd, tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(education.citizenship,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')

par(mfrow = c(1,3))
plot(1:3, education.citizenship.NOgender[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Whole population', side=3, line = 1)
axis(1, at=1:3, colnames(education.citizenship.NOgender))
abline(v=1:3, col = 'lightgrey')
points(1:3, education.citizenship.NOgender[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, education.citizenship.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(education.citizenship.NOgender), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:3, education.citizenship.male[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:3, colnames(education.citizenship.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, education.citizenship.male[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, education.citizenship.male[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(education.citizenship.male), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:3, education.citizenship.female[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:3, colnames(education.citizenship.female))
abline(v=1:3, col = 'lightgrey')
points(1:3, education.citizenship.female[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, education.citizenship.female[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(education.citizenship.female), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# Note that different citizenship status seem to lead to different
# opportunities even id the level of education is the same.
#
# Is this difference to be considered an interaction 
# or it is only a matter of difference in citizenship?
# 
#           => I WOULD CONSIDER EDUCATION IN RELATION OF GENDER AND citizenship.
#              MOREOVER, I WOULD NOT DISCARD THE POSSIBILITY OF A 3 LEVEL GROUP
#              CONSIDERING THE COUNTRY AS WELL, AT LEAST IN THE FREQUENTIST MODELS.
#

rm(cnt, ctz)



