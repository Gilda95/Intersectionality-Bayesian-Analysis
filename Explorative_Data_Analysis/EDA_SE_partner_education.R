# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - PARTNER EDUCATION - - - - - - - - - - #

#### load the dataset ####

setwd('/home/alessandro/Documents/Bay Project/new')
setwd('/Users/gildamatteucci/OneDrive - Politecnico di Milano/PROGETTO_BAYESIANA/DataCleaning_EDA')
setwd("C:/Users/aless/Desktop/POLIMI/MSC2.1/BAYESIAN/progetto")


work <- read.csv('data_work.csv', header = T)

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
citizienship <- levels(work$ctzmod)

# Convert eduptre in factor and drop levels
work$edutre <- as.factor(work$edutre)
work$eduptre <- as.factor(work$eduptre)

#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)



#### Employment rate by gender in South Europe ####

partner.education.male.tab <- svytable(~eduptre+pdwrk, subset(design, gndr == 'male'))
partner.education.female.tab <- svytable(~eduptre+pdwrk, subset(design, gndr == 'female'))
partner.education.male <- partner.education.male.tab[,2]/rowSums(partner.education.male.tab)*100
partner.education.female <- partner.education.female.tab[,2]/rowSums(partner.education.female.tab)*100

rm(partner.education.male.tab, partner.education.female.tab)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(education,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')

par(mfrow = c(1,1))
plot(1:3, partner.education.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:3, names(partner.education.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, partner.education.female, type = 'o', col = 'red', lwd = 3)
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


#### eduptre: Difference between countries WITHOUT GENDER ####

partner.education.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~eduptre+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.education.south.NOgender <- cbind(partner.education.south.NOgender, tabnostd)
}
colnames(partner.education.south.NOgender) <- country

rm(tabnostd)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(partner.education.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')


par(mfrow = c(1,1))
plot(1:5, partner.education.south.NOgender[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=1:5, colnames(partner.education.south.NOgender))
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.education.south.NOgender[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, partner.education.south.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(partner.education.south.NOgender), 
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



#### eduptre: Difference between countries WITH GENDER ####


partner.education.south.male <- partner.education.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~eduptre+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.education.south.male <- cbind(partner.education.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~eduptre+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.education.south.female <- cbind(partner.education.south.female, tabnostd.female)
}
colnames(partner.education.south.male) <- country
colnames(partner.education.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(partner.education.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')

par(mfrow = c(1,2))
plot(1:5, partner.education.south.male[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:5, colnames(partner.education.south.male))
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.education.south.male[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, partner.education.south.male[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(partner.education.south.male), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:5, partner.education.south.female[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:5, colnames(partner.education.south.female))
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.education.south.female[2,], type = 'o', col = 'gold', lwd = 3)
points(1:5, partner.education.south.female[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(partner.education.south.female), 
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



#### eduptre: Difference between countries WITH CITIZIENSHIP ####

partner.education.citizienship.NOgender <- NULL
for( ctz in citizienship){
  tabnostd <- svytable(~eduptre+pdwrk, subset(design, ctzmod == ctz))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.education.citizienship.NOgender <- cbind(partner.education.citizienship.NOgender, tabnostd)
}
colnames(partner.education.citizienship.NOgender) <- citizienship

partner.education.citizienship.male <- partner.education.citizienship.female <- NULL
for( ctz in citizienship){
  tabnostd.male <- svytable(~eduptre+pdwrk, subset(design, gndr == 'male' & ctzmod == ctz))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.education.citizienship.male <- cbind(partner.education.citizienship.male, tabnostd.male)
  
  tabnostd.female <- svytable(~eduptre+pdwrk, subset(design, gndr == 'female' & ctzmod == ctz))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.education.citizienship.female <- cbind(partner.education.citizienship.female, tabnostd.female)
}
colnames(partner.education.citizienship.male) <- citizienship
colnames(partner.education.citizienship.female) <- citizienship

rm(tabnostd, tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(partner.education.citizienship,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('education ratio of men and women in South Europe')

par(mfrow = c(1,3))
plot(1:3, partner.education.citizienship.NOgender[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Whole population', side=3, line = 1)
axis(1, at=1:3, colnames(partner.education.citizienship.NOgender))
abline(v=1:3, col = 'lightgrey')
points(1:3, partner.education.citizienship.NOgender[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, partner.education.citizienship.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(partner.education.citizienship.NOgender), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:3, partner.education.citizienship.male[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:3, colnames(partner.education.citizienship.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, partner.education.citizienship.male[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, partner.education.citizienship.male[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(partner.education.citizienship.male), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(1:3, partner.education.citizienship.female[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:3, colnames(partner.education.citizienship.female))
abline(v=1:3, col = 'lightgrey')
points(1:3, partner.education.citizienship.female[2,], type = 'o', col = 'gold', lwd = 3)
points(1:3, partner.education.citizienship.female[3,], type = 'o', col = 'darkgreen', lwd = 3)
legend('bottomleft', legend = rownames(partner.education.citizienship.female), 
       fill = c('red', 'gold', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# Note that different citizienship status seem to lead to different
# opportunities even id the level of education is the same.
#
# Is this difference to be considered an interaction 
# or it is only a matter of difference in citizienship?
# 
#           => I WOULD CONSIDER EDUCATION IN RELATION OF GENDER AND CITIZIENSHIP.
#              MOREOVER, I WOULD NOT DISCARD THE POSSIBILITY OF A 3 LEVEL GROUP
#              CONSIDERING THE COUNTRY AS WELL, AT LEAST IN THE FREQUENTIST MODELS.
#

rm(cnt, ctz)




#### Relationship with edu3 ####

svytable(~edutre+eduptre, design)
#       eduptre
# edutre         1         2         3
#      1 801.85774 250.52794  80.87445
#      2 282.70706 557.89797 198.47491
#      3  93.63130 266.20877 511.88877


