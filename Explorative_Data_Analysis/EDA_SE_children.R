# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - CHILDREN - - - - - - - - - - #

# How does the number of children affect the employment rates?

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


#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, weights = ~anweight, nest = T, 
                    data = work)

#### Table with numerosity ####
svytable(~chld14, design)
# chld14
#           0           1           2           3           4           5           6 
# 2980.172088 1069.687040  588.302461   82.940837   16.176420    1.884988    2.009085

# I decide to exclude from this analysis people 5,6 children,
# so if people have 5,6 children are treated as if they had 4.

# Convert chld14 in factor and drop levels
k <- 3    # max number of children to consider separately
if (k == 3){ work$chld14[work$chld14 == 4 | work$chld14 == 5 | work$chld14 == 6] <- 3 }
if (k == 4){ work$chld14[work$chld14 == 5 | work$chld14 == 6] <- 4 }
work$chld14 <- as.factor(work$chld14)
work$chld14 <- droplevels(work$chld14)

#### Define the weights design (update) ####

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)


#### Employment rate by gender in South Europe ####

children.male.tab <- svytable(~chld14+pdwrk, subset(design, gndr == 'male'))
children.female.tab <- svytable(~chld14+pdwrk, subset(design, gndr == 'female'))
children.male <- children.male.tab[,2]/rowSums(children.male.tab)*100
children.female <- children.female.tab[,2]/rowSums(children.female.tab)*100

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(children,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('children ratio of men and women in South Europe')

par(mfrow = c(1,1))
plot(0:k, children.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=0:k, names(children.male))
abline(v=0:k, col = 'lightgrey')
points(0:k, children.female, type = 'o', col = 'red', lwd = 3)
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')

# COMMENT ......................................................................
#
#
# The two behaviors are definitely different.
# 
# 
#           => A GENDER BASED DIVISION IN THE MODEL SEEMS TO BE JUSTIFIED.
#
#


#### CHLD14: Difference between countries WITHOUT GENDER ####

children.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~chld14+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  children.south.NOgender <- cbind(children.south.NOgender, tabnostd)
}
colnames(children.south.NOgender) <- country

rm(tabnostd)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(children.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('children ratio of men and women in South Europe')


par(mfrow = c(1,1))
plot(0:4, children.south.NOgender[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=0:4, colnames(children.south.NOgender))
abline(v=0:4, col = 'lightgrey')
points(0:4, children.south.NOgender[2,], type = 'o', col = 'gold', lwd = 2)
points(0:4, children.south.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(0:4, children.south.NOgender[4,], type = 'o', col = 'purple', lwd = 2)
if (k==4){ points(0:4, children.south.NOgender[5,], type = 'o', col = 'blue', lwd = 2)}
legend('bottomleft', legend = rownames(children.south.NOgender), 
       fill = c('red', 'gold', 'darkgreen', 'purple', 'blue'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


# COMMENT ......................................................................
#
#
# Almost ZERO differences among the countries
# (except for the "extreme" datum of 4 or more children,
#  which in my humble opinion is not enough)
# 
#                         => I WOULD NOT CONSIDER A COUNTRY-ONLY DIVISION HERE.
#
#



#### CHLD14: Difference between countries WITH GENDER ####


children.south.male <- children.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~chld14+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  children.south.male <- cbind(children.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~chld14+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  children.south.female <- cbind(children.south.female, tabnostd.female)
}
colnames(children.south.male) <- country
colnames(children.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(children.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('children ratio of men and women in South Europe')

par(mfrow = c(1,2))
plot(0:4, children.south.male[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=0:4, colnames(children.south.male))
abline(v=0:4, col = 'lightgrey')
points(0:4, children.south.male[2,], type = 'o', col = 'gold', lwd = 2)
points(0:4, children.south.male[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(0:4, children.south.male[4,], type = 'o', col = 'purple', lwd = 2)
if (k==4){points(0:4, children.south.male[5,], type = 'o', col = 'blue', lwd = 2)}
legend('bottomleft', legend = rownames(children.south.male), 
       fill = c('red', 'gold', 'darkgreen', 'purple', 'blue'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(0:4, children.south.female[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=0:4, colnames(children.south.female))
abline(v=0:k, col = 'lightgrey')
points(0:4, children.south.female[2,], type = 'o', col = 'gold', lwd = 2)
points(0:4, children.south.female[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(0:4, children.south.female[4,], type = 'o', col = 'purple', lwd = 2)
if (k==4){points(0:4, children.south.female[5,], type = 'o', col = 'blue', lwd = 2)}
legend('bottomleft', legend = rownames(children.south.female), 
       fill = c('red', 'gold', 'darkgreen', 'purple', 'blue'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# This plot confirms that a gender division is justified,
# but I still think that a country based division is not much informative.
#
# Moreover, I start to think that the 4 children category is not much reliable.
# 
#            => I WOULD NOT CONSIDER CHILDREN IN RELATION OF COUNTRY AND GENDER.
#
#



#### CHLD14: Difference between countries WITH citizenship ####

children.citizenship.NOgender <- NULL
for( ctz in citizenship){
  tabnostd <- svytable(~chld14+pdwrk, subset(design, ctzmod == ctz))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  children.citizenship.NOgender <- cbind(children.citizenship.NOgender, tabnostd)
}
colnames(children.citizenship.NOgender) <- citizenship

children.citizenship.male <- children.citizenship.female <- NULL
for( ctz in citizenship){
  tabnostd.male <- svytable(~chld14+pdwrk, subset(design, gndr == 'male' & ctzmod == ctz))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  children.citizenship.male <- cbind(children.citizenship.male, tabnostd.male)
  
  tabnostd.female <- svytable(~chld14+pdwrk, subset(design, gndr == 'female' & ctzmod == ctz))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  children.citizenship.female <- cbind(children.citizenship.female, tabnostd.female)
}
colnames(children.citizenship.male) <- citizenship
colnames(children.citizenship.female) <- citizenship

rm(tabnostd, tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(children.citizenship,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('children ratio of men and women in South Europe')

par(mfrow = c(1,3))
plot(1:3, children.citizenship.NOgender[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Whole population', side=3, line = 1)
axis(1, at=1:3, colnames(children.citizenship.NOgender))
abline(v=1:3, col = 'lightgrey')
points(1:3, children.citizenship.NOgender[2,], type = 'o', col = 'gold', lwd = 2)
points(1:3, children.citizenship.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(1:3, children.citizenship.NOgender[4,], type = 'o', col = 'purple', lwd = 2)
if (k==4){points(1:3, children.citizenship.NOgender[5,], type = 'o', col = 'blue', lwd = 2)}
legend('bottomleft', legend = rownames(children.citizenship.NOgender),
       fill = c('red', 'gold', 'darkgreen', 'purple'),
       border = NA,
       cex = 1.2,
       bty = 'n')
plot(1:3, children.citizenship.male[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:3, colnames(children.citizenship.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, children.citizenship.male[2,], type = 'o', col = 'gold', lwd = 2)
points(1:3, children.citizenship.male[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(1:3, children.citizenship.male[4,], type = 'o', col = 'purple', lwd = 2)
if (k==4){points(1:3, children.citizenship.male[5,], type = 'o', col = 'blue', lwd = 2)}
# legend('bottomleft', legend = rownames(children.citizenship.male), 
#        fill = c('red', 'gold', 'darkgreen', 'purple'), 
#        border = NA, 
#        cex = 1.2,
#        bty = 'n')
plot(1:3, children.citizenship.female[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:3, colnames(children.citizenship.female))
abline(v=1:3, col = 'lightgrey')
points(1:3, children.citizenship.female[2,], type = 'o', col = 'gold', lwd = 2)
points(1:3, children.citizenship.female[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(1:3, children.citizenship.female[4,], type = 'o', col = 'purple', lwd = 2)
if (k==4){points(1:3, children.citizenship.female[5,], type = 'o', col = 'blue', lwd = 2)}
# legend('bottomleft', legend = rownames(children.citizenship.female), 
#        fill = c('red', 'gold', 'darkgreen', 'purple'), 
#        border = NA, 
#        cex = 1.2,
#        bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# Same comments as always for the citizenship,
# which I start to think does not really interact with anything.
# 
#           => I WOULD CONSIDER CHILDREN IN RELATION OF GENDER AND citizenship.
#
#


#### CHLD14: Relationship with partner WITHOUT GENDER ####

children.partner.south.NOgender <- NULL
for( prt in 0:1){
  tabnostd <- svytable(~chld14+pdwrk, subset(design, prtnr == prt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  children.partner.south.NOgender <- cbind(children.partner.south.NOgender, tabnostd)
}
colnames(children.partner.south.NOgender) <- c('without partner', 'with partner')

rm(tabnostd)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(children.partner.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('children.partner ratio of men and women in South Europe')


par(mfrow = c(1,1))
plot(0:k, children.partner.south.NOgender[,1], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=0:k, rownames(children.partner.south.NOgender))
abline(v=0:k, col = 'lightgrey')
points(0:k, children.partner.south.NOgender[,2], type = 'o', col = 'darkgreen', lwd = 2)
legend('bottomleft', legend = colnames(children.partner.south.NOgender), 
       fill = c('red', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


# COMMENT ......................................................................
#
#
# If we don't consider the gender, having a partner makes no much difference.
# 
#                         => I WOULD NOT CONSIDER A PARTNER-ONLY DIVISION HERE.
#
#



#### CHLD14: Difference between countries WITH GENDER ####


children.partner.south.male <- children.partner.south.female <- NULL
for( prt in 0:1){
  tabnostd.male <- svytable(~chld14+pdwrk, subset(design, gndr == 'male' & prtnr == prt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  children.partner.south.male <- cbind(children.partner.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~chld14+pdwrk, subset(design, gndr == 'female' & prtnr == prt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  children.partner.south.female <- cbind(children.partner.south.female, tabnostd.female)
}
colnames(children.partner.south.male) <- c('without partner', 'with partner')
colnames(children.partner.south.female) <- c('without partner', 'with partner')

rm(tabnostd.male, tabnostd.female)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(children.partner.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('children.partner ratio of men and women in South Europe')

par(mfrow = c(1,2))
plot(0:k, children.partner.south.male[,1], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=0:k, rownames(children.partner.south.male))
abline(v=0:k, col = 'lightgrey')
points(0:k, children.partner.south.male[,2], type = 'o', col = 'darkgreen', lwd = 2)
legend('bottomleft', legend = colnames(children.partner.south.male), 
       fill = c('red', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
plot(0:k, children.partner.south.female[,1], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=0:k, rownames(children.partner.south.female))
abline(v=0:k, col = 'lightgrey')
points(0:k, children.partner.south.female[,2], type = 'o', col = 'darkgreen', lwd = 2)
legend('bottomleft', legend = colnames(children.partner.south.female), 
       fill = c('red', 'darkgreen'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


# COMMENT ......................................................................
#
#
# The presence of a partner clearly plays a role here in terms of gender difference.
# 
#            => I WOULD CONSIDER CHILDREN IN RELATION OF COUNTRY, PARTNER AND GENDER.
#               BUT HOW? SEPARATELY OR IN A NESTED WAY?
#
#

rm(cnt, ctz, prt)




