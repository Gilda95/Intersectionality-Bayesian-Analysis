# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - ISCO08P - - - - - - - - - - #

# Does the type of job of the partner affect the employment rate?

#### load the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)

# Focus on south Europe
work <- work[work$rgn == 'South Europe', ]
work$rgn <- NULL

# Focus on people with a partner.working
work <- work[work$prtnr == 1 & work$pdwrkp == 1, ]
work$prtnr <- NULL
work$pdwrkp <- NULL

# Extract vector containing the countries labels
work$cntry <- as.factor(work$cntry)
work$cntry <- droplevels(work$cntry)
country <-  levels(work$cntry)

# Merge native and autochthonous
work$ctzmod[work$ctzmod == 'native'] <- 'autochthonous'

# Convert ctzmod in factor and drop levels
work$ctzmod <- as.factor(work$ctzmod)
work$ctzmod <- droplevels(work$ctzmod)

# Convert pdwrkp in factor and drop levels
work$pdwrkp <- as.factor(work$pdwrkp)

# Convert ctzmod in factor and drop levels
work$isco08p <- as.factor(work$isco08p)
work$isco08p <- droplevels(work$isco08p)

#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)


#### isco08p: Employment rate by gender in South Europe ####


# for each country:
partner.occupation.male.country <- partner.occupation.female.country <- NULL
country <- unique(work$cntry)
for( cnt in country){
  partner.occupation.male.cnt.tab <- svytable(~isco08p+pdwrk, subset(design, gndr == 'male' & cntry==cnt))
  partner.occupation.female.cnt.tab <- svytable(~isco08p+pdwrk, subset(design, gndr == 'female' & cntry==cnt))
  partner.occupation.male.cnt <- partner.occupation.male.cnt.tab[,2]/rowSums(partner.occupation.male.cnt.tab)*100
  partner.occupation.female.cnt <- partner.occupation.female.cnt.tab[,2]/rowSums(partner.occupation.female.cnt.tab)*100
  partner.occupation.male.country <- rbind(partner.occupation.male.country,partner.occupation.male.cnt)
  partner.occupation.female.country <- rbind(partner.occupation.female.country,partner.occupation.female.cnt)
}
rownames(partner.occupation.male.country) <- country
rownames(partner.occupation.female.country) <- country


# overall mean
partner.occupation.male.tab <- svytable(~isco08p+pdwrk, subset(design, gndr == 'male'))
partner.occupation.female.tab <- svytable(~isco08p+pdwrk, subset(design, gndr == 'female'))
partner.occupation.male <- partner.occupation.male.tab[,2]/rowSums(partner.occupation.male.tab)*100
partner.occupation.female <- partner.occupation.female.tab[,2]/rowSums(partner.occupation.female.tab)*100

rm(partner.occupation.male.tab, partner.occupation.female.tab)



# PLOT

par(mfrow = c(1,1))
plot(1:5, partner.occupation.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
# mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:5, names(partner.occupation.male), cex.axis = 0.8)
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.occupation.female, type = 'o', col = 'red', lwd = 3)
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')




par(mfrow = c(1,1))
plot(1:5, partner.occupation.male.country[1,], type = 'o', col = 'lightblue1', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
points(1:5, partner.occupation.female.country[1,], type = 'o', col = 'rosybrown1', lwd = 2)
for (i in 2:length(country)){
  points(1:5, partner.occupation.male.country[i,], type = 'o', col = 'lightblue1', lwd = 2)
  points(1:5, partner.occupation.female.country[i,], type = 'o', col = 'rosybrown1', lwd = 2)
}
points(1:5, partner.occupation.male, type = 'o', col = 'dodgerblue3', lwd = 3)
points(1:5, partner.occupation.female, type = 'o', col = 'red', lwd = 3)
axis(1, at=1:5, names(partner.occupation.male), cex.axis = 0.8)
abline(v=1:5, col = 'lightgrey')
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


#### isco08p: Difference between countries WITHOUT GENDER ####

partner.occupation.south.NOgender <- NULL
for( cnt in country){
  tabnostd <- svytable(~isco08p+pdwrk, subset(design, cntry == cnt))
  tabnostd <- tabnostd[,2]/rowSums(tabnostd)*100
  partner.occupation.south.NOgender <- cbind(partner.occupation.south.NOgender, tabnostd)
}
colnames(partner.occupation.south.NOgender) <- country

rm(tabnostd)



# PLOT

par(mfrow = c(1,1))
plot(1:5, partner.occupation.south.NOgender[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Occupation rate in South Europe by country', side=3, line = 1)
axis(1, at=1:5, colnames(partner.occupation.south.NOgender))
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.occupation.south.NOgender[2,], type = 'o', col = 'gold', lwd = 2)
points(1:5, partner.occupation.south.NOgender[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(1:5, partner.occupation.south.NOgender[4,], type = 'o', col = 'purple', lwd = 2)
points(1:5, partner.occupation.south.NOgender[5,], type = 'o', col = 'blue', lwd = 2)
legend('bottomleft', legend = rownames(partner.occupation.south.NOgender), 
       fill = c('red', 'gold', 'darkgreen', 'purple', 'blue'), 
       border = NA, 
       cex = 1,
       bty = 'n')


#### isco08p: Difference between countries WITH GENDER ####


partner.occupation.south.male <- partner.occupation.south.female <- NULL
for( cnt in country){
  tabnostd.male <- svytable(~isco08p+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  tabnostd.male <- tabnostd.male[,2]/rowSums(tabnostd.male)*100
  partner.occupation.south.male <- cbind(partner.occupation.south.male, tabnostd.male)
  
  tabnostd.female <- svytable(~isco08p+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  tabnostd.female <- tabnostd.female[,2]/rowSums(tabnostd.female)*100
  partner.occupation.south.female <- cbind(partner.occupation.south.female, tabnostd.female)
}
colnames(partner.occupation.south.male) <- country
colnames(partner.occupation.south.female) <- country

rm(tabnostd.male, tabnostd.female)



# PLOT

par(mfrow = c(1,2))
plot(1:5, partner.occupation.south.male[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('MEN', side=3, line = 1)
axis(1, at=1:5, colnames(partner.occupation.south.male))
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.occupation.south.male[2,], type = 'o', col = 'gold', lwd = 2)
points(1:5, partner.occupation.south.male[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(1:5, partner.occupation.south.male[4,], type = 'o', col = 'purple', lwd = 2)
points(1:5, partner.occupation.south.male[5,], type = 'o', col = 'blue', lwd = 2)
legend('bottomleft', legend = rownames(partner.occupation.south.male), 
       fill = c('red', 'gold', 'darkgreen', 'purple', 'blue'), 
       border = NA, 
       cex = 0.8,
       bty = 'n')
plot(1:5, partner.occupation.south.female[1,], type = 'o', col = 'red', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('WOMEN', side=3, line = 1)
axis(1, at=1:5, colnames(partner.occupation.south.female))
abline(v=1:5, col = 'lightgrey')
points(1:5, partner.occupation.south.female[2,], type = 'o', col = 'gold', lwd = 2)
points(1:5, partner.occupation.south.female[3,], type = 'o', col = 'darkgreen', lwd = 2)
points(1:5, partner.occupation.south.female[4,], type = 'o', col = 'purple', lwd = 2)
points(1:5, partner.occupation.south.female[5,], type = 'o', col = 'blue', lwd = 2)
legend('bottomleft', legend = rownames(partner.occupation.south.female), 
       fill = c('red', 'gold', 'darkgreen', 'purple', 'blue'), 
       border = NA, 
       cex = 0.8,
       bty = 'n')
mtext('Occupation rate in South Europe by country', outer = T, side=3, line = -1.5)


rm(cnt)
