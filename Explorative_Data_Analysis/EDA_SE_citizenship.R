# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - CITIZENSHIP - - - - - - - - - - #

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
# title('South Europe')


#### Employment rate by gender in South Europe ####

# for each country:
country <- unique(work$cntry)
occupation.south.male.country <- occupation.south.female.country <- NULL
for (cnt in country){
  occupation.south.male.cnt.tab <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'male' & cntry == cnt))
  occupation.south.female.cnt.tab <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'female' & cntry == cnt))
  occupation.south.male.cnt <- occupation.south.male.cnt.tab[,2]/rowSums(occupation.south.male.cnt.tab)*100
  occupation.south.female.cnt <- occupation.south.female.cnt.tab[,2]/rowSums(occupation.south.female.cnt.tab)*100
  occupation.south.male.country <- rbind(occupation.south.male.country, occupation.south.male.cnt)
  occupation.south.female.country <- rbind(occupation.south.female.country, occupation.south.female.cnt)
}
rownames(occupation.south.male.country) <- country
rownames(occupation.south.female.country) <- country

# overall mean:
occupation.south.male.tab <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'male'))
occupation.south.female.tab <- svytable(~ctzmod+pdwrk, subset(design, gndr == 'female'))
occupation.south.male <- occupation.south.male.tab[,2]/rowSums(occupation.south.male.tab)*100
occupation.south.female <- occupation.south.female.tab[,2]/rowSums(occupation.south.female.tab)*100

rm(occupation.south.male.cnt.tab, occupation.south.female.cnt.tab, 
   occupation.south.male.cnt, occupation.south.female.cnt, 
   occupation.south.male.tab, occupation.south.female.tab)


par(mfrow = c(1,1))
plot(1:3, occupation.south.male, type = 'o', col = 'dodgerblue3', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
# mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:3, names(occupation.south.male))
abline(v=1:3, col = 'lightgrey')
points(1:3, occupation.south.female, type = 'o', col = 'red', lwd = 3)
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')




par(mfrow = c(1,1))
plot(1:3, occupation.south.male.country[1,], type = 'o', col = 'lightblue1', lwd = 2,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
points(1:3, occupation.south.female.country[1,], type = 'o', col = 'rosybrown1', lwd = 2)
for (i in 1:length(country)){
  points(1:3, occupation.south.male.country[i,], type = 'o', col = 'lightblue1', lwd = 2)
  points(1:3, occupation.south.female.country[i,], type = 'o', col = 'rosybrown1', lwd = 2)
}
points(1:3, occupation.south.male, type = 'o', col = 'dodgerblue3', lwd = 3)
points(1:3, occupation.south.female, type = 'o', col = 'red', lwd = 3)
# mtext('Occupation rate in South Europe', side=3, line = 1)
axis(1, at=1:3, names(occupation.south.male))
abline(v=1:3, col = 'lightgrey')
legend('bottomleft', legend = c('Men', 'Women'), 
       fill = c('dodgerblue3','red'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')


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

rm(cnt, i)
