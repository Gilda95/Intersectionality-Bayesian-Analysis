# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - AGE - - - - - - - - - - #

# Does the respondent age affect his employment chances?

#### load the dataset ####

work <- read.csv('Data_Cleaning/data_work.csv', header = T)

# Focus on south Europe
work <- work[work$rgn == 'South Europe', ]
work$rgn <- NULL

# Extract vector containing the countries labels
work$cntry <- as.factor(work$cntry)
work$cntry <- droplevels(work$cntry)
country <-  levels(work$cntry)


#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, weights = ~anweight, nest = T, 
                    data = work)



#### Overall age distribution ####

par(mfrow = c(1,2))
svyhist(~agea, design, col = 'gold')
svyhist(~agea, subset(design, gndr == 'male'), col = rgb(0,0,1,0.2))
svyhist(~agea, subset(design, gndr == 'female'), col = rgb(1,0,0,0.2), add = T)

# par(mfrow = c(1,1))
# svyhist(~agea, design, col = 'gold', main = '', xlab = 'age', ylab = '')


#### Age in relation with employment status and gender ####

age.temp <- svytable(~agea+pdwrk, design = design)
age.male.temp <- svytable(~agea+pdwrk, design = subset(design, gndr == 'male'))
age.female.temp <- svytable(~agea+pdwrk, design = subset(design, gndr == 'female'))

# Group people in 5 years spans
# (group 1: 25-29, group 2: 30-34 ...)
age <- age.male <- age.female <- matrix(NA, nrow = 6, ncol = 2)
for(i in 1:6){
  
  age[i,] <- colSums(age.temp[(5*i-4):(5*i),])
  age.male[i,] <- colSums(age.male.temp[(5*i-4):(5*i),])
  age.female[i,] <- colSums(age.female.temp[(5*i-4):(5*i),])

}
age[6,] <- age[6,] + age.temp[dim(age.temp)[1],]
age.male[6,] <- age.male[6,] + age.male.temp[dim(age.male.temp)[1],]
age.female[6,] <- age.female[6,] + age.female.temp[dim(age.female.temp)[1],]


# Compute percentage of employed people for each age
age <- age[,2]/rowSums(age) * 100
age.male <- age.male[,2]/rowSums(age.male) * 100
age.female <- age.female[,2]/rowSums(age.female) * 100

rm(age.temp, age.male.temp, age.female.temp)



# PLOT

par(mfrow = c(1,3), oma=c(0,0,2,0))
barplot(age,
        col = 'gold',
        ylim = c(0,100),
        names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
        las = 2)
mtext('Overall population', side=3, line = 1)
barplot(age.male,
        col = 'dodgerblue3',
        ylim = c(0,100),
        names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
        las = 2)
mtext('Men', side=3, line = 1)
barplot(age.female,
        col = 'red',
        ylim = c(0,100),
        names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
        las = 2)
mtext('Women', side=3, line = 1)
mtext('Occupation rate in South Europe by age', outer = T, side=3, line = -1)


#### Age in relation with employment status and gender for each country ####

age.south <- age.south.male <- age.south.female <- NULL
for( cnt in country){
  
  age.temp <- svytable(~agea+pdwrk, design = subset(design, cntry == cnt))
  age.male.temp <- svytable(~agea+pdwrk, design = subset(design, gndr == 'male' & cntry == cnt))
  age.female.temp <- svytable(~agea+pdwrk, design = subset(design, gndr == 'female' & cntry == cnt))
  
  age.south <- cbind(age.south, age.temp)
  age.south.male <- cbind(age.south.male, age.male.temp)
  age.south.female <- cbind(age.south.female, age.female.temp)
}


# Group people in 5 years spans
# (group 1: 25-29, group 2: 30-34 ...)
age.temp <- age.south
age.male.temp <- age.south.male
age.female.temp <- age.south.female
age.south <- age.south.male <- age.south.female <- matrix(NA, nrow = 6, ncol = dim(age.temp)[2])
for(i in 1:6){
  
  age.south[i,] <- colSums(age.temp[(5*i-4):(5*i),])
  age.south.male[i,] <- colSums(age.male.temp[(5*i-4):(5*i),])
  age.south.female[i,] <- colSums(age.female.temp[(5*i-4):(5*i),])
  
}
age.south[6,] <- age.south[6,] + age.temp[dim(age.temp)[1],]
age.south.male[6,] <- age.south.male[6,] + age.male.temp[dim(age.male.temp)[1],]
age.south.female[6,] <- age.south.female[6,] + age.female.temp[dim(age.female.temp)[1],]


# Pass to percentages
age.temp <- age.south
age.male.temp <- age.south.male
age.female.temp <- age.south.female
age.south <- age.south.male <- age.south.female <- matrix(NA, nrow = 6, ncol = dim(age.temp)[2]/2)
for(i in 1:5){
  
  age.south[,i] <- age.temp[,2*i] / rowSums(age.temp[,(2*i-1):(2*i)])*100
  age.south.male[,i] <- age.male.temp[,2*i] / rowSums(age.male.temp[,(2*i-1):(2*i)])*100
  age.south.female[,i] <- age.female.temp[,2*i] / rowSums(age.female.temp[,(2*i-1):(2*i)])*100
  
}

colnames(age.south) <- country
colnames(age.south.male) <- country
colnames(age.south.female) <- country


par(mfrow = c(2,3))
for(i in 1:length(country)){
  barplot(age.south[,i],
          col = 'gold',
          ylim = c(0,100),
          names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
          las = 2)
  mtext(country[i], side=3, line = 1)
  barplot(age.south.male[,i],
          col = 'dodgerblue3',
          ylim = c(0,100),
          names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
          las = 2)
  mtext(paste(country[i], 'men'), side=3, line = 1)
  barplot(age.south.female[,i],
          col = 'red',
          ylim = c(0,100),
          names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
          las = 2)
  mtext(paste(country[i], 'women'), side=3, line = 1)
}


#### A final look to women ####

age.female.temp <- svytable(~agea+pdwrk, design = subset(design, gndr == 'female'))
age.female.tot <- age.female.temp[,2]/rowSums(age.female.temp) * 100

par(mfrow =c(1,2))
barplot(age.female.tot,
        col = 'red',
        ylim = c(0,100),
        # names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
        las = 2)
barplot(age.female,
        col = 'red',
        ylim = c(0,100),
        names.arg = c('25-29','30-34','35-39','40-44','45-49','50-55'),
        las = 2)


rm(age.temp, age.male.temp, age.female.temp, i, cnt)

