# - - - - - - - - - - EXPLORATIVE DATA ANALYSIS - GENDER - - - - - - - - - - #

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


#### Define the weights design ####
library(survey)

design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, nest = T, 
                    data = work)



#### Employment rate by gender in South Europe ####

gender.south.tab <- svytable(~gndr+pdwrk, design)
gender.south <- gender.south.tab[,2]/rowSums(gender.south.tab)*100
# > gender.south
#   female     male 
# 66.59818 81.20397 

rm(gender.south.tab)


#### gndr: Difference between countries ####


gender.south.country <- NULL
for( cnt in country){
  tabnostd.country <- svytable(~gndr+pdwrk, subset(design, cntry == cnt))
  tabnostd.country <- tabnostd.country[,2]/rowSums(tabnostd.country)*100
  gender.south.country <- cbind(gender.south.country, tabnostd.country)
}

colnames(gender.south.country) <- country

rm(tabnostd.country)



# PLOT

# colors.south <- c('darkgreen','darkred', 'darkblue', 'darkorange2')
# barplot(gender.south,
#         ylim = c(0,100),
#         beside = T,
#         col = colors.south)
# title('gender ratio of men and women in South Europe')

par(mfrow = c(1,1))
plot(1:5, gender.south.country[1,], type = 'o', col = 'red', lwd = 3,
     ylim = c(0,100), xlab = '', ylab = '', xaxt = 'n')
mtext('Employment rate in South Europe by country', side=3, line = 1)
axis(1, at=1:5, colnames(gender.south.country))
abline(v=1:5, col = 'lightgrey')
points(1:5, gender.south.country[2,], type = 'o', col = 'dodgerblue3', lwd = 3)
legend('bottomleft', legend = rownames(gender.south.country), 
       fill = c('red', 'dodgerblue3'), 
       border = NA, 
       cex = 1.2,
       bty = 'n')

rm(cnt)



