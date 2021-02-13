# - - - - - - - - - - DATASET - - - - - - - - - - #

#### loading libraries ####

library(haven)

#### loading dataset ####

dataset <- read_sas('ess9e02.sas7bdat', NULL)


#### selecting variables of interest ####

# DEMOGRAPHIC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# idno
# agea: Age
# gndr: Gender
# cntry: Country
# ctzcntr: Citizen of country
# ctzshipd: Citizenship
# cntbrthd:	Country of birth
# brncntr:	Born in country
# blgetmg: Belong to minority ethnic group in country

# FAMILY & EDUCATION - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# isco08:	Occupation, ISCO08
# isco08p	Occupation partner, ISCO08
# domicil: Domicile, respondent's description
# nbthcld:	Number of children ever given birth to/ fathered
# fcldbrn:	Year (first) child was born
# eisced: Highest level of education, ES - ISCED
# edulvlb:	Highest level of education
# edulvlpb: Highest level of education for partner

# WORK - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# pdwrk:	Doing last 7 days: paid work
# fvgabc
# infqbst:	Your [pay/pensions/social benefits], which frequency do you know best
# grspnum: What is your usual [weekly/monthly/annual] gross pay
# grsplet:	Which letter describes your gross pay
# mnactic:	Main activity, last 7 days. All respondents. Post coded
# wkhct:	Total contracted hours per week in main job overtime excluded

# DATASET INFO - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# anweight:	Analysis weight
# stratum:	Sampling stratum
# psu	Primary: sampling unit

var.sel <- c('idno', 'agea', 'gndr', 'cntry', 'ctzcntr', 'ctzshipd', 'cntbrthd', 'brncntr', 'blgetmg',
             'isco08', 'isco08p', 'domicil', 'nbthcld', 'fcldbrn', 'edulvlb', 'edulvlpb', 'pdwrkp',
             'pdwrk', 'fvgabc', 'infqbst', 'grspnum', 'grsplet', 'mnactic', 'wkhct', 
             'anweight', 'psu', 'stratum')

data.sel <- dataset[, var.sel]

# selecting people with age between 25 and 55
data.sel <- data.sel[(data.sel$agea<=55) & (data.sel$agea>=25), ]

rm(var.sel)


#### uniforming 6, 7, 8, 9 (not answer and/or cannot answer)  ####
# 6, ... of ctzcntr
data.sel$ctzcntr[data.sel$ctzcntr == 6] <- 6666666666
data.sel$ctzcntr[data.sel$ctzcntr == 7] <- 7777777777
data.sel$ctzcntr[data.sel$ctzcntr == 8] <- 8888888888
data.sel$ctzcntr[data.sel$ctzcntr == 9] <- 9999999999

# 6666, ... of ctzshipd
data.sel$ctzshipd[data.sel$ctzshipd == 6666] <- 6666666666
data.sel$ctzshipd[data.sel$ctzshipd == 7777] <- 7777777777
data.sel$ctzshipd[data.sel$ctzshipd == 8888] <- 8888888888
data.sel$ctzshipd[data.sel$ctzshipd == 9999] <- 9999999999

# 6666, ... of cntbrthd
data.sel$cntbrthd[data.sel$cntbrthd == 6666] <- 6666666666
data.sel$cntbrthd[data.sel$cntbrthd == 7777] <- 7777777777
data.sel$cntbrthd[data.sel$cntbrthd == 8888] <- 8888888888
data.sel$cntbrthd[data.sel$cntbrthd == 9999] <- 9999999999

# 6, ... of brncntr
data.sel$brncntr[data.sel$brncntr == 6] <- 6666666666
data.sel$brncntr[data.sel$brncntr == 7] <- 7777777777
data.sel$brncntr[data.sel$brncntr == 8] <- 8888888888
data.sel$brncntr[data.sel$brncntr == 9] <- 9999999999

# 6, ... of blgetmg
data.sel$blgetmg[data.sel$blgetmg == 6] <- 6666666666
data.sel$blgetmg[data.sel$blgetmg == 7] <- 7777777777
data.sel$blgetmg[data.sel$blgetmg == 8] <- 8888888888
data.sel$blgetmg[data.sel$blgetmg == 9] <- 9999999999

# 6, ... of isco08
data.sel$isco08[data.sel$isco08 == 66666] <- 6666666666
data.sel$isco08[data.sel$isco08 == 77777] <- 7777777777
data.sel$isco08[data.sel$isco08 == 88888] <- 8888888888
data.sel$isco08[data.sel$isco08 == 99999] <- 9999999999

# 6, ... of isco08p
data.sel$isco08p[data.sel$isco08p == 66666] <- 6666666666
data.sel$isco08p[data.sel$isco08p == 77777] <- 7777777777
data.sel$isco08p[data.sel$isco08p == 88888] <- 8888888888
data.sel$isco08p[data.sel$isco08p == 99999] <- 9999999999

# 6, of domicil
data.sel$domicil[data.sel$domicil == 6] <- 6666666666
data.sel$domicil[data.sel$domicil == 7] <- 7777777777
data.sel$domicil[data.sel$domicil == 8] <- 8888888888
data.sel$domicil[data.sel$domicil == 9] <- 9999999999

# 6, ... of nbthcld
data.sel$nbthcld[data.sel$nbthcld == 66] <- 6666666666
data.sel$nbthcld[data.sel$nbthcld == 77] <- 7777777777
data.sel$nbthcld[data.sel$nbthcld == 88] <- 8888888888
data.sel$nbthcld[data.sel$nbthcld == 99] <- 9999999999

# 6666, ... of fcldbrn
data.sel$fcldbrn[data.sel$fcldbrn == 6666] <- 6666666666
data.sel$fcldbrn[data.sel$fcldbrn == 7777] <- 7777777777
data.sel$fcldbrn[data.sel$fcldbrn == 8888] <- 8888888888
data.sel$fcldbrn[data.sel$fcldbrn == 9999] <- 9999999999

# 6, ... of fvgabc  
data.sel$fvgabc[data.sel$fvgabc == 6] <- 6666666666
data.sel$fvgabc[data.sel$fvgabc == 7] <- 7777777777
data.sel$fvgabc[data.sel$fvgabc == 8] <- 8888888888
data.sel$fvgabc[data.sel$fvgabc == 9] <- 9999999999

# 6 of infqbst
data.sel$infqbst[data.sel$infqbst == 6] <- 6666666666
data.sel$infqbst[data.sel$infqbst == 7] <- 7777777777
data.sel$infqbst[data.sel$infqbst == 8] <- 8888888888
data.sel$infqbst[data.sel$infqbst == 9] <- 9999999999

# 666666666, ... of grspnum
data.sel$grspnum[data.sel$grspnum == 666666666] <- 6666666666
data.sel$grspnum[data.sel$grspnum == 777777777] <- 7777777777
data.sel$grspnum[data.sel$grspnum == 888888888] <- 8888888888
data.sel$grspnum[data.sel$grspnum == 999999999] <- 9999999999

# 66, ... of grsplet
data.sel$grsplet[data.sel$grsplet == 66] <- 6666666666
data.sel$grsplet[data.sel$grsplet == 77] <- 7777777777
data.sel$grsplet[data.sel$grsplet == 88] <- 8888888888
data.sel$grsplet[data.sel$grsplet == 99] <- 9999999999

# 66 of mnactic
data.sel$mnactic[data.sel$mnactic == 66] <- 6666666666
data.sel$mnactic[data.sel$mnactic == 77] <- 7777777777
data.sel$mnactic[data.sel$mnactic == 88] <- 8888888888
data.sel$mnactic[data.sel$mnactic == 99] <- 9999999999

# 6, ... of wkhct 
data.sel$wkhct[data.sel$wkhct == 666] <- 6666666666
data.sel$wkhct[data.sel$wkhct == 777] <- 7777777777
data.sel$wkhct[data.sel$wkhct == 888] <- 8888888888
data.sel$wkhct[data.sel$wkhct == 999] <- 9999999999


#### make income annual ####
cond <- (data.sel$grspnum != 6666666666) & (data.sel$grspnum != 7777777777) & 
  (data.sel$grspnum != 8888888888) & (data.sel$grspnum != 9999999999)
data.sel$grspnum[(data.sel$infqbst == 1) & cond] <- data.sel$grspnum[(data.sel$infqbst == 1) & cond]*52 # weekly
data.sel$grspnum[(data.sel$infqbst == 2) & cond] <- data.sel$grspnum[(data.sel$infqbst == 2) & cond]*12 # monthly

#### convert income in euro ####
curr.cz <- (25.855 + 25.701 + 25.477) / 3               # mean of the currency exchange in 2018
curr.pl <- mean(4.3060, 4.3029, 4.2905, 4.2954, 4.3157) # mean of the currency exchange in 2018
curr.rs <- 100/0.87                                     # mean of the currency exchange in 2018
data.sel$grspnum[(data.sel$cntry == 'BG') & cond] <- data.sel$grspnum[(data.sel$cntry == 'BG') & cond]/1.95583  # BULGARIA BG
data.sel$grspnum[(data.sel$cntry == 'HR') & cond] <- data.sel$grspnum[(data.sel$cntry == 'HR') & cond]/7.44     # CROATIA HR
data.sel$grspnum[(data.sel$cntry == 'CZ') & cond] <- data.sel$grspnum[(data.sel$cntry == 'CZ') & cond]/curr.cz  # CZECHIA CZ
data.sel$grspnum[(data.sel$cntry == 'HU') & cond] <- data.sel$grspnum[(data.sel$cntry == 'HU') & cond]/322      # HUNGARY HU
data.sel$grspnum[(data.sel$cntry == 'NO') & cond] <- data.sel$grspnum[(data.sel$cntry == 'NO') & cond]/9.63     # NORWAY NO
data.sel$grspnum[(data.sel$cntry == 'PL') & cond] <- data.sel$grspnum[(data.sel$cntry == 'PL') & cond]/curr.pl  # POLAND PL
data.sel$grspnum[(data.sel$cntry == 'RS') & cond] <- data.sel$grspnum[(data.sel$cntry == 'RS') & cond]/curr.rs  # SERBIA RS
data.sel$grspnum[(data.sel$cntry == 'SE') & cond] <- data.sel$grspnum[(data.sel$cntry == 'SE') & cond]/10.17    # SWEDEN SE
data.sel$grspnum[(data.sel$cntry == 'CH') & cond] <- data.sel$grspnum[(data.sel$cntry == 'CH') & cond]/1.14     # SWITZERLAND CH
data.sel$grspnum[(data.sel$cntry == 'GB') & cond] <- data.sel$grspnum[(data.sel$cntry == 'GB') & cond]/1.12     # UNITED KINDOM GB

rm(curr.cz, curr.pl, curr.rs, cond)

#### renominating fvgabc ####
data.sel$fvgabc[data.sel$fvgabc == 1] <- 'Pay'
data.sel$fvgabc[data.sel$fvgabc == 2] <- 'Pension'
data.sel$fvgabc[data.sel$fvgabc == 3] <- 'Social benefits'

#### renominating gender #####
data.sel$gndr[data.sel$gndr == 1] <- 'male'
data.sel$gndr[data.sel$gndr == 2] <- 'female'

#### aggregating isco08 ####
for (i in 0:9) {
  data.sel$isco08[(data.sel$isco08 >= i*1000) & (data.sel$isco08 < (i+1)*1000)] <- i
}

data.sel$isco08[data.sel$isco08 == 0] <- 'military'
data.sel$isco08[data.sel$isco08 == 1] <- 'manager'
data.sel$isco08[data.sel$isco08 == 2] <- 'professionals and employees'
data.sel$isco08[data.sel$isco08 == 3] <- 'professionals and employees'
data.sel$isco08[data.sel$isco08 == 4] <- 'professionals and employees'
data.sel$isco08[data.sel$isco08 == 5] <- 'services and sales workers'
data.sel$isco08[data.sel$isco08 == 6] <- 'skilled manual workers'
data.sel$isco08[data.sel$isco08 == 7] <- 'skilled manual workers'
data.sel$isco08[data.sel$isco08 == 8] <- 'skilled manual workers'
data.sel$isco08[data.sel$isco08 == 9] <- 'elementary occupations'

#### aggregating isco08p ####
for (i in 0:9) {
  data.sel$isco08p[(data.sel$isco08p >= i*1000) & (data.sel$isco08p < (i+1)*1000)] <- i
}

data.sel$isco08p[data.sel$isco08p == 0] <- 'military'
data.sel$isco08p[data.sel$isco08p == 1] <- 'manager'
data.sel$isco08p[data.sel$isco08p == 2] <- 'professionals and employees'
data.sel$isco08p[data.sel$isco08p == 3] <- 'professionals and employees'
data.sel$isco08p[data.sel$isco08p == 4] <- 'professionals and employees'
data.sel$isco08p[data.sel$isco08p == 5] <- 'services and sales workers'
data.sel$isco08p[data.sel$isco08p == 6] <- 'skilled manual workers'
data.sel$isco08p[data.sel$isco08p == 7] <- 'skilled manual workers'
data.sel$isco08p[data.sel$isco08p == 8] <- 'skilled manual workers'
data.sel$isco08p[data.sel$isco08p == 9] <- 'elementary occupations'


#### renominating ethnic minorities ####
data.sel$blgetmg[data.sel$blgetmg == 1] <- 'yes'
data.sel$blgetmg[data.sel$blgetmg == 2] <- 'no'


#### creating variable for citizenship ####
data.sel$ctzmod <- NA
data.sel$ctzmod[(data.sel$ctzcntr == 1) & (data.sel$brncntr == 1) & (data.sel$blgetmg == 'no')] <- 'autochthonous'
data.sel$ctzmod[(data.sel$ctzcntr == 1) & (data.sel$brncntr == 2) & (data.sel$blgetmg == 'no')] <- 'native'
data.sel$ctzmod[((data.sel$ctzcntr == 2) & (data.sel$brncntr == 1)) | ((data.sel$ctzcntr == 1) & (data.sel$blgetmg == 'yes'))] <- 'ethnic minorities'
data.sel$ctzmod[(data.sel$ctzcntr == 2) & (data.sel$brncntr == 2)] <- 'immigrant'


#### creating variable for born country ####
data.sel$brnmod <- NA
data.sel$cntbrthd[data.sel$brncntr == 1] <- data.sel$cntry[data.sel$brncntr == 1] 

# aggregating country of origin
rich <- c('AU', '53', 'AT', 'BE', 'CA', 'CL', 'CO', 'CZ', 'DK', 'EE', 'FI', 'FR', 
          'DE', 'GR', 'HU', 'IS', 'IE', 'IL', 'IT', 'JP', 'KP', 'KR', 'LV', 'LT', 
          'LU', 'MX', 'NL', 'NZ', 'NO', 'PL', 'PT', 'SK', 'SI', 'ES', 'SE', 'CH', 
          'TR', 'GB', 'US', 'MC', '1000', '3000')
africa <- c("11", "14", "15", "17", "18", "2", "202", "AO", "BF", "BI", "BJ", "BW", 
            "CD", "CF", "CG", "CI", "CM", "CV", "DJ", "DZ", "EG", "EH", "ER", "ET", 
            "GA", "GH", "GM", "GN", "GQ", "GW", "KE", "KM", "LR", "LS", "LY", "MA", 
            "MG", "ML", "MR", "MU", "MW", "MZ", "NA", "NE", "NG", "RE", "RW", "SC", 
            "SD", "SH", "SL", "SN", "SO", "SS", "ST", "SZ", "TD", "TG", "TN", "TZ", 
            "UG", "YT", "ZA", "ZM", "ZW")
america <- c("13", "19", "21", "29", "419", "5", "AG", "AI", "AR", "AW", "BB", "BL", 
             "BM", "BO", "BQ", "BR", "BS", "BZ", "CR", "CU", "CW", "DM", "DO", "EC", 
             "FK", "GD", "GF", "GL", "GP", "GT", "GY", "HN", "JM", "KN", "KY", "LC", 
             "MF", "MQ", "MS", "NI", "PA", "PE", "PM", "PR", "PY", "SR", "SV", "SX", 
             "TC", "TT", "UM", "UY", "VC", "VE", "VG", "VI")
asia <- c("142", "143", "145", "30", "34", "35", "5000", "AE", "AF", "AM", "BD", 
          "BH", "BN", "BT", "CC", "CN", "CX", "HK", "HT", "ID", "IN", "IO", "IQ", 
          "IR", "JO", "KG", "KH", "KW", "LA", "LB", "LK", "MM", "MN", "MO", "MV", 
          "MY", "NP", "OM", "PH", "PK", "PS", "QA", "SA", "SG", "SY", "TH", "TJ", 
          "TL", "TM", "TW", "UZ", "VN", "YE")
europe <- c("150", "151", "154", "155", "39", "4000", "6000", "AD", "AL", "AX", "BA", 
            "BG", "BY", "CY", "FO", "GG", "GI", "HR", "IM", "JE", "LI", "MD", "ME", 
            "MK", "MT", "RO", "RS", "SJ", "SM", "UA", "VA", "XK")
oceania <- c("54", "57", "61", "9", "AS", "CK", "FJ", "FM", "GU", "KI", "MH", "MP", "NC", 
             "NF", "NR", "NU", "PF", "PG", "PN", "PW", "SB", "TK", "TO", "TW", "VU", "WF", 
             "WS")
antartica <- c('AQ', 'GS', 'HM', 'TF')

for (i in rich) {
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'Rich'
}
for (i in africa){
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'Africa'
}
for (i in america){
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'America'
}
for (i in asia){
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'Asia'
}
for ( i in europe){
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'Europe'
}
for (i in oceania){
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'Oceania'
}
for (i in antartica){
  data.sel$brnmod[data.sel$cntbrthd == i] <- 'Antartica'
}

# table(data.sel$brnmod)
# Africa   America    Asia  Europe   Oceania    Rich 
#    348       184     328    3941         4   16794 
#
# Note that there are only 4 people from Oceania and 0 from Antardide

data.sel$ctzcntr <- data.sel$ctzshipd <- data.sel$cntbrthd <- data.sel$brncntr <- NULL

rm(africa, america, antartica, asia, europe, oceania, rich)


#### renominating domicil #####
data.sel$domicil[data.sel$domicil == 1] <- 'Big City'
data.sel$domicil[data.sel$domicil == 2] <- 'Suburbs or Outskirts of Big City'
data.sel$domicil[data.sel$domicil == 3] <- 'Small City'
data.sel$domicil[data.sel$domicil == 4] <- 'Country Village'
data.sel$domicil[data.sel$domicil == 5] <- 'Farm or Countryside'

# table(data.sel$domicil)
#  Big City      Country Village       Farm or Countryside         Small City         Suburbs or Outskirts of Big City
#      4729                 6795                      1050               6810                                     2415 



#### creating variable for educazione ####

# for the subject
data.sel$edumod <- NA
for (i in 0:8) {
  data.sel$edumod[(data.sel$edulvlb >= i*100) & (data.sel$edulvlb < (i+1)*100)] <- i
}

data.sel$edumod[data.sel$edulvlb == 7777] <- NA
data.sel$edumod[data.sel$edulvlb == 5555] <- NA
data.sel$edumod[data.sel$edulvlb == 8888] <- NA
data.sel$edumod[data.sel$edulvlb == 9999] <- NA

data.sel$edulvlb <- NULL

# for the partner
data.sel$edupmod <- NA
for (i in 0:8) {
  data.sel$edupmod[(data.sel$edulvlpb >= i*100) & (data.sel$edulvlpb < (i+1)*100)] <- i
}

data.sel$edupmod[data.sel$edulvlpb == 5555] <- NA
data.sel$edupmod[data.sel$edulvlpb == 6666] <- 6666666666
data.sel$edupmod[data.sel$edulvlpb == 7777] <- NA
data.sel$edupmod[data.sel$edulvlpb == 8888] <- NA
data.sel$edupmod[data.sel$edulvlpb == 9999] <- NA

data.sel$edulvlpb <- NULL

# table(data.sel$edupmod)
# 0          1          2          3          4          5          6          7          8 
# 87        372       1530       6020        990        936       1899       2219        204


#### creating variable for children ####

dataset1 <- dataset[(dataset$agea<=55) & (dataset$agea>=25), ]
data.eta <- dataset1[,c('yrbrn2', 'yrbrn3', 'yrbrn4', 'yrbrn5', 'yrbrn6', 'yrbrn7', 'yrbrn8', 
                       'yrbrn9', 'yrbrn10', 'yrbrn11', 'yrbrn12', 'yrbrn13', 'yrbrn14', 'yrbrn15')]
data.rel <- dataset1[,c('rshipa2', 'rshipa3', 'rshipa4', 'rshipa5', 'rshipa6', 'rshipa7', 'rshipa8', 
                       'rshipa9', 'rshipa10', 'rshipa11', 'rshipa12', 'rshipa13', 'rshipa14', 'rshipa15')]

data.year <- dataset1$inwyys

data.sel$chld3 <- NA      # number of children under 3 years old 
data.sel$chld10 <- NA     # number of children under 10 years old 
data.sel$chld14 <- NA     # number of children under 14 years old

for (i in 1:dim(dataset1)[1]) {
  n3 <- 0
  n10 <- 0
  n14 <- 0
  for (j in 1:dim(data.rel)[2]) {
    if ( !is.na(data.eta[i,j]) & !is.na(data.rel[i,j]) ) {
      if ( (data.rel[i,j] == 2) & ((data.year[i] - data.eta[i,j]) <= 3) ) {
        n3 <- n3 + 1
      }
      if ( (data.rel[i,j] == 2) & ((data.year[i] - data.eta[i,j]) <= 10) ) {
        n10 <- n10 + 1
      }
      if ( (data.rel[i,j] == 2) & ((data.year[i] - data.eta[i,j]) <= 14) ) {
        n14 <- n14 + 1
      }
    }
  }
  data.sel$chld3[i] <- n3
  data.sel$chld10[i] <- n10
  data.sel$chld14[i] <- n14
}

rm(n3, n10, n14, i, j, dataset1, data.rel, data.eta, data.year)

#### creating binary variable for partner/no partner ####

dataset1 <- dataset[(dataset$agea<=55) & (dataset$agea>=25), ]
part.cont <- dataset1[,c('rshipa2', 'rshipa3', 'rshipa4', 'rshipa5', 'rshipa6', 'rshipa7', 'rshipa8', 
                         'rshipa9', 'rshipa10', 'rshipa11', 'rshipa12', 'rshipa13', 'rshipa14', 'rshipa15')]

data.sel$prtnr <- 0

for (i in 1:dim(part.cont)[1]) {
  for (j in 1:dim(part.cont)[2]) {
    if(part.cont[i,j] == 1 & !is.na(part.cont[i,j])){
      data.sel$prtnr[i] <- 1
    }
  }
}

rm(i, j, dataset1, part.cont)


#### excluding subjects ####

# eliminating low numerosity categories and not answered 

# military from isco08 and isco08p
data.sel <- data.sel[data.sel$isco08 != 'military',]
data.sel <- data.sel[data.sel$isco08p != 'military',]

# oceania from brnmod
data.sel <- data.sel[data.sel$brnmod != 'Oceania',]

# 666666666 from education of partner partner
data.sel <- data.sel[(data.sel$prtnr == 0) | (data.sel$prtnr == 1 & data.sel$edupmod != 6666666666), ]


#### creating a variable for region ####
data.exp <- na.omit(data.exp)

OVEST = c( "AT" , "BE", "CH",  "DE" ,  "FR" , "NL")
EST = c("EE", "LT" , "LV" ,"BG", "CZ" , "HU" , "PL" , "SK")
NORD = c( "FI" , "GB"  , "IE","NO" , "SE")
SUD = c("CY", "ES" ,"IT", "PT", "SI" )
BALK = c("ME" , "HR", "RS")

data.sel$rgn <- NA
for( i in 1:dim(data.sel)[1] ){
  if(is.na(data.sel$cntry[i]))
    next
  
  for( est in EST ){
    if( data.sel$cntry[i] == est ){
      data.sel$rgn[i] <- 'East Europe'
      break
    }
  }
  
  for( ovest in OVEST ){
    if( data.sel$cntry[i] == ovest ){
      data.sel$rgn[i] <- 'West Europe'
      break
    }
  }
  
  for( nord in NORD ){
    if( data.sel$cntry[i] == nord ){
      data.sel$rgn[i] <- 'North Europe'
      break
    }
  }
  
  for( sud in SUD ){
    if( data.sel$cntry[i] == sud ){
      data.sel$rgn[i] <- 'South Europe'
      break
    }
  }
  
  for( balk in BALK ){
    if( data.sel$cntry[i] == balk ){
      data.sel$rgn[i] <- 'Balkans'
      break
    }
  }
  
}

#### creating a new variable for education with only 3 levels ####

# for subject
data.sel$edutre <- NULL
data.sel$edutre[data.sel$edumod  == 0 | data.sel$edumod  == 1 | data.sel$edumod  == 2 ] <- 1
data.sel$edutre[data.sel$edumod  == 3 | data.sel$edumod  == 4] <- 2
data.sel$edutre[data.sel$edumod  == 5 | data.sel$edumod  == 6 | data.sel$edumod  == 7 | data.sel$edumod  == 8 ] <- 3

# for partner
data.sel$eduptre <- NULL
data.sel$eduptre[data.sel$edupmod  == 0 | data.sel$edupmod  == 1 | data.sel$edupmod  == 2 ] <- 1
data.sel$eduptre[data.sel$edupmod  == 3 | data.sel$edupmod  == 4] <- 2
data.sel$eduptre[data.sel$edupmod  == 5 | data.sel$edupmod  == 6 | data.sel$edupmod  == 7 | data.sel$edupmod  == 8 ] <- 3
data.sel$eduptre[data.sel$edupmod  == 6666666666] <- 6666666666


#### creating a column for homogamy  ####
data.sel$omogamia <- NULL
pippo <- data.sel$edutre - data.sel$eduptre
data.sel$omogamia[pippo == 0] <- 1 
data.sel$omogamia[pippo != 0] <- 0
data.sel$omogamia[abs(pippo) > 10] <- 6666666666


#### export data for the analysis ####

var.sel1 <- c('gndr', 'agea', 'cntry', 'rgn' ,  'ctzmod', 'brnmod', 'domicil', 'prtnr', 'pdwrkp',
              'isco08p', 'edumod', 'edutre', 'edupmod', 'eduptre' , 'omogamia' , 'chld3', 'chld10', 'chld14', 
              'anweight', 'psu', 'stratum', 'pdwrk')

data.exp <- data.sel[, var.sel1]
data.exp[data.exp == 7777777777] <- NA
data.exp[data.exp == 8888888888] <- NA
data.exp[data.exp == 9999999999] <- NA
data.exp <- na.omit(data.exp)


write.csv(data.exp, file = "Data_Cleaning/data_work.csv", row.names = FALSE)






