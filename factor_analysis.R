library(data.table)
library(psych)
library(GPArotation)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sona = read.csv("sona_data_screened.csv")
mturk = read.csv("mturk_data_screened.csv")

##split the data and recombine
sona$where = "sona"
mturk$where = "mturk"

#deal with rounding
marker = nrow(sona)/2
marker2 = nrow(mturk)/2

#data
efadata = rbindlist(list(sona[1:marker , ], 
                         mturk[1:round(marker2), ]), 
                    fill = TRUE)
  
cfadata = rbindlist(list(sona[marker+1:nrow(sona) , ], 
                         mturk[round(marker2)+1:nrow(mturk), ]),
                    fill = TRUE)

#questions are columns 37:56, 58:76, 78:92, 94:97
efaquestions = efadata[, c(37:56, 58:76, 78:92, 94:97)]


#### PRE-EFA ####

### Bartlett's
correlations = cor(efaquestions)
cortest.bartlett(correlations, n = nrow(efaquestions))
#correlations are large enough to run efa, p < .001

### KMO
KMO(correlations)
#our overall sample size is good, MSA = 0.94
#possible issues: Q2_13 (0.78), Q5_11 (0.68), Q5_20 (0.71)

### Number of factors
nofactors = fa.parallel(efaquestions, fm="ml", fa="fa")
nofactors$fa.values
sum(nofactors$fa.values > 1.0) ##old kaiser criterion = 5
sum(nofactors$fa.values > .7) ##new kaiser criterion = 6
#parallel analysis = 6
#scree = 3 or 4

### FIVE-FACTOR EFA ###

## Simple Structure ##

fa(efaquestions, nfactors=5, rotate = "oblimin", fm = "ml")
#Item 24 had no loadings
#Items 2, 14, 30, 42, 47, 48, and 52 had multiple loadings

fa(efaquestions[ , -c(2,14,24,30,42,47,48,52)], nfactors=5, rotate = "oblimin", fm = "ml")
#Item 53 had no loadings
#Item 31 had multiple loadings

fa(efaquestions[ , -c(2,14,24,30,42,47,48,52,53,31)], 
   nfactors=5, rotate = "oblimin", fm = "ml")
#Items 1, 6 had multiple loadings

fa(efaquestions[ , -c(2,14,24,30,42,47,48,52,53,31,1,6)], 
   nfactors=5, rotate = "oblimin", fm = "ml")
#Items 23, 29 had multiple loadings

fa(efaquestions[ , -c(2,14,24,30,42,47,48,52,53,31,1,6,23,29)], 
   nfactors=5, rotate = "oblimin", fm = "ml")
#Item 5 had multiple loadings

fa(efaquestions[ , -c(2,14,24,30,42,47,48,52,53,31,1,6,23,29,5)], 
   nfactors=5, rotate = "oblimin", fm = "ml")
#We have achieved simple structure!
#Eliminating items 1,2,5,6,14,23,24,29,30,31,42,47,48,52,53

five_factor1 = efaquestions[,c(11,12,17,18,33,34,37,39:41,49,57)]
five_factor2 = efaquestions[,c(7:9,19:22,26,32,35,43,54:56)]
five_factor3 = efaquestions[,c(3,4,10,15,16,46)]
five_factor4 = efaquestions[,c(13,25,27,28,36,38,45,51)]
five_factor5 = efaquestions[,c(44,50,58)]

## Adequate Solution ##

#CFI = .8965
five_finalmodel = fa(efaquestions[ , -c(2,14,24,30,42,47,48,52,53,31,1,6,23,29,5)], 
                nfactors=5, rotate = "oblimin", fm = "ml")
1 - ((five_finalmodel$STATISTIC-five_finalmodel$dof)/
       (five_finalmodel$null.chisq-five_finalmodel$null.dof))

library(knitr)
tableprint = matrix(NA, nrow = 4, ncol = 3)

tableprint[1, ] = c("RMSEA", "0.06, 90% CI[0.053, 0.061]", "Acceptable to Good")
tableprint[2, ] = c("RMSR", 0.04, "Good")
tableprint[3, ] = c("CFI", 0.897, "Poor")
tableprint[4, ] = c("TLI", 0.864, "Poor")

kable(tableprint, 
      digits = 3,
      col.names = c("Fit Index", "Value", "Description"))

#Reliabilities
psych::alpha(five_factor1, check.keys = T) # .94 - great
psych::alpha(five_factor2, check.keys = T) # .89 - good
psych::alpha(five_factor3, check.keys = T) # .87 - good
psych::alpha(five_factor4, check.keys = T) # .74 - acceptable
psych::alpha(five_factor5, check.keys = T) # .66 - eek


### FOUR-FACTOR EFA ###

## Simple Structure ##

fa(efaquestions, nfactors=4, rotate = "oblimin", fm = "ml")
#Items 1,2,6,10,14,30,31,46,48,52 had multiple loadings

fa(efaquestions[ , -c(1,2,6,10,14,30,31,46,48,52)], 
   nfactors=4, rotate = "oblimin", fm = "ml")
#Items 24,25,42,47 had multiple loadings
#Item 56 had no loadings

fa(efaquestions[ , -c(1,2,6,10,14,24,25,30,31,42,46,47,48,52,56)], 
   nfactors=4, rotate = "oblimin", fm = "ml")
#Items 5,21,22,23,57 had multiple loadings
#Items 32,43,50 had no loadings

fa(efaquestions[ , -c(1,2,5,6,10,14,21,22,23,24,25,30,31,32,42,43,46,47,48,50,52,56,57)], 
   nfactors=4, rotate = "oblimin", fm = "ml")
#Item 44 had no loadings

fa(efaquestions[ , -c(1,2,5,6,10,14,21,22,23,24,25,30,31,32,42,43,44,46,47,48,50,52,56,57)], 
   nfactors=4, rotate = "oblimin", fm = "ml")
#Item 58 had no loadings

fa(efaquestions[ , -c(1,2,5,6,10,14,21,22,23,24,25,30,31,32,42,43,44,46,47,48,50,52,56,57,58)], 
   nfactors=4, rotate = "oblimin", fm = "ml")
#We have achieved simple structure!
#Eliminating items 1,2,5,6,10,14,21,22,23,24,25,30,31,32,42,43,44,46,47,48,50,52,56,57,58

four_factor1 = efaquestions[,c(11,12,17,18,33,34,37,39:41,49)]
four_factor2 = efaquestions[,c(7:9,19,20,26,35,54,55)]
four_factor3 = efaquestions[,c(3,4,15,16,29)]
four_factor4 = efaquestions[,c(13,27,28,36,38,45,51,53)]

FF1 = c(11,12,17,18,33,34,37,39:41,49)
FF2 = c(7:9,19,20,26,35,54,55)
FF3 = c(3,4,15,16,29)

## Adequate Solution ##

#CFI = 0.914
four_finalmodel = fa(efaquestions[ , -c(1,2,5,6,10,14,21,22,23,24,25,30,31,32,42,
                                        43,44,46,47,48,50,52,56,57,58)], 
                     nfactors=4, rotate = "oblimin", fm = "ml")
1 - ((four_finalmodel$STATISTIC-four_finalmodel$dof)/
       (four_finalmodel$null.chisq-four_finalmodel$null.dof))

tableprint = matrix(NA, nrow = 4, ncol = 3)

tableprint[1, ] = c("RMSEA", "0.064, 90% CI[0.056, 0.067]", "Acceptable")
tableprint[2, ] = c("RMSR", 0.04, "Good")
tableprint[3, ] = c("CFI", 0.914, "Acceptable")
tableprint[4, ] = c("TLI", 0.885, "Poor")

kable(tableprint, 
      digits = 3,
      col.names = c("Fit Index", "Value", "Description"))

#Reliabilities
psych::alpha(four_factor1, check.keys = T) # .94 - great
psych::alpha(four_factor2, check.keys = T) # .87 - good
psych::alpha(four_factor3, check.keys = T) # .88 - good
psych::alpha(four_factor4, check.keys = T) # .73 - acceptable


#### THREE-FACTOR EFA ####

## Simple Structure ##

fa(efaquestions, nfactors=3, rotate = "oblimin", fm = "ml")
#Items 2,6,24,25,46,52 had multiple loadings
#Items 44,50,53,56,58 had no loadings

fa(efaquestions[ , -c(2,6,24,25,44,46,50,52,53,56,58)], 
   nfactors=3, rotate = "oblimin", fm = "ml")
#Items 14,30,47 had multiple loadings

fa(efaquestions[ , -c(2,6,14,24,25,30,44,46,47,50,52,53,56,58)], 
   nfactors=3, rotate = "oblimin", fm = "ml")
#Items 36,57 had multiple loadings

fa(efaquestions[ , -c(2,6,14,24,25,30,36,44,46,47,50,52,53,56,57,58)], 
   nfactors=3, rotate = "oblimin", fm = "ml")
#Item 45 had no loadings

fa(efaquestions[ , -c(2,6,14,24,25,30,36,44,45,46,47,50,52,53,56,57,58)], 
   nfactors=3, rotate = "oblimin", fm = "ml")
#We have achieved simple structure!

three_factor1 = efaquestions[,c(1,3:5,7:10,15,16,19:23,26,29,32,35,43,48,54,55)]
three_factor2 = efaquestions[,c(11,12,17,18,33,34,37,39:42,49)]
three_factor3 = efaquestions[,c(13,27,28,31,38,51)]

## Adequate Solution ##

#CFI = .862
three_finalmodel = fa(efaquestions[ , -c(2,6,14,24,25,30,36,44,45,46,47,50,52,53,56,57,58)], 
                     nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((three_finalmodel$STATISTIC-three_finalmodel$dof)/
       (three_finalmodel$null.chisq-three_finalmodel$null.dof))

tableprint = matrix(NA, nrow = 4, ncol = 3)

tableprint[1, ] = c("RMSEA", "0.069, 90% CI[0.062, 0.07]", "Acceptable")
tableprint[2, ] = c("RMSR", 0.05, "Good")
tableprint[3, ] = c("CFI", 0.862, "Poor")
tableprint[4, ] = c("TLI", 0.837, "Poor")

kable(tableprint, 
      digits = 3,
      col.names = c("Fit Index", "Value", "Description"))

#Reliabilities
psych::alpha(three_factor1, check.keys = T) # .94 - great
psych::alpha(three_factor2, check.keys = T) # .94 - great
psych::alpha(three_factor3, check.keys = T) # .70 - meh


#### TWO-FACTOR EFA ####

fa(efaquestions, nfactors=2, rotate = "oblimin", fm = "ml")
#Item 57 had multiple loadings
#Items 13,27,31,38,44,50,51,58 had no loadings

fa(efaquestions[, -c(13,27,31,38,44,50,51,57,58)], 
   nfactors=2, rotate = "oblimin", fm = "ml")
#We have simple structure!

two_factor1 = efaquestions[,c(1:10,14:16,19:26,28:30,32,35,36,43,45:48,52:56)]
two_factor2 = efaquestions[,c(11,12,17,18,33,34,37,39:42,49)]

## Adequate Solution ##

#CFI = .816
two_finalmodel = fa(efaquestions[, -c(13,27,31,38,44,50,51,57,58)], 
                      nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((two_finalmodel$STATISTIC-two_finalmodel$dof)/
       (two_finalmodel$null.chisq-two_finalmodel$null.dof))

tableprint = matrix(NA, nrow = 4, ncol = 3)

tableprint[1, ] = c("RMSEA", "0.073, 90% CI[0.066, NA]", "Acceptable")
tableprint[2, ] = c("RMSR", 0.06, "Good")
tableprint[3, ] = c("CFI", 0.799, "Poor")
tableprint[4, ] = c("TLI", 0.816, "Poor")

kable(tableprint, 
      digits = 3,
      col.names = c("Fit Index", "Value", "Description"))

#Reliabilities
psych::alpha(two_factor1, check.keys = T) # .95 - great
psych::alpha(two_factor2, check.keys = T) # .94 - great



#### EFA w/o Factor 4 ####

## Simple Structure ##
columns_stuff = c(FF1, FF2, FF3)

fa(efaquestions[ , ..columns_stuff], 
   nfactors=3, rotate = "oblimin", fm = "ml")


## Adequate Solution ##

#CFI = 0.891
finalmodel = fa(efaquestions[ , ..columns_stuff], 
                nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

tableprint = matrix(NA, nrow = 4, ncol = 3)

tableprint[1, ] = c("RMSEA", "0.063, 90% CI[0.056, 0.064]", "Acceptable")
tableprint[2, ] = c("RMSR", 0.04, "Good")
tableprint[3, ] = c("CFI", 0.891, "Poor")
tableprint[4, ] = c("TLI", 0.873, "Poor")

kable(tableprint, 
      digits = 3,
      col.names = c("Fit Index", "Value", "Description"))

#Reliabilities
psych::alpha(efaquestions[ , ..FF1], check.keys = T) # .94 - great
psych::alpha(efaquestions[ , ..FF2], check.keys = T) # .87 - great
psych::alpha(efaquestions[ , ..FF3], check.keys = T) # .88 - great

### Summary: Fewer items had to be eliminated when we started without factor 4.
### We lost some goodness of fit but residual measures were still good.
### Reliabilities were VERY good. (They also contained more questions so...)
### We'll need to see if the factors still make logical sense with the added items.

