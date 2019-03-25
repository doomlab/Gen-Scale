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


#### EFA ####

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

### Simple structure
fa(master, nfactors=3, rotate = "oblimin", fm = "ml")

fa(master[ , -c(8,14,15,17,18,23,30)], nfactors=3, rotate = "oblimin", fm = "ml")



