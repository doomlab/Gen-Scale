
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("sona_data.csv")
colnames(master)[1] = "StartDate"

block1 = c(24:48)
block2 = c(49:72)
block3 = c(73:96)

scale_questions = c(24:44,49:68,73:92)


# Accuracy ----------------------------------------------------------------

notypos = master
summary(notypos[scale_questions])
#everything is within the expected range

apply(notypos[scale_questions], 2, mean, na.rm = TRUE)
apply(notypos[scale_questions], 2, sd, na.rm = TRUE)
#nothing seems terribly wacky


# Missing -----------------------------------------------------------------

percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

#rows
missing = apply(notypos[scale_questions], 1, percentmiss) 
table(missing)
replacepeople = subset(notypos, missing <= 5)
dontpeople = subset(notypos, missing > 5)

#columns
apply(replacepeople[scale_questions], 2, percentmiss)
table(missingcol[scale_questions])
replacecolumn = replacepeople[scale_questions]
dontcolumn = replacepeople[ , -c(24:44,49:68,73:92)]

#replacing those with <= 5% missing by multiple imputation
library(mice)
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

#putting data back together
#excluding participants with > 5% missing
allcolumns = cbind(dontcolumn, nomiss)
summary(allcolumns)
nomissing = allcolumns


# SAD ---------------------------------------------------------------------

source("SADfunction.R")


# Outliers ----------------------------------------------------------------




# Assumptions -------------------------------------------------------------

# Additivity #


# Linearity #


# Normality #


# Homog/s #

