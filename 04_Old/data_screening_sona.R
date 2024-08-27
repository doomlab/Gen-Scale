
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(92310)

master = read.csv("sona_data.csv")
colnames(master)[1] = "StartDate"

block1 = c(23,24:48)
block2 = c(23,49:72)
block3 = c(23,73:96)

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
missingcol = apply(replacepeople[scale_questions], 2, percentmiss)
table(missingcol[scale_questions])
replacecolumn = replacepeople[scale_questions]
dontcolumn = replacepeople[ , -c(24:44,49:68,73:92)]

#replacing those with <= 5% missing by multiple imputation
library(mice)
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

#putting data back together
#excluding participants with > 5% missing (7 total)
allcolumns = cbind(dontcolumn, nomiss)
summary(allcolumns)
nomissing = allcolumns


# SAD ---------------------------------------------------------------------

source("SADfunction_emb.R")

#Block 1
page1 = SAD(dat = nomissing[, 37:57], 
    rt = nomissing$Q10_Page.Submit,
    min = 1, 
    max = 5, 
    partno = nomissing$ResponseId, 
    click = nomissing$Q10_Click.Count, 
    manvec = nomissing$Q2_21, 
    mancor = 1, 
    char = 1626)

#Block 2
page2 = SAD(dat = nomissing[, 58:77], 
            rt = nomissing$Q11_Page.Submit,
            min = 1, 
            max = 5, 
            partno = nomissing$ResponseId, 
            click = nomissing$Q11_Click.Count, 
            manvec = nomissing$Q4_20, 
            mancor = 4, 
            char = 1491)

#Block 3
page3 = SAD(dat = nomissing[, 78:97], 
            rt = nomissing$Q12_Page.Submit,
            min = 1, 
            max = 5, 
            partno = nomissing$ResponseId, 
            click = nomissing$Q12_Click.Count, 
            manvec = nomissing$Q5_16, 
            mancor = 3, 
            char = 1496)

#Total
nomissing$totalbad = page1$badTotal + page2$badTotal + page3$badTotal
nomissing$totalbadUP = page1$badChar + page1$badClick + page1$badMC + 
  page2$badChar + page2$badClick + page2$badMC + 
  page3$badChar + page3$badClick + page3$badMC
table(nomissing$totalbadUP)
nolowqual = subset(nomissing, totalbadUP < 4)
##B&S say 2/5 which is 40%, so 40% of the 9 total is 3.6 or 4 or more

# Outliers ----------------------------------------------------------------

mahal = mahalanobis(nolowqual[ , 37:97], 
                    colMeans(nolowqual[ , 37:97], na.rm = TRUE),
                    cov(nolowqual[ , 37:97], use="pairwise.complete.obs"))

cutoff = qchisq(1 - .001,ncol(nolowqual[ , 37:97]))
ncol(nolowqual[ , 37:97]) #df 61
cutoff #cutoff 100.8879

summary(mahal < cutoff)
mahal[mahal > cutoff]

noout = subset(nolowqual, mahal < cutoff)
#42 participants excluded as outliers

# Assumptions -------------------------------------------------------------

# Linearity #
random = rchisq(nrow(noout[ , 37:97]), 7) 
fake = lm(random~., data=noout[ , 37:97])

standardized = rstudent(fake)
{qqnorm(standardized)
  abline(0,1)}
#seems okay

# Normality #
hist(standardized, breaks=15)
#a little skewed but mostly centered on 0 and between -2 and 2 so okay

# Homog/s #
fitvalues = scale(fake$fitted.values)
{plot(fitvalues, standardized) 
  abline(0,0)
  abline(v = 0)}
#some homogeneity issues but we'll accept it

write.csv(noout, file = "sona_data_screened.csv", row.names = F)
library(beepr)
beep(sound = 5)

# > nrow(master)
# [1] 564
# > nrow(nomissing)
# [1] 557
# > nrow(nolowqual)
# [1] 507
# > nrow(noout)
# [1] 464
