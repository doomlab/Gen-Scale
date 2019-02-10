
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("mturk_data.csv")
colnames(master)[1] = "StartDate"

scale_questions = c(24:44,49:68,73:92)


# Accuracy ----------------------------------------------------------------

notypos = master
summary(notypos[scale_questions])
#everything is within the expected range

apply(notypos[scale_questions], 2, mean, na.rm = TRUE)
apply(notypos[scale_questions], 2, sd, na.rm = TRUE)


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
page1 = SAD(dat = nomissing[, 41:61], 
            rt = nomissing$Q10_Page.Submit,
            min = 1, 
            max = 5, 
            partno = nomissing$ResponseId, 
            click = nomissing$Q10_Click.Count, 
            manvec = nomissing$Q2_21, 
            mancor = 1, 
            char = 1626)

#Block 2
page2 = SAD(dat = nomissing[, 62:81], 
            rt = nomissing$Q11_Page.Submit,
            min = 1, 
            max = 5, 
            partno = nomissing$ResponseId, 
            click = nomissing$Q11_Click.Count, 
            manvec = nomissing$Q4_20, 
            mancor = 4, 
            char = 1491)

#Block 3
page3 = SAD(dat = nomissing[, 82:101], 
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
table(nomissing$totalbad)
nolowqual = subset(nomissing, totalbad < 6)
#74 participants excluded as low quality data
#34%, might be too much

# Outliers ----------------------------------------------------------------

mahal = mahalanobis(nolowqual[ , 41:101], 
                    colMeans(nolowqual[ , 41:101], na.rm = TRUE),
                    cov(nolowqual[ , 41:101], use="pairwise.complete.obs"))

cutoff = qchisq(1 - .001,ncol(nolowqual[ , 41:101]))
ncol(nolowqual[ , 41:101]) #df 61
cutoff #cutoff 100.8879

summary(mahal < cutoff)
mahal[mahal > cutoff]

noout = subset(nolowqual, mahal < cutoff)
#4 participants excluded as outliers

# Assumptions -------------------------------------------------------------

# Linearity #
random = rchisq(nrow(noout[ , 41:101]), 7) 
fake = lm(random~., data=noout[ , 41:101])

standardized = rstudent(fake)
{qqnorm(standardized)
  abline(0,1)}

# Normality #
hist(standardized, breaks=15)

# Homog/s #
fitvalues = scale(fake$fitted.values)
{plot(fitvalues, standardized) 
  abline(0,0)
  abline(v = 0)}
#actually looks pretty good except for one point out a little far

write.csv(noout, file = "mturk_data_screened.csv")
