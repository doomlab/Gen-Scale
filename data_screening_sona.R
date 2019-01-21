
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("sona_data.csv")

block1 = c(24:48)
block2 = c(49:72)
block3 = c(73:96)

scale_questions = c(24:44,49:68,73:92)

### Accuracy ###

notypos = master
summary(notypos[-c(1:2), scale_questions])
notypos[,24]
levels(notypos[,24])


### Missing ###



### SAD ###



### Outliers ###



### Assumptions ###

# Additivity #


# Linearity #


# Normality #


# Homog/s #
