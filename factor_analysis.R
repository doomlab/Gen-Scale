library(data.table)

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

#do the efa here 