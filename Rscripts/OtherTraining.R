
  #source("Rscripts/FormatData.R")


#source("Rscripts/FormatData.R")

## subset, merge, and reformat data
OtherTraining <- c(data_Training$Q10_other_1, data_Support$Q10_other_2, data_Support$Q10_other_3)
OtherTraining <- OtherTraining[!is.na(OtherTraining)]

### Nb of respondents
length(OtherTraining)

## recode
OtherTraining
