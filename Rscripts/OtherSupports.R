
  #source("Rscripts/FormatData.R")

## subset, merge, and reformat data
OtherSupport <- c(data_Support$Q11_other_1, data_Support$Q11_other_2, data_Support$Q11_other_3)
OtherSupport <- OtherSupport[!is.na(OtherSupport)]

### Nb of respondents
length(OtherSupport)

## recode

OtherSupport
