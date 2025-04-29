
  #source("Rscripts/FormatData.R")

data_CurrentCriteria_other <- subset_columns_by_pattern2(data_CurrentCriteria,"Q2", "Q8_other")

### Nb of respondents
nrow(data_CurrentCriteria_other)

## recode
OtherCurrentCriteria <- data.frame(stack(data_CurrentCriteria_other[,-1])$values)
OtherCurrentCriteria <- OtherCurrentCriteria[!is.na(OtherCurrentCriteria),]
OtherCurrentCriteria <- data.frame(criteria = toupper(OtherCurrentCriteria))
OtherCurrentCriteria$'Other current criteria' <- NA

## recode

OtherCurrentCriteria$'Other current criteria'[str_detect(OtherCurrentCriteria$criteria, 'YOUNG AGE|CAREER STAGE|^AGE|SEX|GENDER|NATIONALITY|LANGUAGE')] <- 'Personal demographics (gender, age, nationality, language)'
OtherCurrentCriteria$'Other current criteria'[str_detect(OtherCurrentCriteria$criteria, 'FIT|COLLABORATION|NETWORKING')] <- 'Good fit to research environment and potential for collaboration'
OtherCurrentCriteria

OtherCurrentCriteria <- tabyl(OtherCurrentCriteria, var1 = 'Other current criteria', show_na = FALSE, show_missing_levels = FALSE)
OtherCurrentCriteria <- OtherCurrentCriteria[,c('Other current criteria', 'n')]
OtherCurrentCriteria
