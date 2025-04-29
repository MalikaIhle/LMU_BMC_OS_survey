
#source("Rscripts/FormatData.R")

data_DesiredCriteria_other <- subset_columns_by_pattern2(data_DesiredCriteria, "Q2","Q9_other")

### Nb of respondents
nrow(data_DesiredCriteria_other)

## recode
OtherDesiredCriteria <- data.frame(stack(data_DesiredCriteria_other[,-1])$values) # remove Role column
OtherDesiredCriteria <- OtherDesiredCriteria[!is.na(OtherDesiredCriteria),]
OtherDesiredCriteria <- data.frame(criteria = toupper(OtherDesiredCriteria))
OtherDesiredCriteria$'Other desired criteria' <- NA

## recode

OtherDesiredCriteria$'Other desired criteria'[str_detect(OtherDesiredCriteria$criteria, '^AGE|SEX|GENDER|NATIONALITY')] <- 'Independence of personal demographics (gender, age)'
OtherDesiredCriteria$'Other desired criteria'[str_detect(OtherDesiredCriteria$criteria, 'GERMAN')] <- 'Ability to teach in German'
OtherDesiredCriteria$'Other desired criteria'[str_detect(OtherDesiredCriteria$criteria, 'INTEGRITY|ROBUSTNESS|QUALITY HIGH OPEN RESEARCH')] <- 'Integrity, robustness, reliability and openness of research'
OtherDesiredCriteria

OtherDesiredCriteria <- tabyl(OtherDesiredCriteria, var1 = 'Other desired criteria', show_na = FALSE, show_missing_levels = FALSE)
OtherDesiredCriteria <- OtherDesiredCriteria[,c('Other desired criteria', 'n')]
OtherDesiredCriteria
