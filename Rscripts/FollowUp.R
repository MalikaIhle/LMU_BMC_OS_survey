
#source("Rscripts/FormatData.R")

table_FollowUp <- as.data.frame(data_FollowUp %>% group_by(Q2, Q13) %>% summarise(n = n()))
table_FollowUp <- table_FollowUp[complete.cases(table_FollowUp),]
table_FollowUp
