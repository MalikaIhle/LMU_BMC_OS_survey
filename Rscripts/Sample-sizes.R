
data_Awareness_ss <- data_Awareness %>% summarise(n())
data_Effect_ss <- data_Effect %>% summarise(n())
data_Barriers_ss <- data_Barriers %>% summarise(n())
data_Downsides_ss <- data_Downsides %>% summarise(n())
data_CurrentCriteria_ss <- data_CurrentCriteria %>% summarise(n())
data_DesiredCriteria_ss <- data_DesiredCriteria %>% summarise(n())
data_Training_ss <- data_Training %>% summarise(n())
data_Support_ss <- data_Support%>% summarise(n())

                                            
table_SS <- rbind(data %>% summarise(n())
        , data_Awareness_ss
        , data_Effect_ss
        , data_Barriers_ss
        , data_Downsides_ss
        , data_CurrentCriteria_ss
        , data_DesiredCriteria_ss
        , data_Training_ss
        , data_Support_ss 
        )
colnames(table_SS) <- "Overall n"

rownames(table_SS) <- c('Role'
                 , 'Awareness'
                 , 'Effect'
                 , 'Barriers'
                 , 'Downsides'
                 , 'Current criteria'
                 , 'Desired criteria'
                 , 'Training'
                 , 'Support'
)

NperRoles <- rbind(table(data$Q2)
                   , table(data_Awareness$Q2)
                   , table(data_Effect$Q2)
                   , table(data_Barriers$Q2)
                   , table(data_Downsides$Q2)
                   , table(data_CurrentCriteria$Q2)
                   , table(data_DesiredCriteria$Q2)
                   , table(data_Training$Q2)
                   , table(data_Support$Q2)
                  )


table_SS <- cbind(table_SS, NperRoles)

colnames(table_SS) <- c('Overall n'
                        , 'MSc students'
                        , 'PhD students'
                        , 'Group leaders'
                        , 'Postdocs'
                        , 'Support staff'

)

table_SS$Questions <- rownames(table_SS)
table_SS$Order <- 1:nrow(table_SS)
colnames(table_SS)[colnames(table_SS) == "Questions"] <- "Question topic"
table_SS <- table_SS[,c('Order','Question topic','Overall n', 'Group leaders', 'Postdocs', 'Support staff', 'PhD students', 'MSc students')]
rownames(table_SS) <- NULL
table_SS
