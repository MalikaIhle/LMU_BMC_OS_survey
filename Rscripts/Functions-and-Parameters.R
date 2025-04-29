# items to judges

Measures <- c('Preprint/Postprint', 'RDM plan', 'FAIR data sharing', 'Code sharing', 'Materials sharing', 'Preregistration', 'Registered Report')
Criteria <- c('Number of publications','Prestige of publication outlet','Quality of publications', 'Authorship role', 'Citations', 'Grant acquisition', 
              'Impact','Teaching', 'Supervision, mentoring', 'Service to the profession','Citizenship','Awards',
              'Collaboration network','Open research practices')

# functions


## Format raw data
clean_formR_data <- function(data){
  
  ## Subset data to records with filled out Consent and Role: the 2 mandatory questions
  data <- subset(data[!is.na(data$consent_text_mandatory) & !is.na(data$Q2),] )
  data[!is.na(data) & data ==""] <- NA
  return(data)
  }


subset_columns_by_pattern <- function(data, pattern){
  data <- data[, grep(pattern=pattern, x=colnames(data))]
} 

subset_columns_by_pattern2 <- function(data, pattern1, pattern2){
  data <- cbind(data[, c(pattern1), drop = FALSE]
                , data[, grep(pattern=pattern2, x=colnames(data))])
  data <- data[rowSums(!is.na(data)) >1,] # remove rows where all NA or empty
}

### for Oxford data

Divisions <- c("MSD", "MPLS","SSD", "Hum", "ContEd", "GLAM", "College")
Roles <- c('PGR Student','Research Staff', 'Research Support Staff', 'Academic')
clean_qualtrics_data <- function(data, surveyindex){
  
  ## change column names according to survey index
  data <- data[-c(1, 2), -which(names(data) %in% c("DistributionChannel","UserLanguage"))] 
  colnames(data) <- surveyindex$VariableName[surveyindex$QuestionCode == colnames(data)] 
  data[data == ""] <- NA
  
  ## Subset data to records with filled out Consent, Affiliation, and Role: the 3 mandatory questions
  #nrow(data)
  data <- subset(data[!is.na(data$Consent) & !is.na(data$DivCol) & !is.na(data$Role),] )
  #nrow(data)
  
  ## Consent T/F
  data$Consent <- data$Consent == "I consent to participating in the survey and to processing of the information I provide as outlined above."
  
  # Affiliation
  {
    ## College TRUE FALSE
    data$College <- data$DivCol == "College-only staff"
    #table(data$College)
    
    ## Division
    data$Div <- data$DivCol
    data$Div[data$Div == "College-only staff"] <- data$ColDiv[data$Div == "College-only staff"] # replacing College only staff affiliation to their Div of affinity
    data$Div[is.na(data$Div)] <- 'College' # removing college only staff who didn't say which Division their field of research was closed to...
    
    data$Div[data$Div == "Social Sciences Division"] <- "SSD"
    data$Div[data$Div == "Humanities Division"] <- "Hum"
    data$Div[data$Div == "Department for Continuing Education"] <- "ContEd"
    data$Div[data$Div == "Mathematical, Physical, and Life Sciences Division"] <- "MPLS"
    data$Div[data$Div == "Medical Sciences Division"] <- "MSD"
    data$Div[data$Div == "Gardens, Libraries and Museums"] <- "GLAM"
    #table(data$Div)
    
    ## Department
    data$Dept <- data$Dept1 
    data$Dept[!is.na(data$Dept2)] <- data$Dept2[!is.na(data$Dept2)]
    data$Dept[!is.na(data$Dept3)] <- data$Dept3[!is.na(data$Dept3)]
    data$Dept[!is.na(data$Dept4)] <- data$Dept4[!is.na(data$Dept4)]
    data$Dept[!is.na(data$Dept5)] <- data$Dept5[!is.na(data$Dept5)]
    data$Dept[!is.na(data$Dept6)] <- data$Dept6[!is.na(data$Dept6)]
    data$Dept[!is.na(data$Dept7)] <- data$Dept7[!is.na(data$Dept7)]
    data$Dept[!is.na(data$Dept8)] <- data$Dept8[!is.na(data$Dept8)]
    data$Dept[!is.na(data$Dept9)] <- data$Dept9[!is.na(data$Dept9)]
    data$Dept[!is.na(data$Dept10)] <- data$Dept10[!is.na(data$Dept10)]
    #table(data$Dept)
    
    # one department was renamed
    data$Dept[!is.na(data$Dept) & str_detect(data$Dept, "School of Interdisciplinary Area Studies")] <- "School of Global Area Studies" 
    
    
    ## Other Department
    #table(data$OtherDept)
    data[!is.na(data$OtherDept),c('OtherDept', 'Dept', 'Div')]
    
    ### recoding of Dept for otherdept actually in the list
    data$Dept[!is.na(data$OtherDept) & (data$OtherDept == "Wellcome Centre for Human Genetics" |
                                          data$OtherDept == "Experimental Medicine"|
                                          data$OtherDept == "NDM Experimental Medicine"|
                                          data$OtherDept == "Nuffield department of medicine"|
                                          data$OtherDept == "Division of Structural Biology"|
                                          data$OtherDept == "Nuffield Department of Experimental Medicine")] <- "Nuffield Department of Clinical Medicine"
    
    data$Dept[!is.na(data$OtherDept) & str_detect(data$OtherDept, "Oxford Internet Institute")] <- "Oxford Internet Institute"
    
    data[!is.na(data$Dept) & data$Dept == "Other",]
    
    # clean up long longer useful column
    unwanted_colnames <- c("DivCol", "ColDiv", names(data[, grep(pattern="Dept[0-9]+", colnames(data))]))
    data <- data[, -which(names(data) %in% unwanted_colnames)] 
    rm(unwanted_colnames)
  }
  
  # Role
  {
    #table(data$Role) 
    data$StudentStaff <- data$Role == "Student on a postgraduate research programme"
    data$StudentStaff[data$StudentStaff == TRUE] <- "Student"
    data$StudentStaff[data$StudentStaff == FALSE] <- "Staff"
    
    data$Role[data$Role == "Student on a postgraduate research programme"] <- "PGR Student"
    data$Role[data$Role == "Research Staff or Research Fellow"] <- "Research Staff"
    data$Role <- factor(data$Role, levels = Roles)
  }
  
  # Years of Experience
  data$Duration <- as.numeric(data$Duration)
  
  return(data)
  
}

## Prepare data (all but barriers) for plotting
create_skeleton <- function(Question, answers, columns){ # needed for the legend in case some values were not represented
  
  # Question <- Measures
  # answers <- Awareness_answers
  # columns <- Awareness_columns

  LabelIndiv <- rep(Question, each = length(answers)) 
  Answer <- rep(answers, times= length(columns)) 
  ID <- paste(LabelIndiv, Answer, sep="_") 
  skeleton <- data.frame(ID, LabelIndiv, Answer)
}

summarise_item <-  function(data, item, name_item){  # item is the name of the columns e.g. Q4_oa given as an expression (not a string)
  
  # example to test function summarise_item
  # data <- data
  # columns <- Awareness_columns
  # Question <- Measures
  # i <- 1
  # item <- columns[[i]]
  # name_item <- Question[i]
  # summarise_item(data, columns[[i]], Question[i])

  
 data2 <-  data[!is.na(data[as.character(item)]),]  %>%  # select 1 column (item) when non NA (e.g. 'Q4_oa')
    group_by({{item}}) %>%  # the double curly bracket read item as e.g. Q4_oa; ie. it groups by the type of answer (e.g. 'Accessing / using only')
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n, na.rm = TRUE) * 100 ) 
  data2$ID = paste(name_item, unlist(data2[,as.character(item)]), sep ="_") # use the full name of the item (e.g. "Open Access")
  return(data2[,c('ID', 'n', 'perc')])
  
}

bind_summaries_items <- function(Question, data, columns){
  
  # example to test function summarise_item
  # data <- data
  # columns <- Awareness_columns
  # Question <- Measures
  # i <- 1
  # item <- columns[[i]]
  # name_item <- Question[i]
  # summarise_item(data, columns[[i]], Question[i])
  
  summaryitems <- vector(mode= "list", length = length(Question))
  for (i in 1:length(Question)) {
    summaryitems[[i]] <-  summarise_item(data, columns[[i]], Question[i])
  }
  summaryitems <- data.frame(do.call(rbind, summaryitems))
  return(summaryitems)
}

prepare_data_for_plotting <- function(Question, data, answers, columns){
  skeleton <- create_skeleton(Question, answers, columns) # create skeleton of all possible answers
  summaryitems <- bind_summaries_items(Question, data, columns)
  formatted_data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE) # merge summary items to skeleton
  return(formatted_data)
}


## Prepare barriers data for plotting

summarise_barriers_item <-  function(data, item, name_item){
  
  #example to test function
  # data <- data_Barriers
  # columns <- Barriers_columns
  # i <- 1
  # item <- columns[[i]]
  # name_item <- Question[i]
  # summarise_barriers_item(data, columns[[i]])
  # summarise_barriers_item(data_Barriers, "Q6_oa")

  data1 <- subset_columns_by_pattern(data, item) 
  data2 <-  data1[,  -grep('comment', x=colnames(data1))]
  data3 <- data2[!is.na(data2)] # remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
  
  NbRespondents <- length(data3)  # number of time the item was scored
  
  
  data4 <- as.data.frame(table(unlist(strsplit(data3, ', '))))# %>% tibble::as_tibble() %>% count(value)  # counts of each answer
  data5 <- merge(data4, NbRespondents, all.x = TRUE) 
  data5$perc <- data5$Freq/data5$y * 100 # calculate percentages of respondent having selected (non mutually exclusively) each answer
  #data5$ID <- paste(as.character({{item}}), data5$Var1, sep="_")
  data5$ID = paste(name_item, unlist(data5[1]), sep ="_") # use the full name of the item (e.g. "Open Access")
  data6 <- data5[,c('ID', 'Freq', 'perc','y')]
  colnames(data6) <- c('ID', 'n', 'perc','NbRespondents')
  return(data6)
}

bind_summaries_barriers_items <- function(Question, data, columns){
  
  # data <- data_Barriers
  # columns <- Barriers_columns
  # item <- columns[[i]]
  # summarise_item(data, columns[[i]])
  
  summaryitems <- vector(mode= "list", length = length(Question))
  for (i in 1:length(Question)) {
    summaryitems[[i]] <-  summarise_barriers_item(data, columns[[i]], Question[i])
  }
  summaryitems <- data.frame(do.call(rbind, summaryitems))
  return(summaryitems)
  
  
}

prepare_barriers_data_for_plotting <- function(Question, data, answers, columns){
  
  # Question <- Measures
  # data <- data_Barriers
  # columns <- Barriers_columns
  # answers <- Barriers_answers
  
  
  skeleton <- create_skeleton(Question, answers, columns) # create skeleton of all possible answers
  summaryitems <- bind_summaries_barriers_items(Question, data, columns)
  formatted_data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE) # merge summary items to skeleton
  return(formatted_data)
}


## Plotting functions
horizontal_stacked_barplot <- function(data, Question, answers, answers_colors, title_plot, legend, legend_position){
  data$LabelIndiv <- factor(data$LabelIndiv, levels = rev(Question)) # this will determine order of the bars
  
  data %>% 
    ggplot() +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = answers)),
             stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels = legend, 
                      drop = FALSE) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = legend_position, 
      legend.text = element_text(size=11.5, face = "bold", hjust = 0),
      #legend.box.margin = margin(2, 2, 2, 0),
      #legend.margin=margin(2, 2, 2, 0),
      #legend.justification = "left",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      axis.text.x = element_text(size = 14,face = "bold"),
      axis.text.y = element_text(size = 18, colour = "black", face = "bold"),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = 20)) + 
    ggtitle(title_plot) + 
    guides(fill = guide_legend(nrow = 1, 
      reverse = TRUE,
      byrow = TRUE))
}

horizontal_dodged_barplot_on_barriers <- function(data, Question, answers, answers_colors, title_plot, legend, plot_ylim){
  data$LabelIndiv <- factor(data$LabelIndiv, levels = rev(Question)) # this will determine order of the bars
  ggplot(data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", position = "dodge") +
       scale_fill_manual(values = Barriers_colors, # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks= Barriers_answers,
                      labels = Barriers_legend
                      , drop = FALSE)+
    
    coord_flip(ylim = c(0, plot_ylim)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80), label = c("0%", "20%", "40%", "60%", "80%"))+
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
     # axis.text.x = element_text(angle = 90),
            legend.title=element_blank()) + guides(fill = guide_legend(reverse = FALSE))+
    ggtitle(title_plot)
  
}  

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}


## analyse text
capitalise_all_strings <- function(data){
  data.frame(lapply(data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}), stringsAsFactors=FALSE)
}

prepare_freetext_subdataset <- function(data, pattern1, pattern2){
  # example to test function
  # data <- data_Barriers
  # pattern1 <- "Q2"
  # pattern2 <- "_comment$"
  subdataset <- subset_columns_by_pattern2(data, pattern1,pattern2)
  subdataset <- capitalise_all_strings(subdataset)
  colnames(subdataset) <- str_remove(colnames(subdataset), pattern2)
  colnames(subdataset)[-1] <- str_remove(colnames(subdataset)[-1], "Q\\d_")
  return(subdataset)
}

create_list_for_checking_cat <- function (data){
  
  # example to test function
  # data <- data_WD

  data <- add_column(data, ID = 1:nrow(data), .before = 1)
  colnameswithcat <- colnames(data[,grep(pattern=".*cat", x=colnames(data))])
  colnamesnotcat <- colnames(data[,-grep(pattern=".*cat", x=colnames(data))])
  data_cat <- data %>% select(all_of(colnameswithcat))
  
  # merge pivot table of values with their first cat 
  a_values <- pivot_longer(data[,colnamesnotcat], -c(ID), values_to = "Value", names_to = "Measure")
  a_cat <- pivot_longer(data_cat, colnameswithcat, values_to = "Category", names_to = "Measure")
  a <- cbind(a_values, a_cat[,c('Category')])
  a <- a[!is.na(a["Value"]),]
  a <- a[with(a,order(a$ID,a$Measure)),]
  
  return(a[a$Category != 'Not categorised',])
  } # to update if cat2 and cat3 created
  
create_pivot_table_from_list_for_checking_cat <- function (a, title_table){
  
  #example to test function
  #a <- a_data_WD
  #title_table <- "Downsides to adoption of Open Research Practices"

  name_data_argument <- deparse(substitute(a)) # get the name of the dataset to apply if statement below to sort columns in the table
  
  a$Measure[a$Measure == 'oa'] <- "Preprint/Postprint" 
  a$Measure[a$Measure == 'rdm'] <- "RDM plan" 
  a$Measure[a$Measure == 'fair'] <- "FAIR data sharing"
  a$Measure[a$Measure == 'code'] <- "Code sharing" 
  a$Measure[a$Measure == 'material'] <- "Materials sharing" 
  a$Measure[a$Measure == 'prereg'] <- "Preregistration"
  a$Measure[a$Measure == 'rr'] <- "Registered Report"
  a$Measure<- factor(a$Measure, levels = Measures)
  
  b <- a %>% group_by(Measure, Category) %>% summarise(count = n()) 
  c <- dcast(b, Category ~ Measure, value.var = "count") # from reshape2
  c$Total <- rowSums(c[,-1], na.rm=TRUE)
  
 
  c <- c[with(c, order(-c$Total
                       #,-c$`Preprint/Postprint`
                       ,-c$`RDM plan`
                       #,-c$`FAIR data sharing`
                       #,-c$`Code sharing`
                       #,-c$`Materials sharing`
                       ,-c$Preregistration
                       ,-c$`Registered Report`
                       )),]
  
  
  c[is.na(c)] <- '-'
  d <- c[c$Category != 'Not categorised',]
  colnames(d)[colnames(d) == 'Category'] <- title_table
  rownames(d) <- NULL
  pivot_table <- d
  rm(a,b,c,d)
  
  return(pivot_table)
}

