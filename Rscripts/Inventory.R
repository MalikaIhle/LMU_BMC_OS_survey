
  #source("Rscripts/FormatData.R")

data_Inventory
data_Inventory <- data.frame(stack(data_Inventory[,-1]))$values
data_Inventory <- data_Inventory[!is.na(data_Inventory)]
data_Inventory <- data.frame(initiative = toupper(data_Inventory))
data_Inventory$initiative_cat <- NA
data_Inventory$initiative_cat[str_detect(data_Inventory$initiative, 
                                            c("TUTORIAL|ONLINE VIDEO|COOK BOOK"))] <- 'Not categorised'
data_Inventory

## Nb of responses
nrow(data_Inventory)

data_Inventory$initiative_cat[str_detect(data_Inventory$initiative, 
                                            c("OPEN SCIENCE CENTER"))] <- "LMU Open Science Center"
data_Inventory$initiative_cat[str_detect(data_Inventory$initiative, 
                                            c("STRAUB|BMC"))] <- "Dr. Straub, Bioinformatics Core Facilities"
data_Inventory$initiative_cat[str_detect(data_Inventory$initiative, 
                                         c("SFB 1064 OPEN SCIENCE PROGRAM"))] <- "SFB 1064 Open Science Program"


Table_Inventory <- data.frame(table(data_Inventory$initiative_cat[data_Inventory$initiative_cat != "Not categorised"]))
colnames(Table_Inventory) <- c("initiatives", "count")
Table_Inventory %>% arrange(-count)

