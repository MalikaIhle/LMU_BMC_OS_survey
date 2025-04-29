# Packages
library(here)
library(renv)
library(tidyverse)
library(RColorBrewer)
library(janitor)
library(egg) # for function egg:: ggarrange (next package overwrites it because when not specified in code I want ggarrange from ggpubr). this one equalise width for shared axis
library(ggpubr) # for function ggarrange for grid ggplot. this one create common legend
library(arsenal) # for function comparedf (to compare dataset created by hand and output of function)
library(data.table) # for transpose(df) 
library(reshape2) # for making pivot tables (function 'dcast' in functions and parameter .R)
library(pivottabler) # for making other pivot tables (function 'PivotTable' in sample size .R)

# Functions
source(here::here("Rscripts","Functions-and-Parameters.R"))

# Load and format data   <------------------------ need updating !!
data <- read.csv(here("Data/BMC_OS_survey.csv"), stringsAsFactors=FALSE)
#targetnumber <- 100 # suspected number of researchers in BMC - FIXME
data <- clean_formR_data(data) 


# Split data per question
data_Awareness <- subset_columns_by_pattern2(data, "Q2","Q4")
data_Effect <- subset_columns_by_pattern2(data, "Q2","Q5")
data_Barriers <- subset_columns_by_pattern2(data, "Q2","Q6")
data_Downsides <- subset_columns_by_pattern2(data,"Q2", "Q7")
data_Training <- subset_columns_by_pattern2(data, "Q2","Q10")
data_Support <- subset_columns_by_pattern2(data, "Q2","Q11")
data_CurrentCriteria <- subset_columns_by_pattern2(data, "Q2","Q8")
data_DesiredCriteria <- subset_columns_by_pattern2(data,"Q2", "Q9")
data_Inventory <- subset_columns_by_pattern2(data, "Q2", "Q12")
data_FollowUp <- data[, c("Q2", "Q13")]

source(here::here("Rscripts","Sample-sizes.R"))
