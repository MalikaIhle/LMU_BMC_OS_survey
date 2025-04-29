# OB = Other Barriers
# WD = What Downsides


# source("Rscripts/FormatData.R")


# data_OB -----
data_OB <- prepare_freetext_subdataset(data_Barriers,"Q2", "_comment$")
data_OB <- data_OB[,-1] #remove Role column again (can be used to subset ahead of this line)
data_OB

# remove line 3 'I WAS REFERRING TO THE LACK OF RELEVANT DATA'
data_OB <- data_OB[-3,] # check that this remove the right line
data_OB

## Nb of responses
data_OB %>% summarise(across (everything(), ~sum(!is.na(.))))
data_OB$oa[!is.na(data_OB$oa)] # "POLICIES NOT STRONG, BUT ABSENT OR WEAK DOES NOT APPLY"
data_OB$material[!is.na(data_OB$material)] # "RAW DATA TOO LARGE FOR PUBLIC REPOSITORIES, THEREFORE ONLY (IF AT ALL) DEPOSITION OF PROCESSED DATA."



# data_WD -----
data_WD <- prepare_freetext_subdataset(data_Downsides, "Q2", "_comment$")
data_WD <- data_WD[,-1]
data_WD 

## Nb of responses
data_WD %>% summarise(across (everything(), ~sum(!is.na(.))))
data_WD$oa[!is.na(data_WD$oa)] # not peer reviewed work propagating ; not accepted work that could be scooped ; problematic with some publishers
data_WD$rdm[!is.na(data_WD$rdm)] # too much work and not enough incentives in this competitive world
data_WD$code[!is.na(data_WD$code)] # too much work and not enough incentives in this competitive world
data_WD$material[!is.na(data_WD$material)] # too much work and not enough incentives in this competitive world
data_WD$prereg[!is.na(data_WD$prereg)] # fear of scooping ; too much work  ; **lack of flexibility in changing goals and approach even within an experimental study** !! ; not relevant for exploratory research
data_WD$rr[!is.na(data_WD$rr)] # scooping; work; lack of flexibility ; difficult for exploratory research ; +: delay before starting experiment + more work for reviewers 
data_WD$rr[!is.na(data_WD$rr)][1]

