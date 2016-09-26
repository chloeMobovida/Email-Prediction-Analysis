#--------------------------------------------------LOAD-------------------------------------------------------#
rm(list = ls())
#set directory
setwd("/Users/chloeli/Documents/02. EmailPrediction_CL/011.Data")

#load neccessary packages
#Note, you might need to install pacman first
pacman::p_load("party","gridExtra","ggplot2", "dplyr","lubridate","reshape2","data.table","quantmod","lme4","lattice","plyr","broom",'ReporteRs',"knitr","xtable", "cluster","fpc","factoextra","grid","gridExtra","scales")


#load data
RFM_DT <- read.csv("RRFFK_220K_sends.csv") #this is aggregated data after David Liu rank riid by recency and frequency

RFM_DT <- dplyr::filter(RFM_DT, sends != "#N/A")

#there are 3953 records of sends being "#N/A". David said remove those no matter what.


RFM_DT_orig <- RFM_DT #reassign the dataset into new vector for clustering purpose

#reformat data for graphing purpose
RFM_DT$riid <- as.character(RFM_DT$riid)
#change multiple variables into factor type
cols = c(2, 3, 4,5)
RFM_DT[cols] <- lapply(RFM_DT[cols], factor)

RFM_DT[,'sends'] <- as.numeric(as.character(RFM_DT[,'sends']))
RFM_DT[,'conversions'] <- as.numeric(RFM_DT[,'conversions'])


RFM_DT$Converted <- as.factor(ifelse(RFM_DT$conversions != 0, 1,0))





Fmla = Converted ~ Ro + Rc + Fc + Fo
TreeModel = ctree(Fmla, data = RFM_DT)
plot(TreeModel)
