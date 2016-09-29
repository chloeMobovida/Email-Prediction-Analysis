#This script is created for email aggregated data. Be sure to use the same structure before running. 
#The purpose of this is to see goodness of fit for different formula combination


#--------------------------------------------------------PREPARATION---------------------------------------------------------------#

                #clear environment if neccessary
                rm(list = ls())
                
                
                #set your working directory
                setwd("~/Documents/02. EmailPrediction_CL/018.EmailPrediction_Logistic_CL/")
                
                source("./dtTransform_FUN.R") #trasnform data type #don't change
                
                #grep data insert data name
                DT <- read.csv("./Data/eRFM_email_test1.csv")
                #str(DT)
                DT <- dtTransform(DT) #run this only once
                
 #--------------------------------------------------------EDIT PART---------------------------------------------------------------#
                
                
                #setup independent variabe and dependent variable, create a formula, setup partition percentage
                #make sure the name of column names match
                percentPartition <- 0.75 #% of train set for partitioning
                fml = opened ~ Ro + Rc + Fc + Fo + conversions + lifetime_sent + clusterNum 
                IV <- c("Ro", "Rc","Fc","Fo","conversions", "lifetime_sent","clusterNum")
                DV <- "opened"
                TargetClassIdentify <- 0.5 #probability of hitting target (class of Good)
                #might need to reset this threshold so that R can distinguish the class to run confusion matrix.

#--------------------------------------------------------EDIT END---------------------------------------------------------------#
#--------------------------------------------------------RUN AUTO---------------------------------------------------------------#
                

                
                #load neccessary packages
                #Note, you might need to install pacman first
                #install.packages("pacman")
                pacman::p_load("party","dplyr", "caret","e1071","pscl","ROCR","ResourceSelection")
                
                
                source("./logit_Model_FUN.R") #run logistic model


                #logit_model returns a full dataframe with prediction probability
                DT <- logit_Model(DT, percentPartition, DV, IV, fml,TargetClassIdentify)
                
                #write.csv(DT, "Log_PredictionDT.csv")
                
                
                
                
