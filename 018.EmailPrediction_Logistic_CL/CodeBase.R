#This script is created for email aggregated data. Be sure to use the same structure before running. 
#The purpose of this is to see goodness of fit for different formula combination


#--------------------------------------------------------PREPARATION---------------------------------------------------------------#

                #clear environment if neccessary
                rm(list = ls())
                
                #load neccessary packages
                #Note, you might need to install pacman first
                #install.packages("pacman")
                pacman::p_load("party","dplyr", "caret","e1071","pscl","ROCR","ResourceSelection")
                
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
                fml = converted ~ Ro + Rc + Fc + Fo + conversions + lifetime_sent + clusterNum + opened_pred +complained_pred + clicked_pred
                #make sure the formula components are the same as IV and DV below
                IV <- c("Ro", "Rc","Fc","Fo","conversions", "lifetime_sent","clusterNum","opened_pred","complained_pred","clicked_pred")
                DV <- "converted"
                TargetClassIdentify <- 0.005 #probability of hitting target (class of Good) (0-1)
                #might need to reset this threshold so that R can distinguish the class to run confusion matrix.
                #if you see error message e.g. the data cannot have more levels than the reference, means that your threshold set too high
                #note, this threshold does not affect any prediction, only affect generating confusion matrix

#--------------------------------------------------------EDIT END---------------------------------------------------------------#
#--------------------------------------------------------RUN AUTO---------------------------------------------------------------#
                
                
                source("./logit_Model_FUN.R") #run logistic model


                #logit_model returns a full dataframe with prediction probability
                Sum <- logit_Model(DT, percentPartition, DV, IV, fml,TargetClassIdentify)
                
                #save entrie dataset with prediction probabilities
                DT <- Sum$dataframe 
                #re-use DT as name of dataframe to test other logistic formula
                
                #save DT as csv. if needed
                #write.csv(DT, "DT_pred.csv")
            
                
                
                
