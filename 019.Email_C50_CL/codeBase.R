#this script is for testing C5.0 tree on email data
#This script is created for email aggregated data. Be sure to use the same structure before running. 
#The purpose of this is to see goodness of fit for different formula combination


#--------------------------------------------------------PREPARATION---------------------------------------------------------------#

              #clear environment if neccessary
              rm(list = ls())
              
              #load neccessary packages
              #Note, you might need to install pacman first
              pacman::p_load("party","ggplot2", "dplyr", "caret",
                             "reshape2","lme4","lattice","plyr",
                            "cluster","C50","e1071")
              
              #set your working directory
              setwd("~/Documents/02. EmailPrediction_CL/019.Email_C50_CL/") #change this
              
              source("./dtTransform_FUN.R") #trasnform data type           #do not change
              
              #grep data insert data name
              DT <- read.csv("./Data/eRFM_email_test1.csv") #change file name if need to
              #str(DT)
              DT <- dtTransform(DT) #run this only once
              #this is to change the data type to proper format

#--------------------------------------------------------EDIT PART---------------------------------------------------------------#


            #setup independent variabe and dependent variable, create a formula, setup partition percentage
            #make sure the name of column names match
            percentPartition <- 0.75 #% of train set for partitioning
            IV <- c("Ro", "Rc","Fc","Fo","conversions", "lifetime_sent","clusterNum")
            DV <- "opened"
            trialsNum <- 2 #an integer specifying the number of boosting iterations. A value of one indicates that a single model is used.
            rules <- FALSE #create C5.0 tree using rule set or not. TRUE - ruleset

#--------------------------------------------------------EDIT END---------------------------------------------------------------#
#--------------------------------------------------------RUN AUTO---------------------------------------------------------------#
            source("./C50_Model_FUN.R") 
            
            #note: function return a whole test set including the prediction value, also print out the summary of the C5.0 tree model, also confusion matrix on test set
            Sum <- C50_Model(DT, percentPartition,DV,IV,trialsNum,rules)
            #more about C5.0 https://www.rulequest.com/see5-unix.html
            
            #to save the Test set data including prediction 
            Test <- Sum$Test_dt
            
            #write.csv(Test, "Test.csv") 
            #to save the csv. file to directory if needed
            
            
            