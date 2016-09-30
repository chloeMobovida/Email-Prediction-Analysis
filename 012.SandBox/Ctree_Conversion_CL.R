#clear environment if neccessary
rm(list = ls())

#load neccessary packages
#Note, you might need to install pacman first
pacman::p_load("party","ggplot2", "dplyr", "caret",
               "reshape2","data.table","lme4","lattice","plyr",
               "knitr", "cluster","grid","gridExtra","C50","e1071")

#set your working directory
setwd("~/Documents/02. EmailPrediction_CL/019.Email_C50_CL/")

source("./dtTransform_FUN.R") #trasnform data type #don't change

#grep data insert data name
DT <- read.csv("./Data/eRFM_email_test1.csv")
#str(DT)
DT <- dtTransform(DT) #run this only once


set.seed(12345)
library(caret)
inTrain = createDataPartition(DT$converted, p = 0.75, list = FALSE)
Train=DT[inTrain,]
Test=DT[-inTrain,]




#-------------------------------------------------------------------------------------------------------------------#
#                                              C50 Tree                                                             #                                               
#-------------------------------------------------------------------------------------------------------------------#


#------------------------Create a cost matrix to make passes and failure worth the same-----------------------------#

#cost matrix
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
#to identify customers who will convert but we predict will not, hurts us more than we think they will NOT convert but they actually will



costs <- matrix(
  c(0, nrow(subset(Train, Train[,"opened"] == "0")), nrow(subset(Train, Train[,"opened"] == "1")), 0),
  nrow=2,
  ncol=2,
  byrow=TRUE, 
  dimnames=list(c("0", "1"), c("0", "1")))

#-----------------------------------------Build a Tree Model--------------------------------------------------------#
Tree_Model_01 <- C5.0(x=Train[,c("Ro", "Rc","Fc","Fo","conversions", "lifetime_sent","clusterNum")],y=Train$opened, costs=costs, trials = 1, rules= FALSE)

plot(Tree_Model_01)

#use capture.output and later use addparagraph to add to pptx

Tree_Model_01

summary(Tree_Model_01)



#rule set
Tree_Model_02 <- C5.0(x=Train[,c("Ro", "Rc","Fc","Fo","conversions", "lifetime_sent","clusterNum")], y=Train$converted, costs=costs, trials = 2, rules= TRUE)
summary(Tree_Model_02)




#---------------------------------------Prediction On Same Data-----------------------------------------------------#
#------------------------------------C5.0 Decision Tree Performance-------------------------------------------------#

#predict on same data set
Tree.Pred <- predict(Tree_Model_01, Test[,c("Ro", "Rc","Fc","Fo","conversions", "lifetime_sent","clusterNum")], type = "class")

#add predicted value to original data set
Test$predTree <- Tree.Pred




#Confusion Matrix

confusionMatrix(Tree.Pred, Test$converted)

#Kappa
#It can also be interpreted as a comparison of the overall acurracy to the expected random chance accuracy. 
#The higher the Kappa metric is, the better your classifier is compared to a random chance classifier. 



Fmla = converted ~ Ro + Rc + Fc + Fo
TreeModel = ctree(Fmla, data = Train)
plot(TreeModel)


