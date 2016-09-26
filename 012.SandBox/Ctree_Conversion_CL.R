#clear environment if neccessary
rm(list = ls())

#load neccessary packages
#Note, you might need to install pacman first
pacman::p_load("party","ggplot2", "dplyr", "caret",
               "reshape2","data.table","lme4","lattice","plyr",
               "knitr", "cluster","grid","gridExtra","C50","e1071")



#data import using ReadData function function
source("/Users/chloeli/Documents/02. EmailPrediction_CL/012.SandBox/ReadData_Fun_CL.R")

#this will return a data frame. So store it to a new vector
DT <- ReadData("/Users/chloeli/Documents/02. EmailPrediction_CL/011.Data", "eRFM_email_test1.csv")

#the following code is generalized for the same data structure
#for the nature of characteristics of different dataset (even with same structure)
#you still need to tweak some numbers for better model building purpose


#reformat data
#change multiple variables into factor type
cols = c(2, 3, 4,5,8,9,10,11,12,13)
DT[cols] <- lapply(DT[cols], factor)
DT$riid <- as.character(DT$riid)


#dont include sent == 0
DT <- filter(DT, sent != 0)


library(caret)
inTrain = createDataPartition(DT$converted, p = 0.75, list = FALSE)
Train=DT[inTrain,]
Test=DT[-inTrain,]

#summary(Train)

table(Train$converted)
table(Train$clicked)
table(Train$opened)

levels(DT$converted) <- c("Bad","Good")
#-------------------------------------------------------------------------------------------------------------------#
#                                              C50 Tree                                                             #                                               
#-------------------------------------------------------------------------------------------------------------------#


#------------------------Create a cost matrix to make passes and failure worth the same-----------------------------#

#cost matrix
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
#to identify customers who will convert but we predict will not, hurts us more than we think they will NOT convert but they actually will



costs <- matrix(
  c(0, nrow(subset(Train, converted == "Bad")), nrow(subset(Train, converted == "Good")), 0),
  nrow=2,
  ncol=2,
  byrow=TRUE, 
  dimnames=list(c("Bad", "Good"), c("Bad", "Good")))

#-----------------------------------------Build a Tree Model--------------------------------------------------------#
Tree_Model_01 <- C5.0(x=Train[,2:5], y=Train$converted, costs=costs, trials = 1, rules= FALSE)

plot(Tree_Model_01)

#use capture.output and later use addparagraph to add to pptx

Tree_Model_01

summary(Tree_Model_01)



#rule set
Tree_Model_02 <- C5.0(x=Train[,2:5], y=Train$converted, costs=costs, trials = 1, rules= TRUE)
summary(Tree_Model_02)


#plot tree
myTree <- C50:::as.party.C5.0(Tree_Model_02)
TreePlot01 <- plot(myTree)


#---------------------------------------Prediction On Same Data-----------------------------------------------------#
#------------------------------------C5.0 Decision Tree Performance-------------------------------------------------#

#predict on same data set
Tree.Pred <- predict(Tree_Model_01, Test[,2:5], type = "class")

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


