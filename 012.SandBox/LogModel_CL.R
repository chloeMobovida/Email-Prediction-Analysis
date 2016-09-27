#clear environment if neccessary
rm(list = ls())

#load neccessary packages
#Note, you might need to install pacman first
pacman::p_load("party","ggplot2", "dplyr", "caret",
               "reshape2","data.table","lme4","lattice","plyr",
               "knitr", "cluster","grid","gridExtra","C50","e1071","pscl","ROCR","ResourceSelection")



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


Fmla = opened ~ Ro + Rc + Fc + Fo
LogModel01 <- glm(opened ~ Ro + Rc + Fc + Fo, data = Train, family = "binomial")
summary(LogModel01)


Sum_Model <- summary(LogModel01)                     


1 - pchisq((Sum_Model$null.deviance-Sum_Model$deviance), df=(Sum_Model$df.null - Sum_Model$df.residual))


anova(LogModel01, test="Chisq")



pR2(LogModel01)
hoslem.test(Train$opened, fitted(LogModel01))


#prediction
M01_prediction <- predict(LogModel01, Test[,2:5], type = "response")

Test$Pred_Open <- M01_prediction

write.csv(Test, "Test_withPred.csv")


#graph ROC and calculate AUC
pr <- prediction(M01_prediction, Test$opened)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






#drop Rc
Fmla_02 = opened ~ Ro + Fc + Fo
LogModel02 <- glm(Fmla_02, data = Train, family = "binomial")
summary(LogModel02)

Sum_Model02 <- summary(LogModel02)                     

1 - pchisq((Sum_Model02$null.deviance-Sum_Model02$deviance), df=(Sum_Model02$df.null - Sum_Model02$df.residual))

anova(LogModel02, test="Chisq")

anova(LogModel01,LogModel02)


#prediction
M02_prediction <- predict(LogModel02, Test[,2:5], type = "response")

#graph ROC and calculate AUC
pr <- prediction(M02_prediction, Test$opened)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc




#drop Fc

Fmla_03 = opened ~ Ro + Fo
LogModel03 <- glm(Fmla_03, data = Train, family = "binomial")
summary(LogModel03)

Sum_Model03 <- summary(LogModel03)                     

1 - pchisq((Sum_Model03$null.deviance-Sum_Model03$deviance), df=(Sum_Model03$df.null - Sum_Model03$df.residual))

anova(LogModel03, test="Chisq")

anova(LogModel01,LogModel02, LogModel03)


#prediction
M03_prediction <- predict(LogModel03, Test[,2:5], type = "response")

#graph ROC and calculate AUC
pr <- prediction(M03_prediction, Test$opened)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

