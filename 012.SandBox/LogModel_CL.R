rm(list = ls())


#set your working directory
setwd("~/Documents/02. EmailPrediction_CL/018.EmailPrediction_Logistic_CL/")

#grep data insert data name
DT <- read.csv("./Data/eRFM_email_test1.csv")

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


set.seed(12345)
inTrain = createDataPartition(DT$opened, p = 0.75, list = FALSE)
Train=DT[inTrain,]
Test=DT[-inTrain,]


Fmla = opened ~ Ro + Rc + Fc + Fo + conversions + lifetime_sent +clusterNum
LogModel01 <- glm(Fmla, data = Train, family = "binomial")






summary(LogModel01)


Sum_Model <- summary(LogModel01)                     


1 - pchisq((Sum_Model$null.deviance-Sum_Model$deviance), df=(Sum_Model$df.null - Sum_Model$df.residual))


with(LogModel01, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



anova(LogModel01, test="Chisq")



pR2(LogModel01)
hoslem.test(Train$opened, fitted(LogModel01))


#prediction
M01_prediction <- predict(LogModel01, Test[,2:8], type = "response")

Test$Pred_Open <- M01_prediction

pred.logit2 <- rep('1',length(M01_prediction))
pred.logit2[M01_prediction>=0.5] <- '0'

#print("Confusion Matrix on Train set")
confusionMatrix(Test[,"opened"], pred.logit2)

#graph ROC and calculate AUC
pr <- prediction(M01_prediction, Test$opened)

auc <- performance(prediction(M01_prediction, Test$opened), measure = "auc")
auc <- auc@y.values[[1]]
auc



M02_prediction <- predict(LogModel01, Train[,2:5], type = "response")


#graph ROC and calculate AUC
pr <- prediction(M02_prediction, Train$opened)
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
plot(prf,colorize = TRUE)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


table(Train$opened, M02_prediction > 0.5)

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

