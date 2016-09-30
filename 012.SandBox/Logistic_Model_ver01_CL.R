install.packages("pROC")
library(pROC)



setwd("/Users/chloeli/Documents/02. EmailPrediction_CL/011.Data")


EmailData <- read.csv("eRFM_email_test1.csv")


#reformat data

#change multiple variables into factor type
cols = c(2, 3, 4,5,8,9,10,11,12,13)
EmailData[cols] <- lapply(EmailData[cols], factor)
EmailData$riid <- as.character(EmailData$riid)


levels(EmailData$converted) <- make.names(levels(factor(EmailData$converted)))



#dont include sent == 0
EmailData <- filter(EmailData, sent != 0)


library(caret)
inTrain = createDataPartition(EmailData$converted, p = 0.75, list = FALSE)
Train=EmailData[inTrain,]
Test=EmailData[-inTrain,]



Log_Model01 <- glm(converted ~ Ro + Rc + Fo + Fc, data = Train, family=binomial("logit"))
  

Sum_Model <- summary(Log_Model01)                     
                       
                       
1 - pchisq((Sum_Model$null.deviance-Sum_Model$deviance), df=(Sum_Model$df.null - Sum_Model$df.residual))
                       

anova(Log_Model01, test="Chisq")


#prediction
M01_prediction <- predict(Log_Model01, Test[,2:5], type = "response")

Test$Pred_Open <- M01_prediction




#graph ROC and calculate AUC
pr <- prediction(M01_prediction, Test$converted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc









cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


set.seed(35)
glm.tune.1 <- train(converted ~ Ro + Rc + Fo + Fc, data = Train,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)



#http://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
#https://stat.ethz.ch/pipermail/r-help/2008-March/156868.html

