#this function created is help you view the logistic regression models' goodness of fit 
#by inputting master dataset, formula
#output: append prediction to master/new master dataset, return equation, AUC, p-value/AIC, deviance etc. for measurement

logit_Model <- function(dataframe, partitionPercent, dependentVar, independentVar, formula, Probability){
	#dataframe is the master dataset where you would like to do data partitioning and analysis on
	#proper data type transformation has to be done on the dataframe before running this function
	#partitionPercent is the percentage that you want to allocate your master to Train set
	#dependentVar is dependent variable name in character string
	#indepedentVar is independent variables name in a vector of character string
	#formula is a vector of formual for modeling: e.g. formula = DV + X1 + X2+ X3 + ...
  	#probability is the percentage (cutoff) to identify target variable's class


	set.seed(12345) #set.seed can be any number. This is just to ensure the result will be the same each time you run the following function
	
	inTrain = createDataPartition(dataframe[,dependentVar], p = partitionPercent, list = FALSE)
	#index the separation based on p
	Train=dataframe[inTrain,] #assign to Train set, ready for modeling
	Test=dataframe[-inTrain,] #assign to Test set, ready for prediction

	Model <- glm(formula, data = Train, family = "binomial") #run logistic regression on Train set

	Sum_Model <- summary(Model)
	#aic <- Sum_Model$aic
	#AIC (Akaike Information Criteria) – The analogous metric of adjusted R² in logistic regression is AIC. 
	#AIC is the measure of fit which penalizes model for the number of model coefficients. Therefore, we always prefer model with minimum AIC value.

	#Null Deviance indicates the response predicted by a model with nothing but an intercept. Lower the value, better the model. 
	#Residual deviance indicates the response predicted by a model on adding independent variables. Lower the value, better the model.


	#calculating the P-value - significant different between null model and proposed model
	#degrees of freedom = no. of observations – no. of predictors
	#p-value = 1 - pchisq(deviance, degrees of freedom)
	pValue <- with(Model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
	#p value of 0 means there is a significant lack of evidence to support the null hypothesis

	#prediction on both sets
	Mtrain_prediction <- predict(Model, Train[,independentVar], type = "response")
	Mtest_prediction <- predict(Model, Test[,independentVar], type = "response")
	
	Train[,paste(dependentVar,"_pred", sep="")] <- Mtrain_prediction #append to original dataframe
	Test[,paste(dependentVar,"_pred", sep="")] <- Mtest_prediction #append to original dataframe

	
	#ROC <- plot(prf, colorize = TRUE) 
	#Receiver Operating Characteristic(ROC) summarizes the model’s performance by evaluating the trade offs between true positive rate (sensitivity) and false positive rate(1- specificity). 
	aucTrain <- performance(prediction(Mtrain_prediction, Train[,dependentVar]), measure = "auc")
	aucTrain <- aucTrain@y.values[[1]]
	#print(paste("AUC on Train set ", aucTrain, sep=""))

	aucTest <- performance(prediction(Mtest_prediction, Test[,dependentVar]), measure = "auc")
	aucTest <- aucTest@y.values[[1]]
	#print(paste("AUC on Test set ", aucTest, sep=""))

	#ROC summarizes the predictive power for all possible values of p > 0.5.  The area under curve (AUC), referred to as index of accuracy(A) or concordance index, is a perfect performance metric for ROC curve. 
	#Higher the area under curve, better the prediction power of the model. 
  
	#Confusion Matrix on Train set
	pred.logit2 <- rep('0',length(Mtrain_prediction))
	pred.logit2[Mtrain_prediction>=Probability] <- '1'
	
	#print("Confusion Matrix on Train set")
	#print(table(pred.logit2, Train[,dependentVar]))
	#print(confusionMatrix(pred.logit2, Train[,dependentVar]))

	
	# Confusion Matrix on the test set
	pred.logit <- rep('0',length(Mtest_prediction))
	pred.logit[Mtest_prediction>=Probability] <- '1'
	
	#print("Confusion Matrix on Test set")
	#print(table(pred.logit, Test[,dependentVar]))
	#print(confusionMatrix(Test[,dependentVar], pred.logit))
	

	#create a list to stroe output
	result <- list(Model = Model, Summary = Sum_Model, AIC =  Sum_Model$aic, Pvalue = pValue, AUC_train = paste("AUC on Train set ", aucTrain, sep=""), AUC_test = paste("AUC on Test set ", aucTest, sep=""),
		CM_train = confusionMatrix(pred.logit2, Train[,dependentVar]), CM_test = confusionMatrix(Test[,dependentVar], pred.logit), dataframe = rbind(Train, Test))

	return(result)	


}