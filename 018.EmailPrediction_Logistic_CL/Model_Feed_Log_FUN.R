#this function created is help you view the logistic regression models' goodness of fit 
#by inputting master dataset, formula
#output: append prediction to master/new master dataset, return equation, AUC, p-value/AIC, deviance etc. for measurement

logit_Model_feed <- function(Newdataframe, Model, dependentVar,independentVar){
	#dataframe is the master dataset where you would like to do data partitioning and analysis on
	#proper data type transformation has to be done on the dataframe before running this function
	#partitionPercent is the percentage that you want to allocate your master to Train set
	#dependentVar is dependent variable name in character string
	#indepedentVar is independent variables name in a vector of character string
	#formula is a vector of formual for modeling: e.g. formula = DV + X1 + X2+ X3 + ...
  	#probability is the percentage (cutoff) to identify target variable's class

  	#prediction on new dataframe using previous model built
  	pred <- predict(Model, Newdataframe[,independentVar], type = "response")
  	Newdataframe[,paste(dependentVar,"_pred",sep="")] <- predict(Model, Newdataframe[,independentVar], type = "response")

	set.seed(12345) #set.seed can be any number. This is just to ensure the result will be the same each time you run the following function
	
	#measurement of goodness of fit
	#ROC <- plot(prf, colorize = TRUE) 
	#Receiver Operating Characteristic(ROC) summarizes the model’s performance by evaluating the trade offs between true positive rate (sensitivity) and false positive rate(1- specificity). 
	#auc <- performance(prediction(pred, Newdataframe[,dependentVar]), measure = "auc")
	#auc <- auc@y.values[[1]]


	#print(paste("AUC on Train set ", aucTrain, sep=""))

	#ROC summarizes the predictive power for all possible values of p > 0.5.  The area under curve (AUC), referred to as index of accuracy(A) or concordance index, is a perfect performance metric for ROC curve. 
	#Higher the area under curve, better the prediction power of the model. 
  

	#create a list to stroe output
	#result <- list(dataframe = Newdataframe)#, AUC = paste("AUC is ", auc, sep=""))

	return(Newdataframe)	


}