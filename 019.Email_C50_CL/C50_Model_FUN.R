#function to run C5.0 tree
#applicable for different formula input

C50_Model <- function(dataframe, partitionPercent, dependentVar, independentVar,trialsNum, rules){

	 inTrain = createDataPartition(dataframe[,dependentVar], p = partitionPercent, list = FALSE)
                Train=dataframe[inTrain,]
                Test=dataframe[-inTrain,]



	#------------------------Create a cost matrix to make passes and failure worth the same-----------------------------#

	#cost matrix
	#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
	#to identify customers who will convert but we predict will not, hurts us more than we think they will NOT convert but they actually will


	costs <- matrix(
		  c(0, nrow(subset(Train, Train[,dependentVar] == "0")), nrow(subset(Train, Train[,dependentVar] == "1")), 0),
		  nrow=2,
		  ncol=2,
		  byrow=TRUE, 
		  dimnames=list(c("0", "1"), c("0", "1")))


	#-----------------------------------------Build a Tree Model--------------------------------------------------------#
		Tree <- C5.0(x=Train[,independentVar], y=Train[,dependentVar], costs=costs, trials = trialsNum, rules= rules)
		#print(summary(Tree))

	#-----------------------------------------Predict on Test Set-------------------------------------------------------#
	#---------------------------------------Prediction On Same Data-----------------------------------------------------#
	#------------------------------------C5.0 Decision Tree Performance-------------------------------------------------#

	#predict on same data set
	Tree.Predtest <- predict(Tree, Test[,independentVar], type = "class") #type="prob" won't work with cost matrix in C5.0 tree

	#add predicted value to original data set
	Test[,paste(dependentVar,"_pred",sep="")] <- Tree.Predtest


	#Confusion Matrix
	#evaluate the goodness of fit of the model on test set

	#print(confusionMatrix(Tree.Pred, Test[,dependentVar]))

	#save all output into a list
	#use result_Name$... to call out the output
	result <- list(Summary = summary(Tree), confusionMatrix = confusionMatrix(Tree.Predtest, Test[,dependentVar]),Test_dt = Test)

	#this function return the whole Test set including the prediction
	return(result)

}
