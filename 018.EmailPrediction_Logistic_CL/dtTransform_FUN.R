#data transformation

dtTransform <- function(dataframe){
	#reformat data
	#change multiple variables into factor type
	cols = c(2, 3, 4,5,8,9,10,11,12,13)
	dataframe[cols] <- lapply(dataframe[cols], factor)
	dataframe$riid <- as.character(dataframe$riid)
  
	numcol = c(6,7)
	dataframe[numcol] <- lapply(dataframe[numcol], as.numeric)

	#dont include sent == 0
	dataframe <- filter(dataframe, sent != 0)

	return(dataframe)

}
