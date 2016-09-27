#data transformation

dtTransform <- function(dataframe){
	#reformat data
	#change multiple variables into factor type
	cols = c(2, 3, 4,5,8,9,10,11,12,13)
	DT[cols] <- lapply(DT[cols], factor)
	DT$riid <- as.character(DT$riid)


	#dont include sent == 0
	DT <- filter(DT, sent != 0)

}
