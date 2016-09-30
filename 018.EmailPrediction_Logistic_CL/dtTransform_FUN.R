#data transformation

dtTransform <- function(dataframe){
	#reformat data
			dataframe[,"riid"] <- as.character(dataframe[,"riid"]) #make riid as character

			columnName <- c("sent","opened","clicked","converted","complained")

			if(length(setdiff(columnName,colnames(dataframe))) == 0){
				col=c("Ro","Rc","Fo","Fc","sent","opened","clicked","converted","complained") #select columns that need to be transformed to factor
                dataframe[col] <- lapply(dataframe[col], factor)
                #dont include sent == 0
				dataframe <- filter(dataframe, sent != 0)
				
			}else{ col=c("Ro","Rc","Fo","Fc")
					 dataframe[col] <- lapply(dataframe[col], factor)
			}

                #select columns need to transform to numeric
                cols=c("lifetime_sent", "conversions")
                dataframe[cols] <- lapply(dataframe[cols], as.numeric)

				

				return(dataframe)

}
