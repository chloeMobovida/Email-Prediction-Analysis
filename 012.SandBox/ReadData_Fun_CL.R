#simple way to import csv data file from your local working directory
#all arguments are in character string
#fileName should contain .csv extension

ReadData <- function(wd, fileName){
  setwd(wd)
  read.csv(fileName)
}