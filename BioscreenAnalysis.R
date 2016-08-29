#TiffanySouterre
#June 30 2016

#Script to analyse Bioscreen data output

library("ggplot2")
source("fonction.R")
source("config.R")

strainList <- getStrainList()
mediumList <- getMediumList()
inputFile <- getInputFile()

input <- read.csv(inputFile, header=FALSE, dec = ",", fileEncoding="utf-16", stringsAsFactors=FALSE)
# input <- sapply(input, gsub, pattern = ",", replacement= ".")
init(input, strainList, mediumList)
