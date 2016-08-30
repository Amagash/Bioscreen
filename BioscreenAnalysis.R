#TiffanySouterre
#June 30 2016

#Script to analyse Bioscreen data output
#Before running this script make sure :
# 1) In your input, decimales are with a dot "." not a coma ","
# 2) In your input is a .csv file with separators being comas ","
# 3) In your input, the time is in decimal (0 ; 0.25 ; 0.5 ...) and not in 00 : 00 : 00
# 4) You filled the input.R with the strains you want to analyse
# 5) You filled the input.R with the media you want to analyse your strains with
# 6) You entered the name of your input in the input.R
# 6) You have a file called "graphs" in your bioscreen folder

library("ggplot2")
source("fonction.R")
source("config.R")

strainList <- getStrainList()
mediumList <- getMediumList()
inputFile <- getInputFile()

#input <- read.csv(inputFile, header=FALSE, dec = ".", stringsAsFactors=FALSE)
input <- read.csv2(inputFile, header=FALSE, dec = ".", stringsAsFactors=FALSE)

init(input, strainList, mediumList)

