#TiffanySouterre
#June 30 2016

#Script to analyse Bioscreen data output

library("ggplot2");
library("gridExtra");
library("dplyr");
#library("plotrix");

source("fonction.R")
# 
# input <- read.csv("./data/kdb.csv", header=FALSE, dec = ".", stringsAsFactors=FALSE)
# strainList <- list("3936","4050","3986", "4097", "4096", "3700", "4160", "4255")
# mediumList <- list("mgg", "mggl", "mggls")

input <- read.csv("./data/IDOmetabo.csv", header=FALSE, dec = ".", stringsAsFactors=FALSE)
strainList <- list("3990","4136","4110", "4170", "4172", "4172", "4173", "4174", "4175")
mediumList <- list("Base", "Base ile", "Base ile suc", "Base ile nor")


init(input, strainList, mediumList)