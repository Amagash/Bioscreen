source("input.R")

defaultColors <- c("#d6d6d6", "#ff0000", "#00ff00", "#0000ff", "#ff33cc", "#cc33ff", "#ffff00", "#ff9933", "#00ffff", "#cc6600", "#cc3300", "#cc3300", "#009933")
myColors <<- c()

getInputFile <- function() {
  return(inputFile)
}

getMediumList <- function() {
  return(mediumList)
}

getStrainList <- function() {
  strainList <- list()
  if (typeof(strains) == "character") {
    i<- 1
    colors <- c()
    
    for (color in strains) {
      colors[i] <- color
      i <- i + 1
    }
    strainList <- as.list(names(strains))
    myColors <<- colors
  } else if (typeof(strains) == "list") {
    strainList <- strains
    myColors <<- defaultColors
  } else {
    print("error")
    quit()
  }
  return(strainList)
}
