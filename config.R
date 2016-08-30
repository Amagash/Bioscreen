source("input.R")

defaultColors <- c("#ff0000", "#0000ff", "#008000", "#ffff00", "#ffc0cb", "#ff00ff", "#ffa500", "#a52a2a")
myColors <<- c()

#Just a reminder of codes for colors
# red = "#ff0000"
# blue = "#0000ff"
# green = "##008000"
# yellow = "#ffff00"
# pink = "#ffc0cb"
# purple = "#800080"
# fuchsia = "#ff00ff"
# orange = "#ffa500"
# brown = "#a52a2a"

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
