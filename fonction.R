
colors <- c("#d6d6d6", "#ff0000", "#00ff00", "#0000ff", "#ff33cc", "#cc33ff", "#ffff00", "#ff9933", "#00ffff", "#cc6600", "#cc3300", "#cc3300", "#009933")

##########################################################################
#This function generates a table that gather all data about one strain for all medium
#raw input -> generateStrainTable

generateStrainTable <- function (input, strain, time){
  i<-1
  j<-1

  dataset <- data.frame(matrix(ncol = 0, nrow = length(time)))
  for (well in input[1, ]){
    if (well == strain){
      table <- input[,paste0("V",i)]
      dataset[,paste0("V",j)]<-table
      j<-j+1
    }
    i<-i+1
  }
  return(dataset)
}

##########################################################################
#This function generates a table that gather all data about one medium for all strains
#raw input -> generateMediumTable

generateMediumTable <- function (input, medium, time){
  i<-1
  j<-1
  dataset <- data.frame(matrix(ncol = 0, nrow = length(time)))
  for (well in input[2, ]){
    if (well == medium){
      table <- input[,paste0("V",i)]
      dataset[,paste0("V",j)]<-table
      j<-j+1
    }
    i<-i+1
  }
  return(dataset)
}

##########################################################################
#This function generates a table that calculates the mean and std for one strain in one medium 
##raw input -> generateStrainTable -> generateMediumTable -> summaryReplicate

summaryReplicate <- function (input, time){
    convertToMatrix <- data.frame(data.matrix(input[3:length(time),]))
    time2 <- time [3:length(time)]
    stdbis <- transform(convertToMatrix, SD=apply(convertToMatrix,1, sd, na.rm = FALSE))
    std <- stdbis[, length(stdbis)]
    mean <- rowMeans(convertToMatrix)
    data <- data.frame(mean, std);
    return(data)
}

##########################################################################
#This function generates a table that calculates the mean and std for one strain in one medium from raw input
#raw input -> masterStrainMediumSummary

masterStrainMediumSummary<- function (input, strain, medium, time){
  
  gStrain <- generateStrainTable (input, strain, time)
  gStrainmedium <- generateMediumTable(gStrain, medium, time)
  gStrainmediumSummary <- summaryReplicate(gStrainmedium, time)
  return(gStrainmediumSummary)
}

#########################################################################
# This function generated a table with all mean and std calculated for all strains in all medium from raw input
#raw input -> masterSummary
masterSummary <- function (input, strainList, mediumList, time){
  map <- new.env(hash=T, parent=emptyenv())
  
  for(strain in strainList){
    for(medium in mediumList){
      map[[paste0(strain, medium)]] <- masterStrainMediumSummary (input, strain, medium, time)
    }
  }
  return(map)
}

###########################################################################
#generate all growth curve of the same strain in all media

generateGraphByStrain <- function(time2, strain, mediumList, dataset) {
  i <- 1
  mediumColor <- c()
  
  for (medium in mediumList){
    mediumColor[medium] <- colors[i]
    i <- i+1
  }
  
  strainplot <- ggplot(dataset, aes(x=time2)) +
    xlab("hours") + 
    ylab("OD(600)") + 
    ggtitle(paste(strain, "growth in different media"))+
    coord_cartesian(xlim=c(0,120), ylim=c(0,.75)) + 
    theme_bw() +
    theme(legend.key = element_blank(), aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    scale_color_manual(name="media", values=mediumColor)
    i <- 1
    for (medium in mediumList) {
    strainplot <- strainplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]], color=shQuote(medium)))
#     strainplot <- strainplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]] - dataset[[paste0(strain, medium, '.std')]], color=shQuote("color0")))
#     strainplot <- strainplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]] + dataset[[paste0(strain, medium, '.std')]], color=shQuote("color0")))
    i <- i + 1
  }
  return(strainplot)
}

############################################################################
#generate all growth curve of all strains in the same medium

generateGraphByMedium <- function(time2, medium, strainList, dataset) {
   i <- 1
  strainColor <- c()
  
  for (strain in strainList){
    strainColor[strain] <- colors[i]
    i <- i+1
  }
  
  mediumplot <- ggplot(dataset, aes(x=time2)) +
    xlab("hours") +
    ylab("OD(600)")+ 
    ggtitle(paste(medium, "lala"))+
    coord_cartesian(xlim=c(0,30), ylim=c(0,.75)) +
    theme_bw() +
    theme(legend.key = element_blank(), aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
    scale_color_manual(name="Strains", values=strainColor)
  i <- 1
  for (strain in strainList) {
    
    mediumplot <- mediumplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]], color=shQuote(strain)))
    #       mediumplot <- mediumplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]] - dataset[[paste0(strain, medium, '.std')]], color=shQuote("color0")))
    #       mediumplot <- mediumplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]] + dataset[[paste0(strain, medium, '.std')]], color=shQuote("color0")))
    i <- i + 1
  }
  
  return(mediumplot)
}
############################################################################
#Create a table with all means and std for one strain


############################################################################
init <- function(input, strainList, mediumList) {
  
  time <- input[, 1]
  time2 <- as.numeric(as.character(time [3:length(time)]))
  
  a <- masterSummary (input, strainList, mediumList, time)
  
  
  
  for (strain in strainList) {
    datasetByStrain <- data.frame(time2)
    for (medium in mediumList) {
      datasetByStrain[,paste0(strain, medium, ".mean")]<-a[[paste0(strain, medium)]]$`mean`
      datasetByStrain[,paste0(strain, medium, ".std")]<-a[[paste0(strain, medium)]]$`std`
    }
    graphByStrain <- generateGraphByStrain(time2, strain, mediumList, datasetByStrain)
    print (graphByStrain)
  }
  
  for (medium in mediumList) {
    datasetByMedium <- data.frame(time2)
    for (strain in strainList) {
      datasetByMedium[,paste0(strain, medium, ".mean")]<-a[[paste0(strain, medium)]]$`mean`
      datasetByMedium[,paste0(strain, medium, ".std")]<-a[[paste0(strain, medium)]]$`std`
    }
  
    graphByMedium <- generateGraphByMedium(time2, medium, strainList, datasetByMedium)
    print (graphByMedium)
  }
}

