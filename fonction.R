

analysisByStrain <- function(val, time){
  mgg <- summaryTriplicate (val,paste0(val,".1"),paste0(val,".2"))
  mggl <- summaryTriplicate (paste0(val,".3"),paste0(val,".4"),paste0(val,".5"))
  mggls <- summaryTriplicate (paste0(val,".6"),paste0(val,".7"),paste0(val,".8"))
  
  dataset <- data.frame(mgg, mggl, mggls)
  
  Plot <- strainPlot(time, dataset, val)
  print (Plot)
}

################################################################################

analysisByMedium <- function(val, time, list){
#   result = new.env()
#   for (strain in list){
#     env[[strain]] <- summaryTriplicate (strain,paste0(strain,".1"),paste0(strain,".2"))
#     
#   }
  
  #result <- matrix(ncol=2, nrow=length(list))
  #result <- list()
  i <- 1

#   for (strain in list){
#     result[i] = summaryTriplicate (strain,paste0(strain,".1"),paste0(strain,".2"))
#     i <- i + 1
#   }
#   browser()
#   dataset <- data.frame(result)
  
  result = new.env()
  for (strain in list){
    result[[strain]] <- summaryTriplicate (strain,paste0(strain,".1"),paste0(strain,".2"))
  }
  browser()
  datset <- as.data.frame(as.list(result))
  Plot <- mediumPlot(time, dataset, val)
  print (Plot)
  
  
  
}
analysisByMedium2 <- function(val, time){
  strain <- summaryTriplicate (paste0(val,".3"),paste0(val,".4"),paste0(val,".5"))
}
analysisByMedium3 <- function(val, time){
  strain <- summaryTriplicate (paste0(val,".6"),paste0(val,".7"),paste0(val,".8"))
}

#################################################################################

summaryTriplicate <- function(well1, well2, well3) {
  value <- c(well1, well2, well3)
  measure <- input[,value];
  mean <- rowMeans(measure[,1:3], na.rm = FALSE);
  stdbis <- transform(measure, SD=apply(measure,1, sd, na.rm = FALSE))
  std <- stdbis[, 4];
  data <- data.frame(mean, std);
  return(data)
}

# summaryHexaplicate <- function(well1, well2, well3, well4, well5, well6) {
#   value <- c(well1, well2, well3, well4, well5, well6)
#   measure <- input[,value];
#   mean <- rowMeans(measure[,1:6], na.rm = FALSE);
#   stdbis <- transform(measure, SD=apply(measure,1, sd, na.rm = FALSE))
#   std <- stdbis[, 7];
#   data <- data.frame(mean, std);
#   return(data)
# }

#strain_plot is for plotting all data about a strain grown in different media

#################################################################################




createStrainPlot <- function(strain, time, mgg, mggl, mggls) {
  dataset <- data.frame(mgg$`mean`, mgg$`std`, mggl$`mean`, mggl$`std`, mggls$`mean`, mggls$`std`)
 browser()
  strain_plot <- ggplot(dataset, aes(x=time[3:length(time)])) +
#     geom_line(aes(y = mgg.mean, colour = "var0")) +
#     geom_line(aes(y = mggl.mean, colour = "var1")) +
    geom_line(aes(y = mggls.mean, colour = "var2")) +
    xlab("hours") + ylab("OD(600)") + 
    ggtitle(paste(strain, "growth in different media"))+
    coord_cartesian(xlim=c(0,40), ylim=c(0,.7)) + 
    theme_bw() +
    theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
    
    
#     geom_line(aes(y=mgg.mean-mgg.std, color="#000000"))+
#     geom_line(aes(y=mgg.mean), size=0.1, color="#4C8A2E") + 
#     geom_point(aes(y=mgg.mean), size=0.5, color="#4C8A2E") + 
#     geom_line(aes(y=mgg.mean+mgg.std, color="#000000"))+
#     
#     geom_line(aes(y=mggl.mean-mggl.std, color="#000000"))+
#     geom_line(aes(y=mggl.mean), size=0.1, color="#201D58") + 
#     geom_point(aes(y=mggl.mean), size=1, color="#201D58") + 
#     geom_line(aes(y=mggl.mean+mggl.std, color="#000000"))+
    
    geom_line(aes(y=mggls.mean-mggls.std, color="#000000"))+
    geom_line(aes(y=mggls.mean), size=0.1, color="#7E2E2A") + 
    geom_point(aes(y=mggls.mean), size=1, color="#7E2E2A") + 
    geom_line(aes(y=mggls.mean+mggls.std, color="#000000"))
  
  return(strain_plot)
}

strainPlot <- function(time, dataset, strain) {
  
  strain_plot <- ggplot(aes(x=time , y=mean), data = dataset) +
  geom_line(size=0.1, color="#23337C") +
#   geom_line(aes(y = mean.1, colour = "var1")) +
#   geom_line(aes(y = mean.2, colour = "var2")) +
  xlab("hours") + ylab("OD(600)") + 
  ggtitle(paste(strain, "growth in different media"))+
  coord_cartesian(xlim=c(0,100), ylim=c(0,.7)) + 
  theme_bw() +
  theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  return(strain_plot)
  
  # geom_line(aes(y=mean-std, color="#000000"))+
#   geom_line(aes(y=mean), size=0.1, color="#4C8A2E") + 
#   geom_point(aes(y=mean), size=0.5, color="#4C8A2E") + 
#   geom_line(aes(y=mean+std, color="#000000"))+
#   
#   geom_line(aes(y=mean.1-std.1, color="#000000"))+
#   geom_line(aes(y=mean.1), size=0.1, color="#201D58") + 
#   geom_point(aes(y=mean.1), size=1, color="#201D58") + 
#   geom_line(aes(y=mean.1+std.1, color="#000000"))+
#   
#   geom_line(aes(y=mean.2-std.2, color="#000000"))+
#   geom_line(aes(y=mean.2), size=0.1, color="#7E2E2A") + 
#   geom_point(aes(y=mean.2), size=1, color="#7E2E2A") + 
#   geom_line(aes(y=mean.2+std.2, color="#000000"))
  

}

#################################################################################

mediumPlot <- function(time, dataset, strain) {
  
  medium_plot <- ggplot(dataset, aes(x=time)) +
    geom_line(aes(y = mean, colour = "var0")) +
    geom_line(aes(y = mean.1, colour = "var1")) +
    geom_line(aes(y = mean.2, colour = "var2")) +
    xlab("hours") + ylab("OD(600)") + 
    ggtitle(paste(strain, "growth in different media"))+
    coord_cartesian(xlim=c(0,120), ylim=c(0,.75)) + 
    theme_bw() +
    theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
    
    
    geom_line(aes(y=mean-std, color="#000000"))+
    geom_line(aes(y=mean), size=0.1, color="#4C8A2E") + 
    geom_point(aes(y=mean), size=0.5, color="#4C8A2E") + 
    geom_line(aes(y=mean+std, color="#000000"))+
    
    geom_line(aes(y=mean.1-std.1, color="#000000"))+
    geom_line(aes(y=mean.1), size=0.1, color="#201D58") + 
    geom_point(aes(y=mean.1), size=1, color="#201D58") + 
    geom_line(aes(y=mean.1+std.1, color="#000000"))+
    
    geom_line(aes(y=mean.2-std.2, color="#000000"))+
    geom_line(aes(y=mean.2), size=0.1, color="#7E2E2A") + 
    geom_point(aes(y=mean.2), size=1, color="#7E2E2A") + 
    geom_line(aes(y=mean.2+std.2, color="#000000"))
  
  return(medium_plot)
}

##########################################################################
#This function generates a table that gather all data about one strain

generateStrainTable <- function (input, strain, time){
  i<-1
  j<-1
  #dataset <- data.frame(time);

  dataset <- data.frame(matrix(ncol = 0, nrow = length(time)))
  for (well in input[1, ]){
    if (well == strain){
      table <- input[,paste0("V",i)]
      #dataset <- data.frame(dataset, table)
      dataset[,paste0("V",j)]<-table
      j<-j+1
    }
    i<-i+1
  }
  return(dataset)
}

##########################################################################
#This function generates a table that gather all data about one medium

generateMediumTable <- function (input, medium, time){
  i<-1
  j<-1
  #dataset <- data.frame(time);
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
#This function generates a table that calculates the mean and std for one strain in one medium
#from scratch
masterStrainMediumSummary<- function (input, strain, medium, time){
  
  gStrain <- generateStrainTable (input, strain, time)
  gStrainmedium <- generateMediumTable(gStrain, medium, time)
  gStrainmediumSummary <- summaryReplicate(gStrainmedium, time)
  return(gStrainmediumSummary)
}

#########################################################################

masterSummary <- function (input, strainList, mediumList, time){
  map <- new.env(hash=T, parent=emptyenv())
  
  for(strain in strainList){
    for(medium in mediumList){
      map[[paste0(strain, medium)]] <- masterStrainMediumSummary (input, strain, medium, time)
    }
  }
  return(map)
}

############################################################################
init <- function(input, strainList, mediumList) {
  
  time <- input[, 1]
  time2 <- as.numeric(as.character(time [3:length(time)]))
  
  a <- masterSummary (input, strainList, mediumList, time)
  
  for (strain in strainList) {
    print (strain)
    # dataset <- data.frame(matrix(ncol = 0, nrow = length(time2)))
    dataset <- data.frame(time2)
    # dataset <- new.env(hash=T, parent=emptyenv())
    for (medium in mediumList) {
      print (medium)
      #dataset <- data.frame(dataset, table)
    
      dataset[,paste0(strain, medium, ".mean")]<-a[[paste0(strain, medium)]]$`mean`
      dataset[,paste0(strain, medium, ".std")]<-a[[paste0(strain, medium)]]$`std`
      
      # dataset[[paste0(strain, medium)]] <- data.frame(a[[paste0(strain, medium)]])
    }
    
    graph <- generateGraphByStain(time2, strain, mediumList, dataset)
    print(graph)
  }
}

generateGraphByStain <- function(time2, strain, mediumList, dataset) {

  strainplot <- ggplot(dataset, aes(x=time2)) +
    xlab("hours") + ylab("OD(600)") + 
    ggtitle(paste(strain, "growth in different media"))+
    coord_cartesian(xlim=c(0,120), ylim=c(0,.75)) + 
    theme_bw() +
    theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    scale_color_manual(name="Val", values=c(color0="#d6d6d6", color1="#ff0000",color2="#00ff00", color3="#0000ff"))
    
    i <- 1
    for (medium in mediumList) {
      strainplot <- strainplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]], color=shQuote(paste0("color",i))))
      strainplot <- strainplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]] - dataset[[paste0(strain, medium, '.std')]], color=shQuote("color0")))
      strainplot <- strainplot + geom_line(aes_string(y = dataset[[paste0(strain, medium, '.mean')]] + dataset[[paste0(strain, medium, '.std')]], color=shQuote("color0")))
      i <- i + 1
    }
    
  return(strainplot)
}
