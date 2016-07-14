#TiffanySouterre
#June 30 2016

#Script to analyse Bioscreen data output

library("ggplot2");
library("foreach");

source("fonction.R")
source("config.R")

strainList <- getStrainList()
mediumList <- getMediumList()
inputFile <- getInputFile()

input <- read.csv(inputFile, header=FALSE, dec = ".", stringsAsFactors=FALSE)

init(input, strainList, mediumList)


# 
# 
# 
# 
# 
# 
# 
# 
# 
# #   #geom_line(size=0.1, color="#23337C") +
# #   geom_line(aes(x=dataset$time2, y=dataset$mean, color="gray50"))+
# #   #   geom_line(aes(y = mean.1, colour = "var1")) +
# #   #   geom_line(aes(y = mean.2, colour = "var2")) +
# #   xlab("hours") + ylab("OD(600)") + 
# #   #ggtitle(paste(strain, "growth in different media"))+
# #   coord_cartesian(xlim=c(0,40), ylim=c(0,.7)) + 
# #   theme_bw() +
# #   theme(legend.position = "none", aspect.ratio=1, panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#   
# strainplot
# 
# arabinanPlot <- ggplot(aes(x= time, y= mean), data = adata) + 
#   geom_line(size=0.1, color="#23337C") + 
#   geom_point(size=1, color="#23337C") + 
#   theme_bw() + 
#   xlab("hours") + ylab("OD(600)") + 
#   geom_line(aes(x=adata$time, y=adata$mean-adata$std, color="gray50"))+
#   geom_line(aes(x=adata$time, y=adata$mean+adata$std, color="gray50"))+
#   coord_cartesian(xlim=c(-0.5,40), ylim=c(0,.9)) + 
#   ggtitle("arabinan")+
#   theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# arabinanPlot
# 
# 
# 
# #graph <- strainPlot(time2, dataset, "4096")
# 
# 
# # graph <- createStrainPlot("3936", time, a$`3936mgg`, a$`3936mggl`, a$`3936mggls`)
# # print(graph)
# 
# # map$`4096mggl`
# # map <- new.env(hash=T, parent=emptyenv())
# # 
# # for(strain in strainList){
# #   for(medium in mediumList){
# #     map[[paste0(strain, medium)]] <- masterSummary (input, strain, medium, time)
# #   }
# # }
# 
# data.frame(map)
# 
# value <- length(g3936mgg)
# value
# 
# RM<-rowMeans(DF[,2:4])
# 
# stdbis <- transform(input, SD=apply(input,1, sd, na.rm = FALSE))
# std <- stdbis[, length(input)];
# data <- data.frame(mean, std);
# 
# summary <- summaryReplicate (g3936mgg)
# summary
# 
# # i<-1
# # dataset <- data.frame(time);
# # for (well in input[1, ]){
# #   if (well == "3936"){
# #     table <- input[,paste0("V",i)]
# #     dataset <- data.frame(dataset, table)
# #   }
# #   i<-i+1
# # }
# # 
# # dataset
# 
# 
# for (well in input[1, ]){
#   if (well == "3936"){
#     cola <- input[,"3936"]}
#   cola
# }
# 
# col <- input[,paste0("V",i)]
# 
# dataset <- data.frame(dataset,col)
# 
#    if (well == "3936"){
# #     #table <- data.frame(mean, std);
#     print (well)
#    }
# 
# 
# 
# generateStrainTable <- function (strain, list){
#   for (well in list[1, ]){
#     if (well = strain){
#       table <- data.frame(mean, std);
#       print (well)
#     }
#   }
#   
# 
#   
#   measure <- input[,value];
# #   merge(x,y) fusionne 2 data frames en utilisant leurs noms
# #   de colonnes en commun (ou en les d?signant avec by.x
# #                          et by.y)
#   
#   for (well in firstRow){
#     if (well = strain){
#       
#     }
#         result[i] = summaryTriplicate (strain,paste0(strain,".1"),paste0(strain,".2"))
#         i <- i + 1
#       }
#   
# }
# 
# summaryTriplicate <- function(well1, well2, well3) {
#   value <- c(well1, well2, well3)
#   measure <- input[,value];
#   mean <- rowMeans(measure[,1:3], na.rm = FALSE);
#   stdbis <- transform(measure, SD=apply(measure,1, sd, na.rm = FALSE))
#   std <- stdbis[, 4];
#   data <- data.frame(mean, std);
#   return(data)
# }
# 
# 
# strainList <- list("X3936","X4050","X3986", "X4097", "X4096", "X3700", "X4160", "X4255")
# # for (n in 0:2){
# #   analysisByMedium (n, time, strainList)
# #   print (n)
# # }
# 
# analysisByMedium (0, time, strainList)
# 
# #3936
# strainList <- list("X3936","X4050","X3986", "X4097", "X4096", "X3700", "X4160", "X4255")
# for (n in strainList){
#   analysisBystrain (n, time)
#   print (n)
# }
# 
# 
# # 
# # 
# # #4050
# # 
# # g4050mgg <- summaryTriplicate ("X4050","X4050.1","X4050.2")
# # g4050mggl <- summaryTriplicate ("X4050.3","X4050.4","X4050.5")
# # g4050mggls <- summaryTriplicate ("X4050.6","X4050.7","X4050.8")
# # 
# # g4050 <- data.frame(g4050mgg, g4050mggl, g4050mggls);
# # 
# # g4050Plot <- strainPlot(time, g4050);
# # g4050Plot
# # 
# # #3936
# # 
# # g3936mgg <- summaryTriplicate ("X3936","X3936.1","X3936.2")
# # g3936mggl <- summaryTriplicate ("X3936.3","X3936.4","X3936.5")
# # g3936mggls <- summaryTriplicate ("X3936.6","X3936.7","X3936.8")
# # 
# # g3936 <- data.frame(g3936mgg, g3936mggl, g3936mggls);
# # 
# # strainPlot <- strainPlot(time, g3936);
# # strainPlot
# # 
# # 
# # # inputkdb <- read.csv("./kdb.csv", header=TRUE, dec = ",")
# # # 
# # # valueMgg <- c("X3936","X3936.1","X3936.2")
# # 
# # # 
# # # measureMgg <- inputkdb[,valueMgg];
# # # meanMgg <- rowMeans(measureMgg[,1:3], na.rm = FALSE);
# # # std1 <- transform(measureMgg, SD=apply(measureMgg,1, sd, na.rm = FALSE))
# # # stdMgg <- std1[, 4];
# # 
# # # 
# # # #Mggl
# # # 
# # # valueMggl <- c("X3936.3","X3936.4","X3936.5")
# # # time <- inputkdb[, 1];
# # # 
# # # measureMggl <- inputkdb[,valueMggl];
# # # meanMggl <- rowMeans(measureMggl[,1:3], na.rm = FALSE);
# # # std2 <- transform(measureMggl, SD=apply(measureMggl,1, sd, na.rm = FALSE))
# # # stdMggl <- std2[, 4];
# # # 
# # # #Mggls
# # # 
# # # valueMggls <- c("X3936.6","X3936.7","X3936.8")
# # # time <- inputkdb[, 1];
# # # 
# # # measureMggls <- inputkdb[,valueMggls];
# # # meanMggls <- rowMeans(measureMggls[,1:3], na.rm = FALSE);
# # # std3 <- transform(measureMggls, SD=apply(measureMggls,1, sd, na.rm = FALSE))
# # # stdMggls <- std3[, 4];
# # 
# # 
# # g3936 <- data.frame(time, meanMgg, stdMgg, meanMggl, stdMggl, meanMggls, stdMggls);
# # 
# # #Plot
# # 
# # g3936Plot <- ggplot(g3936, aes(x=time)) +
# #     geom_line(aes(y = meanMgg, colour = "var0")) +
# #     geom_line(aes(y = meanMggl, colour = "var1")) +
# #     geom_line(aes(y = meanMggls, colour = "var2")) +
# #     xlab("hours") + ylab("OD(600)") + 
# #     ggtitle("G3936 growth")+
# #     coord_cartesian(xlim=c(0,40), ylim=c(0,.75)) + 
# #     theme_bw() +
# #     theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
# # 
# # 
# #     geom_line(aes(y=meanMggls-stdMggls, color="#000000"))+
# #     geom_line(aes(y=meanMggls), size=0.1, color="#4C8A2E") + 
# #     geom_point(aes(y=meanMggls), size=0.5, color="#4C8A2E") + 
# #     geom_line(aes(y=meanMggls+stdMggls, color="#000000"))+
# #     
# #     geom_line(aes(y=meanMggl-stdMggl, color="#000000"))+
# #     geom_line(aes(y=meanMggl), size=0.1, color="#201D58") + 
# #     geom_point(aes(y=meanMggl), size=1, color="#201D58") + 
# #     geom_line(aes(y=meanMggl+stdMggl, color="#000000"))+
# #       
# #     geom_line(aes(y=meanMgg-stdMggl, color="#000000"))+
# #     geom_line(aes(y=meanMgg), size=0.1, color="#7E2E2A") + 
# #     geom_point(aes(y=meanMgg), size=1, color="#7E2E2A") + 
# #     geom_line(aes(y=meanMgg+stdMggl, color="#000000"))
# # 
# # g3936Plot
# # 
# # #G4050
# # #Mgg
# # 
# # valueMgg <- c("X4050","X4050.1","X4050.2")
# # time <- inputkdb[, 1];
# # 
# # measureMgg <- inputkdb[,valueMgg];
# # meanMgg <- rowMeans(measureMgg[,1:3], na.rm = FALSE);
# # std1 <- transform(measureMgg, SD=apply(measureMgg,1, sd, na.rm = FALSE))
# # stdMgg <- std1[, 4];
# # 
# # 
# # #Mggl
# # 
# # valueMggl <- c("X4050.3","X4050.4","X4050.5")
# # time <- inputkdb[, 1];
# # 
# # measureMggl <- inputkdb[,valueMggl];
# # meanMggl <- rowMeans(measureMggl[,1:3], na.rm = FALSE);
# # std2 <- transform(measureMggl, SD=apply(measureMggl,1, sd, na.rm = FALSE))
# # stdMggl <- std2[, 4];
# # 
# # #Mggls
# # 
# # valueMggls <- c("X4050.6","X4050.7","X4050.8")
# # time <- inputkdb[, 1];
# # 
# # measureMggls <- inputkdb[,valueMggls];
# # meanMggls <- rowMeans(measureMggls[,1:3], na.rm = FALSE);
# # std3 <- transform(measureMggls, SD=apply(measureMggls,1, sd, na.rm = FALSE))
# # stdMggls <- std3[, 4];
# # 
# # 
# # g4050 <- data.frame(time, meanMgg, stdMgg, meanMggl, stdMggl, meanMggls, stdMggls);
# # 
# # #Plot
# # 
# # g4050Plot <- ggplot(g4050, aes(x=time)) +
# #   geom_line(aes(y = meanMgg, colour = "var0")) +
# #   geom_line(aes(y = meanMggl, colour = "var1")) +
# #   geom_line(aes(y = meanMggls, colour = "var2")) +
# #   xlab("hours") + ylab("OD(600)") + 
# #   ggtitle("G4050 growth")+
# #   coord_cartesian(xlim=c(0,40), ylim=c(0,.75)) + 
# #   theme_bw() +
# #   theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
# #   
# #   
# #   geom_line(aes(y=meanMggls-stdMggls, color="#000000"))+
# #   geom_line(aes(y=meanMggls), size=0.1, color="#4C8A2E") + 
# #   geom_point(aes(y=meanMggls), size=0.5, color="#4C8A2E") + 
# #   geom_line(aes(y=meanMggls+stdMggls, color="#000000"))+
# #   
# #   geom_line(aes(y=meanMggl-stdMggl, color="#000000"))+
# #   geom_line(aes(y=meanMggl), size=0.1, color="#201D58") + 
# #   geom_point(aes(y=meanMggl), size=1, color="#201D58") + 
# #   geom_line(aes(y=meanMggl+stdMggl, color="#000000"))+
# #   
# #   geom_line(aes(y=meanMgg-stdMggl, color="#000000"))+
# #   geom_line(aes(y=meanMgg), size=0.1, color="#7E2E2A") + 
# #   geom_point(aes(y=meanMgg), size=1, color="#7E2E2A") + 
# #   geom_line(aes(y=meanMgg+stdMggl, color="#000000"))
# # 
# # g4050Plot
# # 
# # #G3986
# # #Mgg
# # 
# # valueMgg <- c("X3986","X3986.1","X3986.2")
# # time <- inputkdb[, 1];
# # 
# # measureMgg <- inputkdb[,valueMgg];
# # meanMgg <- rowMeans(measureMgg[,1:3], na.rm = FALSE);
# # std1 <- transform(measureMgg, SD=apply(measureMgg,1, sd, na.rm = FALSE))
# # stdMgg <- std1[, 4];
# # 
# # 
# # #Mggl
# # 
# # valueMggl <- c("X3986.3","X3986.4","X3986.5")
# # time <- inputkdb[, 1];
# # 
# # measureMggl <- inputkdb[,valueMggl];
# # meanMggl <- rowMeans(measureMggl[,1:3], na.rm = FALSE);
# # std2 <- transform(measureMggl, SD=apply(measureMggl,1, sd, na.rm = FALSE))
# # stdMggl <- std2[, 4];
# # 
# # #Mggls
# # 
# # valueMggls <- c("X3986.6","X3986.7","X3986.8")
# # time <- inputkdb[, 1];
# # 
# # measureMggls <- inputkdb[,valueMggls];
# # meanMggls <- rowMeans(measureMggls[,1:3], na.rm = FALSE);
# # std3 <- transform(measureMggls, SD=apply(measureMggls,1, sd, na.rm = FALSE))
# # stdMggls <- std3[, 4];
# # 
# # 
# # g3986 <- data.frame(time, meanMgg, stdMgg, meanMggl, stdMggl, meanMggls, stdMggls);
# # 
# # #Plot
# # 
# # g3986Plot <- ggplot(g3986, aes(x=time)) +
# #   geom_line(aes(y = meanMgg, colour = "var0")) +
# #   geom_line(aes(y = meanMggl, colour = "var1")) +
# #   geom_line(aes(y = meanMggls, colour = "var2")) +
# #   xlab("hours") + ylab("OD(600)") + 
# #   ggtitle("G3986 growth")+
# #   coord_cartesian(xlim=c(0,40), ylim=c(0,.75)) + 
# #   theme_bw() +
# #   theme(legend.position = "none", aspect.ratio=1,  panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
# #   
# #   
# #   geom_line(aes(y=meanMggls-stdMggls, color="#000000"))+
# #   geom_line(aes(y=meanMggls), size=0.1, color="#4C8A2E") + 
# #   geom_point(aes(y=meanMggls), size=0.5, color="#4C8A2E") + 
# #   geom_line(aes(y=meanMggls+stdMggls, color="#000000"))+
# #   
# #   geom_line(aes(y=meanMggl-stdMggl, color="#000000"))+
# #   geom_line(aes(y=meanMggl), size=0.1, color="#201D58") + 
# #   geom_point(aes(y=meanMggl), size=1, color="#201D58") + 
# #   geom_line(aes(y=meanMggl+stdMggl, color="#000000"))+
# #   
# #   geom_line(aes(y=meanMgg-stdMggl, color="#000000"))+
# #   geom_line(aes(y=meanMgg), size=0.1, color="#7E2E2A") + 
# #   geom_point(aes(y=meanMgg), size=1, color="#7E2E2A") + 
# #   geom_line(aes(y=meanMgg+stdMggl, color="#000000"))
# # 
# # g3986Plot
# # 
# # grid.arrange(g3936Plot, g4050Plot, g3986Plot, ncol = 2, top = "Main title")
