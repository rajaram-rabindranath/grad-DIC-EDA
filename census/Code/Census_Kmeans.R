########################################################################
# Author : Rajaram Rabindranath, Harish Mangalapalli
# Project: Project 1 -- EDA on "IPUMS CPS" Census data
# CSE 587 -- Data Intensive Computing
########################################################################

library(plyr)
library(doBy)
library(ggplot2)
library(maps)
library(mapdata)
library(rgl)

#############################################################

#### :: declaring paths and filename handles
dataSourcePath <- "C:/Users/Harish/SkyDrive/Documents/CSE 587/Project 1/For Submission/Census/Datasets/"

#### :: move to the data directory
setwd(dataSourcePath)

censusData <- get(load("census.rda"))
censusData_01_03 <- subset(censusData,censusData$yr >= 2001 & censusData$yr < 2004)
censusData_04_07 <- subset(censusData,censusData$yr >= 2004 & censusData$yr < 2008)
censusData_08_10 <- subset(censusData,censusData$yr >= 2008 & censusData$yr <=2010)

####################################################################################

wssplot <- function(data, datasetNames, iter, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss) + (0.5*i*ncol(data)*log(nrow(data)))
  }
  title <- "Within groups Sum of Squares by # Clusters for years"
  nameOf_plot <- paste(title,datasetNames[iter],sep=" ")
  datasetNames[iter]
  title <- paste(title,datasetNames[iter],sep=" ")
  nameOf_plot <- paste(nameOf_plot,"jpeg",sep=".")
  jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main=title)
  dev.off()
}


# function to save ggplots
dic.ggsave <- function(filename = default_name(plot), height= 5, width=12, dpi= 200, ...) 
{
  ggsave(filename=filename, height=height, width=width, dpi=dpi, ...)
}

################ Prepare for k means ###############
datasetNames <- c("01_03","04_07","08_10")


################### Create groups of the population by Year of Census #############
data(state.fips)
for(i in 1:length(datasetNames))
{
  name_dataset<-paste("censusData",datasetNames[i],sep="_")
  data_by_timeSlice <- get(name_dataset);
  
  data_by_timeSlice <- subset(data_by_timeSlice, fam_income != "NA" & 
                         fam_income != 999999 & fam_income != 99999 & 
                         fam_income != 99998 & fam_income != -9997 & 
                         fam_income != 0 & fam_income != -9999 & age < 80)

  ######### Keep only househead info and purge unnecessary columns for K Means #######
  
  td_pre_kmeans <- subset(data_by_timeSlice, relationship == 101)
  keeps <- c("fam_income","age", "fam_size")
  tdkmeans <- td_pre_kmeans[keeps]
  tdkmeans <- scale(tdkmeans)
  row.names(tdkmeans) <- NULL
  
  ############## Figure out optimal value of K #############
  
  wssplot(tdkmeans, datasetNames, i)
  
  ########### Run K Means ###############
  
  set.seed(1234)
  fit.km <- kmeans(tdkmeans, 4, nstart=25)    
  
  ########## Set cluster numbers back into actual data for analysis and plotting ##########
  
  td_post_kmeans <- td_pre_kmeans
  td_post_kmeans$Cluster <- fit.km$cluster
  agg <- aggregate(td_post_kmeans, by=list(cluster=fit.km$cluster), mean)
  agg2 <- aggregate(td_post_kmeans[keeps], by=list(cluster=fit.km$cluster), mean)
  agg2 <- agg2[with(agg2, order(fam_income)), ]
  agg2$newClusters <- c(1,2,3,4) 
  assign(paste("groups",i), agg2)
  td_post_kmeans2 <- merge(td_post_kmeans, agg2[c("cluster", "newClusters")], by.x="Cluster", 
                           by.y="cluster")
  td_post_kmeans2$Cluster <- NULL
  td_post_kmeans2$Cluster <- td_post_kmeans2$newClusters
  td_post_kmeans2$newClusters <- NULL
  td_post_kmeans$h_wt_supp <- td_post_kmeans$h_wt_supp/10000
  
  ############### Generate a 3D plot to visualize the generated clusters #########
  
  p2 <- plot3d(xlab="Age", ylab = "Family Size", zlab="Family Income", td_post_kmeans$age,
               td_post_kmeans$fam_size,td_post_kmeans$fam_income, col=td_post_kmeans$Cluster, size=3)
  layer3 <- plot3d(agg$age,agg$fam_size,agg$fam_income, add=TRUE, col="yellow", size = 10)
  
  ############### Generate State-wise cluster statistics ##################
  
  colors <- c("#f03b20", "#636363",  "#c994c7", "#377eb8")
  
  td_post_kmeans.metrics <- summaryBy(td_post_kmeans$h_wt_supp~td_post_kmeans$stateCode+
                                        td_post_kmeans$Cluster,td_post_kmeans,FUN=sum)
  colnames(td_post_kmeans.metrics)[3] <- "NumPeople"
  td_post_kmeans.metrics2 <- summaryBy(td_post_kmeans.metrics$NumPeople~
                                         td_post_kmeans.metrics$stateCode,td_post_kmeans.metrics,FUN=which.max)
  colnames(td_post_kmeans.metrics2)[2] <- "DomCluster"
  
  td_post_kmeans.metrics2 <- merge(td_post_kmeans.metrics2, state.fips, by.x = "stateCode", 
                                   by.y = "fips")
  td_post_kmeans.metrics2$stateCode.x <- NULL
  colnames(td_post_kmeans.metrics2)[colnames(td_post_kmeans.metrics2)=="fips.y"] <- "State"
  
  ############### Plot family clusters on Map ##############
  
  title <- "US family income by state for years"
  nameOf_plot <- paste(title,datasetNames[i],sep=" ")
  title <- paste(title,datasetNames[i],sep=" ")
  nameOf_plot <- paste(nameOf_plot,"jpeg",sep=".")
  jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)
  map("state", col = colors[td_post_kmeans.metrics2$DomCluster], fill = TRUE)
  map("usa", col = "black", fill = FALSE, add = TRUE)
  title(title)
  leg.txt <- c("Family of 1-2 with Young Head and Low Income",
               "Family of 1-2 with Old-Aged Head and Low Income",
               "Family of 4-5 with Middle-Aged Head and Average Income",
               "Family of 3-4 with Middle-Aged Head and High Income")
  legend("bottomleft", leg.txt, horiz = FALSE, fill = colors, cex = 1.2)
  dev.off()
  
  ############### Generate County-wise cluster statistics ##################
  
  td_post_kmeans.metrics <- summaryBy(td_post_kmeans$Cluster~td_post_kmeans$county+
                                        td_post_kmeans$Cluster,td_post_kmeans,FUN=length)
  colnames(td_post_kmeans.metrics)[3] <- "NumPeople"
  td_post_kmeans.metrics2 <- summaryBy(td_post_kmeans.metrics$NumPeople~td_post_kmeans.metrics$county,
                                       td_post_kmeans.metrics,FUN=which.max)
  colnames(td_post_kmeans.metrics2)[2] <- "DomCluster"
  
  ############### Plot family clusters on Map ##############
    
  title <- "US family income by county for years"
  nameOf_plot <- paste(title,datasetNames[i],sep=" ")
  title <- paste(title,datasetNames[i],sep=" ")
  nameOf_plot <- paste(nameOf_plot,"jpeg",sep=".")
  jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)
  map("county", col = colors[td_post_kmeans.metrics2$DomCluster], fill = TRUE)
  map("state", col = "white", fill = FALSE, add = TRUE)
  map("usa", col = "black", fill = FALSE, add = TRUE)
  title(title)
  leg.txt <- c("Family of 1-2 with Young Head and Low Income",
               "Family of 1-2 with Old-Aged Head and Low Income",
               "Family of 4-5 with Middle-Aged Head and Average Income",
               "Family of 3-4 with Middle-Aged Head and High Income")
  legend("bottomleft", leg.txt, horiz = FALSE, fill = colors, cex = 1.2)
  dev.off()

  ########################## Remove datasets ################
  remove(data_by_timeSlice)
  remove(td_pre_kmeans)
  remove(tdkmeans)
  remove(agg)
  remove(agg2)
  remove(td_post_kmeans)
  remove(td_post_kmeans2)
  remove(td_post_kmeans.metrics)
  remove(td_post_kmeans.metrics2)
}
rm(list=ls())