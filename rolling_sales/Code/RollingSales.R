########################################################################
# Author : Rajaram Rabindranath, Harish Mangalapalli
# Project: Project 1 -- EDA on "Rolling Sales" data for "Staten Island"
# CSE 587 -- Data Intensive Computing
########################################################################

library(doBy)
library(ggplot2)
library(gdata)
library(plyr)
library(RCurl)
library(RJSONIO)
library(maps)
library(mapdata)
library(RgoogleMaps)

#####################################################################################

#### :: declaring paths and filename handles
dataSourcePath <- "C:/Users/Harish/SkyDrive/Documents/CSE 587/Project 1/For Submission/Rolling Sales/Datasets/"

#### :: move to the data directory
setwd(dataSourcePath)

################# Read Rolling Sales Data for Staten Island #########################

bk <-read.xls("rollingsales_statenisland.xls",pattern="BOROUGH")
bk$SALE.PRICE.N <-as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))

names(bk) <-tolower(names(bk))
## clean/format the data with regular expressions
bk$gross.sqft <-as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft <-as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$sale.date <-as.Date(bk$sale.date)

## keep only homes which have a build date
bk.sale <-bk[bk$year.built != 0,]
bk.sale$year.built <- ISOdate(bk.sale$year.built,01,01)

#################### Define Age of Building  ##################################
bk.sale$age <- as.numeric(difftime(as.POSIXlt(Sys.time()),as.POSIXlt(bk.sale$year.built)))/365
###############################################################################
#################### Classify Buildings as "OLD" and "NEW" #################### 
population.meanAge <- mean(bk.sale$age)
population.stdAge <- sd(bk.sale$age)

bk.sale$ageCat[bk.sale$age >= population.meanAge+population.stdAge] <- "OLD"
bk.sale$ageCat[bk.sale$age <= population.meanAge-population.stdAge] <- "NEW"
###############################################################################
neighborhood.byAgeCat_NBhoodBins<-summaryBy(age~neighborhood+ageCat, data=bk.sale, FUN=length)
colnames(neighborhood.byAgeCat_NBhoodBins) <- c("neighborhood","ageCat","obsCount")

neighborhood.byNBhoodBins<-summaryBy(age~neighborhood, data=bk.sale, FUN=length)
colnames(neighborhood.byNBhoodBins) <- c("neighborhood","obsCount")

neighborhood.byAgeCatBins<-summaryBy(age~ageCat, data=bk.sale, FUN=length)
colnames(neighborhood.byAgeCatBins) <- c("ageCat","obsCount")

new <- merge(neighborhood.byAgeCat_NBhoodBins, neighborhood.byNBhoodBins, by = "neighborhood")
new$percent <- new$obsCount.x*100/new$obsCount.y

new2 <- merge(neighborhood.byAgeCat_NBhoodBins, neighborhood.byAgeCatBins, by = "ageCat")
new2$percent <- new2$obsCount.x*100/new2$obsCount.y

new.NewHomes <- subset(new, new$ageCat == "NEW")
new2.NewHomes <- subset(new2, new2$ageCat == "NEW")

new.NewHomes$neighborhood <- paste(new.NewHomes$neighborhood, ", Staten Island")
new2.NewHomes$neighborhood <- paste(new2.NewHomes$neighborhood, ", Staten Island")

orderedHomes <- new.NewHomes[order(-new.NewHomes$percent),]

i1 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 1))) 
i2 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0.75)))
i3 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0.5)))
i4 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0.25)))
i5 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0)))

orderedHomes$col <- "red"
orderedHomes$legend <- "Very Recent"
orderedHomes$col[(i2-1):i3] <- "orange"
orderedHomes$legend[(i2-1):i3] <- "Recent"
orderedHomes$col[(i3-1):i4] <- "blue"
orderedHomes$legend[(i3-1):i4] <- "Less Recent"
orderedHomes$col[(i4-1):i5] <- "purple"
orderedHomes$legend[(i4-1):i5] <- "Very Less Recent"


orderedHomes2 <- new2.NewHomes[order(-new2.NewHomes$percent),]

i1 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 1))) 
i2 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0.75)))
i3 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0.5)))
i4 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0.25)))
i5 <- which.min(abs(orderedHomes$percent - quantile(orderedHomes$percent, 0)))

orderedHomes2$col <- "red"
orderedHomes2$legend <- "High"
orderedHomes2$col[(i2-1):i3] <- "orange"
orderedHomes2$legend[(i2-1):i3] <- "Relatively High"
orderedHomes2$col[(i3-1):i4] <- "blue"
orderedHomes2$legend[(i3-1):i4] <- "Relatively Low"
orderedHomes2$col[(i4-1):i5] <- "purple"
orderedHomes2$legend[(i4-1):i5] <- "Low"


########### Initialize functions to plot points on map ##########
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}
#############################################
########## Plot 1st map #####################
lat <- vector(mode="numeric", length=0)
lon <- vector(mode="numeric", length=0)
neighborhood <- vector(mode="character", length=0)


for(i in 1:length(orderedHomes$neighborhood)){
  address <- geoCode(orderedHomes$neighborhood[i], verbose=TRUE)
  lat <- c(lat, address[1])
  lon <- c(lon, address[2])
  neighborhood <- c(neighborhood, address[4]) 
  Sys.sleep(1)
}

samps = data.frame(lat = as.numeric(lat), lon = as.numeric(lon), color = orderedHomes$col, 
                   char =  orderedHomes$neighborhood, legend = orderedHomes$legend)
samps$size <- "small"
samps$char <- ""
mymarkers <- cbind.data.frame(samps$lat, samps$lon, samps$size, samps$col, samps$char, samps$legend)
names(mymarkers) <- c("lat", "lon", "size", "color", "char", "legend")  
bb <- qbbox(lat = mymarkers[,"lat"], lon = mymarkers[,"lon"]);

terrain_close <- GetMap.bbox(lonR= range(bb$lon), latR= range(bb$lat), 
                             destfile= "terrclose2.png", markers= mymarkers[1:15,], 
                             maptype = "terrain", zoom=11, NEWMAP = TRUE)
terrain_close <- GetMap.bbox(lonR= range(bb$lon), latR= range(bb$lat), 
                             destfile= "terrclose2.png", markers= mymarkers[16:30,], 
                             NEWMAP = FALSE, maptype = "terrain", zoom=9)
terrain_close <- GetMap.bbox(lonR= range(bb$lon), latR= range(bb$lat), 
                             destfile= "terrclose2.png", markers= mymarkers[31:45,], 
                             NEWMAP = FALSE, maptype = "terrain", zoom=9)
terrain_close <- GetMap.bbox(lonR= range(bb$lon), latR= range(bb$lat), 
                             destfile= "terrclose2.png", markers= mymarkers[46:59,], 
                             NEWMAP = FALSE, maptype = "terrain", zoom=9)

center = c(mean(bb$lat), mean(bb$lon));
zoom <- min(MaxZoom(range(bb$lat), range(bb$lon)));
title <- "Newest Neighborhoods"
nameOf_plot <- paste(title,"jpeg",sep=".")
jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)

MyMap <- GetMap(center=center, zoom=zoom, destfile = "MyTile1.png");
MyMap <- PlotOnStaticMap(MyMap, lon= mymarkers[1:12,]$lon, lat= mymarkers[1:12,]$lat, col=mymarkers[1:12,]$col, FUN = points, cex=1.5, pch=20, add=FALSE)
MyMap <- PlotOnStaticMap(MyMap, lon= mymarkers[13:25,]$lon, lat= mymarkers[13:25,]$lat, col=mymarkers[13:25,]$col, FUN = points, cex=1.5, pch=20, add=TRUE)
MyMap <- PlotOnStaticMap(MyMap, lon= mymarkers[26:38,]$lon, lat= mymarkers[26:38,]$lat, col=mymarkers[26:38,]$col, FUN = points, cex=1.5, pch=20, add=TRUE)
MyMap <- PlotOnStaticMap(MyMap, lon= mymarkers[39:50,]$lon, lat= mymarkers[39:50,]$lat, col=mymarkers[39:50,]$col, FUN = points, cex=1.5, pch=20, add=TRUE)

tblLgd <- unique(mymarkers[,c("legend","color")])
row.names(tblLgd) <- NULL

legend("bottomright", legend = tblLgd$legend, fill = tblLgd$col, bg = "white")
dev.off()
#############################################
########## Plot 2nd map #####################
lat2 <- vector(mode="numeric", length=0)
lon2 <- vector(mode="numeric", length=0)
neighborhood2 <- vector(mode="character", length=0)

for(i in 1:length(orderedHomes2$neighborhood)){
  address2 <- geoCode(orderedHomes2$neighborhood[i], verbose=TRUE)
  lat2 <- c(lat2, address2[1])
  lon2 <- c(lon2, address2[2])
  neighborhood2 <- c(neighborhood2, address2[4]) 
  Sys.sleep(1)
}



samps2 = data.frame(lat = as.numeric(lat2), lon = as.numeric(lon2), color = orderedHomes2$col, char =  orderedHomes2$neighborhood, legend = orderedHomes2$legend)
samps2$size <- "small"  
samps2$char <- ""   
mymarkers2 <- cbind.data.frame(samps2$lat, samps2$lon, samps2$size, samps2$col, 
                               samps2$char, samps2$legend)
names(mymarkers2) <- c("lat", "lon", "size", "color", "char", "legend")  
bb2 <- qbbox(lat = mymarkers2[,"lat"], lon = mymarkers2[,"lon"]);


terrain_close <- GetMap.bbox(lonR= range(bb2$lon), latR= range(bb2$lat), destfile= "terrclose3.png", markers= mymarkers2, maptype = "terrain")


center2 = c(mean(bb2$lat), mean(bb2$lon));
zoom2 <- min(MaxZoom(range(bb2$lat), range(bb2$lon)));
title <- "Neighborhoods contributing to highest percentage of new homes"
nameOf_plot <- paste(title,"jpeg",sep=".")
jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)
MyMap2 <- GetMap(center=center2, zoom=zoom2, destfile = "MyTile2.png");
MyMap2 <- PlotOnStaticMap(MyMap2, lon= mymarkers2[1:12,]$lon, lat= mymarkers2[1:12,]$lat, col=mymarkers2[1:12,]$col, FUN = points, cex=1.5, pch=20, add=FALSE)
MyMap2 <- PlotOnStaticMap(MyMap2, lon= mymarkers2[13:25,]$lon, lat= mymarkers2[13:25,]$lat, col=mymarkers2[13:25,]$col, FUN = points, cex=1.5, pch=20, add=TRUE)
MyMap2 <- PlotOnStaticMap(MyMap2, lon= mymarkers2[26:38,]$lon, lat= mymarkers2[26:38,]$lat, col=mymarkers2[26:38,]$col, FUN = points, cex=1.5, pch=20, add=TRUE)
MyMap2 <- PlotOnStaticMap(MyMap2, lon= mymarkers2[39:50,]$lon, lat= mymarkers2[39:50,]$lat, col=mymarkers2[39:50,]$col, FUN = points, cex=1.5, pch=20, add=TRUE)

tblLgd2 <- unique(mymarkers2[,c("legend","color")])
row.names(tblLgd2) <- NULL

legend("bottomright", legend = tblLgd2$legend, fill = tblLgd2$col, bg = "white")
dev.off()
##################################################################
########### Analysis of Sales as a Time Series ###################
bk.sale <-bk[bk$sale.price.n != 0,]
bk.sale$sale.half = cut(as.numeric(format(bk.sale$sale.date,'%m')),2,labels=c('1st Half','2nd Half'))
bk.sale$sale.half.year = paste(format(bk.sale$sale.date,'%Y'), bk.sale$sale.half, sep=" ")

title <- "Improper Sale Price Distribution"
nameOf_plot <- paste(title,"jpeg",sep=".")
jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)
ggplot(bk.sale, aes(x=sale.half.year, y=sale.price.n)) + 
  geom_boxplot() +
  labs(x="Time of Sale", y="Sale Price", 
       title=title)
dev.off()

summary <- summaryBy(sale.price.n~tax.class.at.time.of.sale+sale.half.year, 
                     data=subsetBy(~tax.class.at.time.of.sale+sale.half.year, 
                                   sale.price.n >= quantile(sale.price.n, 0.05) & 
                                     sale.price.n <= quantile(sale.price.n, 0.95), data=bk.sale), 
                     FUN=c(mean,median,length))
summary$tax.class.at.time.of.sale[summary$tax.class.at.time.of.sale == 1] <- "Residential"
summary$tax.class.at.time.of.sale[summary$tax.class.at.time.of.sale == 2] <- "Commerical cum Residential"
summary$tax.class.at.time.of.sale[summary$tax.class.at.time.of.sale == 4] <- "Commercial"

title <- "Mean Sale Price Distribution BY Time of Sale & Property Type"
nameOf_plot <- paste(title,"jpeg",sep=".")
jpeg(file = nameOf_plot, width = 15,height = 10, units = "in", res = 120)
ggplot(summary, aes(x=sale.half.year, y=sale.price.n.mean)) + 
  geom_line(aes(colour = tax.class.at.time.of.sale, group =tax.class.at.time.of.sale))+
  guides(colour=guide_legend(title="Property Type")) +
  labs(x="Time of Sale",y="Mean Sale Price",
       title=title)
dev.off()
##################################################################
################# END ############################################