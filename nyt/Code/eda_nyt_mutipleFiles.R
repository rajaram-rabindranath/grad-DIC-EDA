########################################################################
# File name : eda_nyt_multipleFiles.R
# Project   : Project 1 -- NYtimes -- multiple days
# Course    : CSE 587 -- Data Intensive Computing
# Author    : Rajaram Rabindranath, Harish Mangalapalli
########################################################################


### Libs to be loaded
install.packages("reshape"); 
library(reshape)
library(ggplot2)
library(doBy)


## Move to the directory that has the data
## Move to the directory that has the data
dataSourcePath <- "/home/raja/Education/sem 2/DIC/submission/EDA_using_R_CSE587/NYT/Datasets"
setwd(dataSourcePath)

# function to save ggplots
dic.ggsave <- function(filename = default_name(plot), height= 5, width= 8, dpi= 200, ...) 
{
  ggsave(filename=filename, height=height, width=width, dpi=dpi, ...)
}


# the days vector
days <- (1:31)
# Constant
female <- 0
male <- 1

# Summary metrics --- for all 31 days
percentLoggedIn <- vector()
countTotal <- vector()
countFemale <- vector()
countMale <- vector()
meanCTR <- vector()
meanClicks <- vector()
lowClicker <- vector()
okClicker <- vector()
hiClicker <- vector()
nonClicker <- vector()
notTargetted <- vector()

for(i in 1:31)
{
  filename <- paste("nyt",i,sep="")
  filename <- paste(filename,".csv",sep="")

  nyt <- read.csv(filename)
  
  # Create Age buckets
  nyt$AgeGrp <- cut(nyt$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
  
  # Modifying gender classification from NUMERIC -- STRING
  nyt$Gender[nyt$Gender == male] <- "MALE"
  nyt$Gender[nyt$Gender == female & nyt$AgeGrp != "(-Inf,0]"] <- "FEMALE"
  # the nyt$Gender == female cond. is redundant
  nyt$Gender[nyt$Gender == female & nyt$AgeGrp == "(-Inf,0]"] <- "Not loggedIn" 
  
  # Click categories
  nyt$clkCat[nyt$Impressions == 0] <- "NOT_TARGETTED"
  nyt$clkCat[nyt$Clicks == 0 & nyt$Impressions > 0] <- "NON_CLICKERS"
  nyt$clkCat[nyt$Clicks > 0] <- "CLICKERS"
  nyt$clkCat <- factor(nyt$clkCat)
  
  # Split the masterDataset for analysis      
  nyt.notLogged <- subset(nyt, Signed_In != 1)
  nyt.loggedIn <- subset(nyt, Signed_In == 1)
  
  # Count of users -- MALE, FEMALE and total Users
  nyt.loggedIn.totalUsers<-nrow(subset(nyt.loggedIn))
  nyt.loggedIn.maleUsers<-nrow(subset(nyt.loggedIn,nyt.loggedIn$Gender == "MALE"))
  nyt.loggedIn.femaleUsers <- nrow(subset(nyt.loggedIn,nyt.loggedIn$Gender == "FEMALE"))  # note:When not logged in records have '0' for age -- be we are taking care of it since we are using the loggedIn dataset
  
  # get the percentage of people loggedIn on a given day -- must have an vector of these nums for 30 days to plot graph
  percentLoggedIn <- c(percentLoggedIn,(nyt.loggedIn.totalUsers/length(nyt$Age))*100)
  countFemale <- c(countFemale,nrow(subset(nyt.loggedIn,nyt.loggedIn$Gender == "FEMALE")))
  countMale <- c(countMale,nrow(subset(nyt.loggedIn,nyt.loggedIn$Gender == "MALE")))
  countTotal <- c(countTotal,nrow(nyt))
  meanCTR <- c(meanCTR,sum(nyt$Clicks)/sum(nyt$Impressions))
  meanClicks <- c(meanClicks,sum(nyt$Clicks)/nrow(subset(nyt,nyt$Clicks>0)))
  
  # SEGEMENT USERS BASED ON THEIR CLICK BEHAVIOUR
  # We have found that the distrubution of clicks do not follow
  # A guassian distribution -- therefore creating rowLevelCTR
  nyt$rowLevelCTR <- nyt$Clicks/nyt$Impressions
  # Find the Quartiles --- using rowLeveCTR
  quartiles.clicks<-quantile((subset(nyt,nyt$clkCat == "CLICKERS"))$rowLevelCTR,na.rm=TRUE)
  
  # Create categories based on rowLevelCTR quartiles
  nyt$clkBehaviour[nyt$rowLevelCTR <= quartiles.clicks[2] & nyt$Clicks > 0 ] <- "low-clicker"
  nyt$clkBehaviour[nyt$rowLevelCTR > quartiles.clicks[2] & nyt$rowLevelCTR <= quartiles.clicks[4] & nyt$Clicks > 0] <- "ok-clicker"
  nyt$clkBehaviour[nyt$rowLevelCTR > quartiles.clicks[4] & nyt$Clicks > 0] <- "hi-clicker"
  nyt$clkBehaviour[nyt$clkCat == "NON_CLICKERS"] <- "non-clicker"
  nyt$clkBehaviour[nyt$clkCat == "NOT_TARGETTED"] <- "not-targetted"
  
  ####### SUMMARY STATISTICS FOR A DAY STORED IN VECTORS
  lowClicker <- c(lowClicker,nrow(subset(nyt,nyt$clkBehaviour == "low-clicker")))
  hiClicker <- c(hiClicker,nrow(subset(nyt,nyt$clkBehaviour == "hi-clicker")))
  okClicker <- c(okClicker,nrow(subset(nyt,nyt$clkBehaviour == "ok-clicker")))
  nonClicker <- c(nonClicker,nrow(subset(nyt,nyt$clkBehaviour == "non-clicker")))
  notTargetted <- c(notTargetted,nrow(subset(nyt,nyt$clkBehaviour == "not-targetted")))

  remove(nyt)
}

#### :: Prepare dataframe for plots
monthSummary = data.frame(days,percentLoggedIn,countTotal,countFemale,countMale,meanCTR,meanClicks,lowClicker,okClicker,hiClicker,nonClicker,notTargetted)
sapply(monthSummary, class)
#View(monthSummary)


##### :: PLOTS AHEAD

dat <- data.frame(x = rnorm(100))
number_ticks<-function(n){function(limits) pretty (limits,n)}

##### :: Percentage of people logged-in over a 31 days period
ggplot(monthSummary,aes(x=monthSummary$days,y=monthSummary$percentLoggedIn)) + 
  geom_line(stat="identity")+
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="% Logged In",title="Percent Logged In over Time")
dic.ggsave("Percent Logged In over Time.png")

##### :: # Users over time
ggplot(monthSummary,aes(x=monthSummary$days,y=monthSummary$countTotal)) + 
  geom_line(stat="identity")+
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="# Users",title="User count variation over Time")
dic.ggsave("User count variation over Time.png")

##### :: Female and Male users' count over a 31 day period
keepCols <- c("days","countFemale","countMale")
monthSummary.gender<-monthSummary[keepCols]
colnames(monthSummary.gender) <- c("days","Female","Male")
monthSummary.gender.melt <- melt(monthSummary.gender,id.vars=1)

ggplot(monthSummary.gender.melt,aes(x=monthSummary.gender.melt$days,y=monthSummary.gender.melt$value,fill=monthSummary.gender.melt$variable))+
  geom_line(stat="identity",aes(color=variable,group=variable)) +
  scale_x_continuous(breaks=number_ticks(31)) +
  guides(color=guide_legend(title="Gender")) +
  labs(x="Days",y="Users",title="User Count variation by Gender over Time")
dic.ggsave("User Count variation by Gender over Time.png")

##### :: Click behaviour distribution over 31 day period -- only pick few cats
keepCols <- c("days","lowClicker","okClicker","hiClicker")
monthSummary.clkBehaviour<-monthSummary[keepCols]
monthSummary.clkBehaviour.melt <- melt(monthSummary.clkBehaviour,id.vars=1)

mx <- monthSummary.clkBehaviour.melt

ggplot(mx,aes(x=mx$days,y=mx$value,fill=variable))+
  geom_line(stat="identity",aes(color=variable,group=variable)) +
  scale_x_continuous(breaks=number_ticks(31)) +
  guides(color=guide_legend(title="Clicker Behaviour")) +
  labs(x="Days",y="Users",title="User Count variation by Click Behaviour over Time")
dic.ggsave("User Count variation by Click Behaviour over Time.png")

##### :: CTR over 31 day period  
ggplot(monthSummary,aes(x=monthSummary$days, y=monthSummary$meanCTR)) + 
  geom_line(stat="identity") +
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="CTR",title="CTR variation over Time")
dic.ggsave("CTR variation over Time.png")

#### :: Mean CLicks over 31 day period
ggplot(monthSummary,aes(x=monthSummary$days, y=monthSummary$meanClicks)) + 
  geom_line(stat="identity") +
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="Mean Clicks",title="Mean Clicks over Time")
dic.ggsave("Mean Clicks over Time.png")

######################################################################################################################
# END OF FILE
######################################################################################################################