########################################################################
# Author : Rajaram Rabindranath, Harish Mangalapalli
# Project: Project 1 -- EDA on simulated "New York Times" data
# CSE 587 -- Data Intensive Computing
########################################################################

# todo list:
# 1.collect mean CTR
# 2.collect mean clicks
# 3.collected % LoggedIn
# 4.count of people belonging to different click behaviour categories


# please set file name 
dataFile <- "nyt1.csv"


# install some packages
install.packages("doBy") # group by and other jazz
install.packages("ggplot2") # for plots

## Move to the directory that has the data
dataSourcePath <- "/home/raja/Education/sem 2/DIC/submission/EDA_using_R_CSE587/NYT/Datasets"
setwd(dataSourcePath)

library(ggplot2)
library(doBy)

#### declare all functions and constants
siterange <- function(x)
{
  # c func is used to make a vector (length gives the number of obs)
  c(length(x), min(x), max(x), mean(x)) 
}

summaryStatistics <- function(x)
{
  c(length(x),sum(x),mean(x),min(x),max(x))
}

# find agg(clicks)/#obs
clickHabit <- function(dataSet)
{
  mean(dataSet)
}

# function to save ggplots
dic.ggsave <- function(filename = default_name(plot), height= 5, width= 8, dpi= 200, ...) 
{
  ggsave(filename=filename, height=height, width=width, dpi=dpi, ...)
}

# Constant
female <- 0
male <- 1

nyt <- read.csv(dataFile)

############################################### DATA TRANSFORMATION ##############################

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

# SEGEMENT USERS BASED ON THEIR CLICK BEHAVIOUR
# We have found that the distrubution of clicks do not follow
# A guassian distribution -- therefore creating rowLevelCTR
nyt$rowLevelCTR <- nyt$Clicks/nyt$Impressions
unique(nyt$rowLevelCTR)

# Find the Quartiles --- using rowLeveCTR
quartiles.clicks<-quantile((subset(nyt,nyt$clkCat == "CLICKERS"))$rowLevelCTR,na.rm=TRUE)

# Create categories based on rowLevelCTR quartiles
nyt$clkBehaviour[nyt$rowLevelCTR <= quartiles.clicks[2] & nyt$Clicks > 0 ] <- "low-clicker"
nyt$clkBehaviour[nyt$rowLevelCTR > quartiles.clicks[2] & nyt$rowLevelCTR <= quartiles.clicks[4] & nyt$Clicks > 0] <- "ok-clicker"
nyt$clkBehaviour[nyt$rowLevelCTR > quartiles.clicks[4] & nyt$Clicks > 0] <- "hi-clicker"
nyt$clkBehaviour[nyt$clkCat == "NON_CLICKERS"] <- "non-clicker"
nyt$clkBehaviour[nyt$clkCat == "NOT_TARGETTED"] <- "not-targetted"

#DEBUG#
print("DEBUG STATEMENTS BEING RUN : check the results of the CLICK BEHAVIOUR data prep")
quartiles.clicks
nrow(nyt)
nrow(subset(nyt,nyt$Impressions == 0))
nrow(subset(nyt,is.nan(nyt$rowLevelCTR)))
nrow(subset(nyt,nyt$rowLevelCTR <= quartiles.clicks[2] & nyt$clkCat == "CLICKERS"))
unique(nyt$clkCat) 
unique(nyt$Gender) 
unique(nyt$rowLevelCTR)
unique(nyt$clkBehaviour)


##################################### GENERAL STATISTICS #################################

# some graphs to make sense of the data
ggplot(nyt, aes(x=Impressions, fill=AgeGrp))+geom_histogram(binwidth=1)
ggplot(nyt, aes(x=AgeGrp, y=Impressions, fill=AgeGrp))+geom_boxplot()

# Executing the siterange function to
# compute obsCount, min, max and mean
# and group the result by ageBins
nyt.summary.byAgeBins<-summaryBy(Age~AgeGrp, data=nyt, FUN=siterange)
colnames(nyt.summary.byAgeBins) <- c("AgeGrp","ObsCount","minAge","maxAge","meanAge")

##### :: User Distribution by Age Groups
ggplot(nyt.summary.byAgeBins,aes(x=AgeGrp, y=ObsCount,fill=AgeGrp)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Group",y="# Users",title="User Distribution by Age Group Segments")  
dic.ggsave("User Dist by Age Group.png")

################################## DEMOGRAPHICS BASED METRIC ##############################
## Count of ppl loggedIn by Age Groups
nyt.loggedIn.cntByAgeBins <- summaryBy(AgeGrp~AgeGrp,data=nyt.loggedIn, FUN=length)
colnames(nyt.loggedIn.cntByAgeBins) <- c("AgeGrp","TotalCount")

head(nyt.loggedIn.cntByAgeBins)

nyt.loggedIn.genderDist <- summaryBy(Gender~AgeGrp+Gender,data=nyt.loggedIn, FUN=length)
colnames(nyt.loggedIn.genderDist) <- c("AgeGrp","Gender","ObsCount")
nyt.loggedIn.genderDist

# 1. merge datasets
# 2. remove second column -- which has binary for male and female
# 3. rearrange the columns
nyt.loggedIn.genderDist <- merge(nyt.loggedIn.genderDist,nyt.loggedIn.cntByAgeBins)
head(nyt.loggedIn.genderDist) #DEBUG#

# Grouped bar chart to show distribution of M and F by Age Group
ggplot(nyt.loggedIn.genderDist, aes(x=AgeGrp, y=ObsCount, fill=Gender)) + 
geom_bar(stat="identity",position=position_dodge()) +
xlab("Age Groups") +
ylab("# Users") +
labs(x="Age Groups",y="# Users",title="User distribution by Age Groups & Gender")+
scale_colour_grey(name ="Gender")+
theme(legend.background = element_rect(colour = "black"))

dic.ggsave("User Dist by Age Group & Gender.png")

################################ CLICK BEHAVIOUR ##############################
### SUMMARY STATISTICS --- # Imps by Clikc Behaviour + Gender
nyt.clkBehaviour.ByGenderwithImpression <- summaryBy(nyt$Impressions~nyt$clkBehaviour+Gender,data=nyt,FUN=siterange);
nyt.clkBehaviour.ByGenderwithImpression <- nyt.clkBehaviour.ByGenderwithImpression[,-4]
nyt.clkBehaviour.ByGenderwithImpression <- nyt.clkBehaviour.ByGenderwithImpression[,-4]
colnames(nyt.clkBehaviour.ByGenderwithImpression) <- c("clkBehaviour","Gender","ObsCount","MeanImps")

####### :: User distribution by Gender and Click Behaviour 
ggplot(nyt.clkBehaviour.ByGenderwithImpression,aes(x=Gender, y=ObsCount,fill=clkBehaviour)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Gender",y="# Users",title="User distribution by Gender & Click Behaviour")+
  facet_grid(~Gender)
dic.ggsave("User Dist by Gender & Click Behaviour.png")

####### :: User distribution by Gender and Click Behaviour(Sans non-clicker)
ggplot(subset(nyt.clkBehaviour.ByGenderwithImpression,nyt.clkBehaviour.ByGenderwithImpression$clkBehaviour != "non-clicker"),aes(x=Gender, y=ObsCount,fill=clkBehaviour)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Gender",y="# Users",title="User distribution by Gender & Click Behaviour(Sans non-clicker)")+
  facet_grid(~Gender)
dic.ggsave("User Dist by Gender & Click Behaviour (Sans non-clicker).png")

####### :: Mean Impressions by Click Behaviour + Gender
ggplot(subset(nyt.clkBehaviour.ByGenderwithImpression,nyt.clkBehaviour.ByGenderwithImpression$clkBehaviour != "non-clicker"),aes(x=Gender, y=MeanImps,fill=clkBehaviour)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Gender",y="Mean Imperssions",title="Mean Impressions by Gender & Click Behaviour")+
  facet_grid(~Gender)
dic.ggsave("Mean Impressions by Gender & Click Behaviour.png")

###### : SUMMARY STATISTICS for CLICK BEHAVIOUR --- # Imps by Click Behaviour
nyt.clkBehaviour.withImpression <- summaryBy(nyt$Impressions~nyt$clkBehaviour,data=nyt,FUN=siterange);
head(nyt.clkBehaviour.withImpression)
nyt.clkBehaviour.withImpression <- nyt.clkBehaviour.withImpression[,-3]
nyt.clkBehaviour.withImpression <- nyt.clkBehaviour.withImpression[,-3]
colnames(nyt.clkBehaviour.withImpression) <- c("clkBehaviour","ObsCount","MeanImps")

####### :: User Distribution by Click Behaviour
ggplot(nyt.clkBehaviour.withImpression,aes(x=clkBehaviour, y=ObsCount,fill=clkBehaviour)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Click Behaviour",y="# Users",title="User Distribution by Click Behaviour")
dic.ggsave("User Dist by Click Behaviour.png")

####### :: User Distribution by Click Behaviour(Sans non-clicker)
ggplot(subset(nyt.clkBehaviour.withImpression,nyt.clkBehaviour.withImpression$clkBehaviour!="non-clicker"),aes(x=clkBehaviour, y=ObsCount,fill=clkBehaviour)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Click Behaviour",y="# Users",title="User Distribution by Click Behaviour(Sans non-clicker)")  
dic.ggsave("User Dist by Click Behaviour(Sans non-clicker).png") 

####### :: User Distribution by Click Behaviour(Sans non-clicker)
ggplot(subset(nyt.clkBehaviour.withImpression,nyt.clkBehaviour.withImpression$clkBehaviour!="non-clicker"),aes(x=clkBehaviour, y=MeanImps,fill=clkBehaviour)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Click Behaviour",y="Mean Imps",title="Mean Impression by Click Behaviour(Sans non-clicker)")  
dic.ggsave("Mean Imps by Click Behaviour(Sans non-clicker).png") 

######################################### Mean Click #################################

# for each age group find agg(clicks)/# obs (when impressions are > 0) 
meanClick.byAgeGrp_N_Gender <- summaryBy(Clicks~AgeGrp+Gender,data=nyt, FUN=clickHabit)
meanClick.byAgeGrp <- summaryBy(Clicks~AgeGrp,data=nyt, FUN=clickHabit)

##### :: Mean Clicks by Age Group
ggplot(meanClick.byAgeGrp,aes(x=AgeGrp, y=Clicks.clickHabit,fill=AgeGrp)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Clicks/Obs",title="Mean Click by Age Group")  
dic.ggsave("Mean Clicks by Age Group.png") 
#### :: Mean Clicks by Age Grp and Gender
ggplot(meanClick.byAgeGrp_N_Gender,aes(x=AgeGrp, y=Clicks.clickHabit,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Clicks/Obs",title="Mean Click by Age Group & Gender")+
  facet_grid(~Gender)
dic.ggsave("Mean Click by Age Group & Gender.png") 

############################### Impressions Mean by Age Group ###########################
nyt.ImprBias<-summaryBy(Impressions~AgeGrp, data=nyt, FUN=mean)
head(nyt.ImprBias)

#### :: Mean Impressions vs. Age Group
ggplot(nyt.ImprBias,aes(x=AgeGrp,y=Impressions.mean,fill=AgeGrp)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Impression",title="Impressions Mean by Age Group")
dic.ggsave("Impressions Mean by Age Group.png") 

nyt.ImprBiasbyGender<-summaryBy(Impressions~AgeGrp+Gender, data=nyt, FUN=mean)
head(nyt.ImprBiasbyGender)

#### :: Mean Impressions vs. Age Group + Gender
ggplot(nyt.ImprBiasbyGender,aes(x=AgeGrp,y=Impressions.mean,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Impression",title="Impressions Mean by Age Group & Gender")+
facet_grid(~Gender)
dic.ggsave("Impressions Mean by Age Group & Gender.png") 
######################################### SURE SHOT CLIKS ########################################################################
# count -- rows that had impression > 1 and clicks > 1
# count -- rows that had impression == 1 and clicks == 1

nyt.loggedIn.validImps <- subset(nyt.loggedIn,nyt.loggedIn$Impressions > 0)

attach(nyt.loggedIn.validImps)
count.validImps_totalObs <- nrow(nyt.loggedIn.validImps)
count.sureClick_A<-nrow(subset(nyt.loggedIn.validImps,Impressions == 1 & Clicks == 1))
count.sureClick_B <- nrow(nyt.loggedIn.validImps[which(Impressions > 1 & Clicks > 1),])
(count.sureClick_A+count.sureClick_B)/count.validImps_totalObs
detach(nyt.loggedIn.validImps)

############################################# CTR ANALYSIS FROM DOing data science #################################################
ggplot(subset(nyt, Impressions>0), aes(x=Clicks/Impressions,colour=AgeGrp)) + geom_density()
ggplot(subset(nyt, Clicks>0), aes(x=Clicks/Impressions,colour=AgeGrp)) + geom_density()
ggplot(subset(nyt, Clicks>0), aes(x=AgeGrp, y=Clicks,fill=AgeGrp)) + geom_boxplot()
ggplot(subset(nyt, Clicks>0), aes(x=Clicks, colour=AgeGrp))+ geom_density()

# get the distribution of impressions across --- clkCode(3)*Gender(2)*AgeGrp(7)

nyt$clkCode[nyt$Impressions==0] <- "NoImps"
nyt$clkCode[nyt$Impressions >0] <- "Imps" 
# Convert the column to a factor -- mapping in the int domain shall make processing easier 
nyt.impClkSummary<-summaryBy(Impressions+Clicks~clkCode+Gender+AgeGrp,data = subset(nyt,nyt$Gender != "Not loggedIn"), FUN=summaryStatistics)
colnames(nyt.impClkSummary)<-c("clkCode","Gender","AgeGrp","ObsCount","TotImp","MeanImp","MinImp","MaxImp","weedout","TotClk","MeanClk","MinClk","MaxClk")
nyt.impClkSummary<-nyt.impClkSummary[,-7]
nyt.impClkSummary$CTR <- nyt.impClkSummary$TotClk/nyt.impClkSummary$TotImp

##### :: Impression Distribution by AgeGrp and Gender
ggplot(subset(nyt.impClkSummary,clkCode=="Imps"), aes(x=AgeGrp, y=TotImp,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="# Imp",title="Impression Distribution by Age Group & Gender") +
  theme(legend.background = element_rect(colour = "black"))
dic.ggsave("Impression Distribution by Age Group & Gender.png")

##### :: CTR by AgeGrp and Gender
ggplot(subset(nyt.impClkSummary,clkCode=="Imps"), aes(x=AgeGrp, y=CTR,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="# NoImps",title="CTR By Age Group & Gender") +
  theme(legend.background = element_rect(colour = "black"))
dic.ggsave("CTR By Age Group & Gender.png")

remove(nyt.impClkSummary)
nyt$clkCode[nyt$Clicks >0] <- "Clicks"
nyt$clkCode <- factor(nyt$clkCode)
nyt.impClkSummary<-summaryBy(Impressions+Clicks~clkCode+Gender+AgeGrp,data = subset(nyt,nyt$Gender != "Not loggedIn"), FUN=summaryStatistics)
colnames(nyt.impClkSummary)<-c("clkCode","Gender","AgeGrp","ObsCount","TotImp","MeanImp","MinImp","MaxImp","weedout","TotClk","MeanClk","MinClk","MaxClk")
nyt.impClkSummary<-nyt.impClkSummary[,-7]

##### :: Impression Distribution by AgeGrp and Gender when the user has clicked atleast once 
ggplot(subset(nyt.impClkSummary,clkCode=="Clicks"), aes(x=AgeGrp, y=TotImp,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="# Imps",title="Dist of Imp By Age Group & Gender(when user has clicked)") +
  theme(legend.background = element_rect(colour = "black"))
dic.ggsave("Dist of Imp By Age Group & Gender(when user has clicked).png")

nyt.clkSummary<-summaryBy(Clicks~clkCode+AgeGrp+Gender,data = subset(nyt,nyt$Gender != "Not loggedIn"), FUN=summaryStatistics)
colnames(nyt.clkSummary)<-c("clkCode","AgeGrp","Gender","ObsCount","TotClk","MeanClk","MinClk","MaxClk")

##### :: Mean Clicks by AgeGrp and Gender
ggplot(subset(nyt.clkSummary,clkCode=="Clicks"), aes(x=AgeGrp, y=MeanClk,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Click",title="Mean Click by Age Group & Gender") +
  theme(legend.background = element_rect(colour = "black"))
dic.ggsave("Mean Click by Age Group & Gender.png")

########### DATA CLEAN UP
remove(nyt.clkSummary)
remove(nyt.impClkSummary)
remove(nyt.loggedIn)
remove(nyt.notLogged)
remove(nyt.clkBehaviour.ByGenderwithImpression)
remove(nyt.clkBehaviour.withImpression)
remove(nyt.loggedIn.cntByAgeBins)
remove(nyt.loggedIn.validImps)
remove(nyt.ImprBias)
remove(nyt.ImprBiasbyGender)
remove(nyt.loggedIn.genderDist)
remove(nyt.summary.byAgeBins)
remove(meanClick.byAgeGrp)
remove(meanClick.byAgeGrp_N_Gender)
remove(nyt)


######################################################################################################################
# END OF FILE
######################################################################################################################
