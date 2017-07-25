
library(RColorBrewer)
require(RColorBrewer)
library(ggplot2)
require(ggplot2)
library(dplyr)
require(dplyr)
library(ggmap)
require(ggmap)
library(XML)
require(XML)
library(tidyr)
require(tidyr)
library(fmsb)
require(fmsb)

train <- read.csv("IndiaAffectedWaterQualityAreas.csv",stringsAsFactors = FALSE)
train$Year<- as.Date(train$Year,"%d/%m/%Y")
View(table(train$State.Name))
#We can see observations naming CHATTISGARH and CHHATTISGARH, infact reality is its a spelling mistake of some kind.
#There is only CHHATTISGARH as one of the offical states of india.the later has good number of cases this could really screw up the analysis so we need to clear that. 

#cleaning the data a little bit--------------------------------------------
train$State.Name<- gsub(pattern = "\\(.*","",train$State.Name)
train$State.Name<- gsub(pattern = "CHATTISGARH","CHHATTISGARH",train$State.Name)
train$District.Name<- gsub(pattern = "\\(.*","",train$District.Name)
train$Block.Name<- gsub(pattern = "\\(.*","",train$Block.Name)
train$Panchayat.Name<- gsub(pattern = "\\(.*","",train$Panchayat.Name)
train$Quality.Parameter<-as.factor(train$Quality.Parameter)
str(train)
# pattern of chemical compunds -----------------------------------------------------------------------------------------------------------------------------------------------------------------
kk<- as.data.frame(table(train$Year,train$Quality.Parameter),stringsAsFactors = FALSE)
kk$Var1 <- as.Date.factor(kk$Var1)
kk$Var2 <- as.factor(kk$Var2)
qplot(Freq,Var1,data = kk,facets = Var2~.,geom = c("point","smooth"),color=Var2)+labs(title="Trend seen in Chemicals over the Years",x="Number of cases",y="Years",fill="Chemicals")+
theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
  axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6))
#INFERENCE-there is a general downward trend as years goes by. destroying the popular thesis of as development happens water quality degrades.

#which chemical is found most of water quality issues------------------------------------------------------------------------------------------------------------------------------------------------
chemicals_present <- as.data.frame(table(train$Quality.Parameter),stringsAsFactors = FALSE)
names(chemicals_present) <- c("CHEMICAL","FREQ_REPORTED")
f <- ggplot(chemicals_present,aes(chemicals_present$CHEMICAL,chemicals_present$FREQ_REPORTED))
f+geom_bar(stat = "identity",fill= brewer.pal(5,"Set2"))+labs(title="Identifying Most Occuring Chemical",x="Chemicals",y="Cases")+
theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
  axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2))
#INFERENCE-As we can see there is a arge presence of IRON in all the cases,next is Salinity from Fluoride.

#overview of trend n india-----------------------------------------------------------------------------------------------------------------------------------------
overview <- as.data.frame(table(train$State.Name,train$Quality.Parameter,train$Year))
names(overview) <- c("State.Name","CHEMICAL","YEAR","Freq")
str(overview)
an <- ggplot(overview,aes(overview$State.Name,overview$Freq,fill=overview$CHEMICAL))
an+geom_bar(stat="identity",position = "dodge")+theme(axis.text.x = element_text(angle = 90))+
labs(title="Trend of Chemical Compostion in different states of India",x= "States", y="Number_Of_Cases",fill="CHEMICALS")+
 theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
  axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6))
 #As we can see here ASSAM,BIHAR,RAJASTHAN has larger cases of chemical composition reported,we will look into states which have two or more chemicals in larger cases. 

# we could identify states like Assam,Bihar,Orissa,Rajasthan. 

#lets take each state chemical wise and see-----------------------------------------------------------------------------------------------------------
goal1<-as.data.frame(table(train$State.Name,train$Quality.Parameter))
names(goal1) <- c("State.Name","CHEMICAL","Freq")
str(goal1)
an <- ggplot(goal1,aes(goal1$State.Name,goal1$Freq,fill=goal1$CHEMICAL))
an+geom_bar(stat="identity",position = "dodge")+facet_wrap(~goal1$CHEMICAL,scales="free")+
labs(title="Trend of Specific Chemical Compostion in different states of India",x= "States", y="Number_Of_Cases",fill="CHEMICALS")+scale_fill_brewer( type = "qua", palette = "Dark2", direction = 1)+
theme(axis.text.x = element_text(angle = 90),plot.background = element_rect(fill = NA),plot.title = element_text(size = rel(2)), panel.background = element_rect(fill =NA),axis.text = element_text(colour = "blue"),
  panel.grid.major = element_line(colour = "black"),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6)) 
#INFERENCE------------------------------------------------------------------------------------------------------------------------
#iron-ASSAM ,Bihar,Chhatisgargh,orissa
#Fluoride-Rajasthan,second worst Bihar
#Arsenic-westbengal,Assam,Bihar
#Nitrate,Karnataka,Maharashtra,Rajsthan
#salinity-Rajasthan
#so the main problem occurs in ASSAM ,BIHAR&RAJASTHAN as they aranked higher in presence of more than two chemicals at larger cases.
#WESTBENGAL has the highest presence of Arsenic in them but other than that other chemical reported are relatively less.
#we will analyse the trend and see district wise report of these states to get a clearer picture.

#----------------------------------------------------------------------------------------------------------------------------------
#for the state of ASSAM
#to get the values 
table(state_ASSAM$District.Name,state_ASSAM$Quality.Parameter,state_ASSAM$Year
#visualization 
state_ASSAM <- subset(train,train$State.Name=="ASSAM")
ASSAM <- as.data.frame(table(state_ASSAM$District.Name,state_ASSAM$Quality.Parameter,state_ASSAM$Year),stringsAsFactors = FALSE)
str(ASSAM)
names(ASSAM) <- c("District.Name","CHEMICAL","YEAR","Freq")
assam <- ggplot(ASSAM,aes(ASSAM$CHEMICAL,ASSAM$Freq,fill=ASSAM$District.Name))
assam+geom_bar(stat="identity",position = "dodge")+facet_grid(.~ASSAM$YEAR)+
labs(title="TREND of Chemical Compostion in ASSAM Villages",x="Chemicals",y="Number Of Cases",fill="Districts in ASSAM")+
theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
  axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6))
#INFERENCE-Generally the trend shows a downward trend in all the districts with a spike in the year 2011,then it has decreased. 
#District of Sontipur has the highest iron content 


#for the state of Bihar
#to get the values 
table(state_BIHAR$District.Name,state_BIHAR$Quality.Parameter,state_BIHAR$Year)
#visualization 
state_BIHAR<- subset(train,train$State.Name=="BIHAR")
BIHAR <- as.data.frame(table(state_BIHAR$District.Name,state_BIHAR$Quality.Parameter,state_BIHAR$Year),stringsAsFactors = FALSE)
str(BIHAR)
names(BIHAR) <- c("District.Name","CHEMICAL","YEAR","Freq")
bihar <- ggplot(BIHAR,aes(BIHAR$CHEMICAL,BIHAR$Freq,fill=BIHAR$District.Name))
bihar+geom_bar(stat="identity",position = "dodge")+facet_grid(.~BIHAR$YEAR)+
labs(title="TREND of Chemical Compostion in BIHAR Villages",x="Chemicals",y="Number Of Cases",fill="Districts in BIHAR")+
theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
  axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6))
#INFERENCE-Generally the trend shows a downward trend in all the districts,then it has decreased.
# District of Purnia has the highest iron content.


#for the state of Rajasthan
#to get the values 
table(state_RAJASTHAN$District.Name,state_RAJASTHAN$Quality.Parameter,state_RAJASTHAN$Year)
#visualization 

state_RAJASTHAN <- subset(train,train$State.Name=="RAJASTHAN")
RAJASTHAN <- as.data.frame(table(state_RAJASTHAN$District.Name,state_RAJASTHAN$Quality.Parameter,state_RAJASTHAN$Year),stringsAsFactors = FALSE)
str(RAJASTHAN)
names(RAJASTHAN) <- c("District.Name","CHEMICAL","YEAR","Freq")
rajasthan<- ggplot(RAJASTHAN,aes(RAJASTHAN$CHEMICAL,RAJASTHAN$Freq,fill=RAJASTHAN$District.Name))
rajasthan+geom_bar(stat="identity",position = "dodge")+facet_grid(.~RAJASTHAN$YEAR)+
labs(title="TREND of Chemical Compostion in RAJASTHAN Villages",x="Chemicals",y="Number Of Cases",fill="Districts in RAJASTHAN")+
theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
  axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6))
#INFERENCE-Generally the trend shows a downward trend in all the districts,then it has decreased.
#District of Barmer has the highest salinity content. 


#Visualisation using ggmap ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

url <- "http://www.distancelatlong.com/country/india"
poptable <- readHTMLTable(url,which=3)
str(poptable)
names(poptable) <- c("State.Name","latitude","longitude")
poptable$State.Name <- gsub(pattern = "\\(.*","",poptable$State.Name)
poptable$State.Name <- as.character(poptable$State.Name)
poptable$latitude <- as.character(poptable$latitude)
options(digit=8)
poptable$latitude <- as.numeric(poptable$latitude)
poptable$longitude <- as.character(poptable$longitude)
options(digit=8)
poptable$longitude <- as.numeric(poptable$longitude)
poptable$State.Name <- toupper(poptable$State.Name)
lop<-train[,c("State.Name","Quality.Parameter")]
plea<-cbind(lop,poptable)
indiamap <- get_map(location = "india",maptype ="terrain", zoom =5,color='color' ) 
as.num(plea$Quality.Parameter)
#use different zoom levels for the maps ,even change the types 
ggmap(indiamap)+geom_point(data=plea,aes(x=longitude,y=latitude,colour=Quality.Parameter,size=2),alpha=.5,na.rm=TRUE)+
  scale_colour_brewer(type = "seq", palette = "Spectral", direction = 1)

#single line fucking elegant
#train$Habitation.Name <- gsub(pattern = "\\(.*","",train$Habitation.Name)

#to really undertand something one must be liberated from it 
#radar chart-----------------------------------------------------------------------------------------------------------------------------------------------------
# trial <- as.data.frame(table(train$Year,train$Quality.Parameter,train$State.Name=="RAJASTHAN"))
# trial1 <- subset(trial,trial$Var3=="TRUE")
# nrow(trial1)
# table(trial$Var3)
# sum(trial1$Freq)

# #trial1
# trial3 <- spread(trial1,Var2,Freq)
# #trial3
# trial4 <- trial3[,-2]
# #trial4
# #set.seed(99)
# #radarchart(trial4)

# str(trial4)

# radarchart(trial4,seg=6)
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# radarchart( trial4  , axistype=1 , 
#     #custom polygon
#     pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
#     #custom the grid
#     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#     #custom labels
#     vlcex=0.8 
#     )
# legend(x=0.7, y=1, legend = rownames(trial4[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
 
