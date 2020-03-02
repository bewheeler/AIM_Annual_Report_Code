#packages installed= data.table, dplyr, formattable, tidyr using function install.packages("")
library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(formattable)
library(qwraps2)


#Actual.Eco.Site is a column I added to each TerrADat csv to assign the ecological site
#CSVs have to be saved as CSV with the names as follows: plots = "Allyears_plots" , soil horizon = "Allyears_soilhorizons"
#query results = "Allyears_query", plant specis = "PSPPALL" , species richness = "Allyears_species_rich"
#LPI detail = "LPI_all"
Plotdata<-read.csv("~/Allyears_plots.csv")
View(Plotdata)
soildata<-read.csv("~/Allyears_soilhorizons.csv")
soildata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(soildata$PrimaryKey,Plotdata$PrimaryKey)]
AIMdata<-read.csv("~/Allyears_query.csv")
AIMdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(AIMdata$Primary.Key,Plotdata$PrimaryKey)]
AIMdata$PlotID<-Plotdata$PlotID[match(AIMdata$Primary.Key,Plotdata$PrimaryKey)]

#surface cover
View(AIMdata)

AIMdata$shrub.subshrub<-AIMdata$Shrub.Cover.Pct.Any.Hit+AIMdata$Noxious.SubShrub.Cover.Pct.Any.Hit+AIMdata$NonNoxious.SubShrub.Cover.Pct.Any.Hit
AIMdata$succulent<-AIMdata$Noxious.Succulent.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Succulent.Cover.Pct.Any.Hit
AIMdata$tree<-AIMdata$Noxious.Tree.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Tree.Cover.Pct.Any.Hit
AIMdata$noxiousforb<-AIMdata$Noxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$Noxious.Perennial.Forb.Cover.Pct.Any.Hit
AIMdata$nonnoxiousforb<-AIMdata$NonNoxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Perennial.Forb.Cover.Pct.Any.Hit

#calculating Population Size, will be used throughout the entire script
BH<-subset(AIMdata,Actual.Eco.Site=="BH")
LB<-subset(AIMdata,Actual.Eco.Site=="LB")
LOA<-subset(AIMdata,Actual.Eco.Site=="LOA")
MO<-subset(AIMdata,Actual.Eco.Site=="MO")
OTH<-subset(AIMdata,Actual.Eco.Site=="OTH")
ROF<-subset(AIMdata,Actual.Eco.Site=="ROF")
SAL<-subset(AIMdata,Actual.Eco.Site=="SAL")
SAN<-subset(AIMdata,Actual.Eco.Site=="SAN")
FORE<-subset(AIMdata,Actual.Eco.Site=="FORE")

BHN<-count(BH,Primary.Key)
BHN2<-sum(BHN$n)
LBN<-count(LB,Primary.Key)
LBN2<-sum(LBN$n)
LOAN<-count(LOA,Primary.Key)
LOAN2<-sum(LOAN$n)
MON<-count(MO,Primary.Key)
MON2<-sum(MON$n)
OTHN<-count(OTH,Primary.Key)
OTHN2<-sum(OTHN$n)
ROFN<-count(ROF,Primary.Key)
ROFN2<-sum(ROFN$n)
SALN<-count(SAL,Primary.Key)
SALN2<-sum(SALN$n)
SANN<-count(SAN,Primary.Key)
SANN2<-sum(SANN$n)
FOREN<-count(FORE,Primary.Key)
FOREN2<-sum(FOREN$n)


#continuing with calculating surface cover

AIMdata1<-AIMdata %>%
  select(Bare.Soil.Pct,Forb.Cover.Pct.Any.Hit,Grass.Cover.Pct.Any.Hit,Total.Litter.Cover.Pct.First.Hit,Rock.Cover.Pct.First.Hit,shrub.subshrub,succulent,tree,Foliar.Cover.Pct,Actual.Eco.Site)

AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)

Covers<-c("Bare Soil", "Forb","Grass", "Litter", "Rock","Shrub","Succulent","Tree","Foliar")
AIMdata2$covertype <- factor(AIMdata2$covertype, levels = c("Bare.Soil.Pct","Forb.Cover.Pct.Any.Hit","Grass.Cover.Pct.Any.Hit","Total.Litter.Cover.Pct.First.Hit","Rock.Cover.Pct.First.Hit","shrub.subshrub","succulent","tree","Foliar.Cover.Pct","Actual.Eco.Site"))

BH<-subset(AIMdata2,Actual.Eco.Site=="BH")
LB<-subset(AIMdata2,Actual.Eco.Site=="LB")
LOA<-subset(AIMdata2,Actual.Eco.Site=="LOA")
MO<-subset(AIMdata2,Actual.Eco.Site=="MO")
OTH<-subset(AIMdata2,Actual.Eco.Site=="OTH")
ROF<-subset(AIMdata2,Actual.Eco.Site=="ROF")
SAL<-subset(AIMdata2,Actual.Eco.Site=="SAL")
SAN<-subset(AIMdata2,Actual.Eco.Site=="SAN")
FORE<-subset(AIMdata2,Actual.Eco.Site=="FORE")

BHpop<-paste("BH Surface Cover   n=",BHN2)
LBpop<-paste("LB Surface Cover   n=",LBN2)
LOApop<-paste("LOA Surface Cover   n=",LOAN2)
MOpop<-paste("MO Surface Cover   n=",MON2)
OTHpop<-paste("OTH Surface Cover   n=",OTHN2)
ROFpop<-paste("ROF Surface Cover   n=",ROFN2)
SALpop<-paste("SAL Surface Cover   n=",SALN2)
SANpop<-paste("SAN Surface Cover   n=",SANN2)
FOREpop<-paste("FORE Surface Cover   n=",FOREN2)

SC<-ggplot(BH,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(BHpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(LB,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(LBpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(LOA,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(LOApop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(MO,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(MOpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(OTH,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(OTHpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(ROF,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(ROFpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(SAL,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(SALpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(SAN,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(SANpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

SC<-ggplot(FORE,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(FOREpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
