#packages installed= data.table, dplyr, formattable, tidyr using function install.packages("") for example install.packages ("tidyr")
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

#Plotdata has the actual.eco.site assigned, the "match" code below is adding a column that will assign actual.eco.site to every row
##using the Plotdata csv
Plotdata<-read.csv("~/Allyears_plots.csv")
soildata<-read.csv("~/Allyears_soilhorizons.csv")
soildata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(soildata$PrimaryKey,Plotdata$PrimaryKey)]
AIMdata<-read.csv("~/Allyears_query.csv")
AIMdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(AIMdata$Primary.Key,Plotdata$PrimaryKey)]
PSPSdata<-read.csv("~/PSPPALL.csv")
PSPSdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(PSPSdata$PrimaryKey,Plotdata$PrimaryKey)]
SPdata<-read.csv("~/Allyears_species_rich.csv")
SPdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(SPdata$PrimaryKey,Plotdata$PrimaryKey)]
ST<-read.csv("~/LPI_all.csv")
#when downloading LPI detail from TerrADat online you will have to label the PrimaryKey column PrimaryKey, you can use the View()
##command to ensure you're matching with the correct column
ST<-ST %>%
  select(HeightWoody,HeightHerbaceous,SpeciesWoody,SpeciesHerbaceous,PrimaryKey)
STN<-na.omit(ST)
STN$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(STN$PrimaryKey,Plotdata$PrimaryKey)]

#replace with abbreviation of your ecosite. abbrev of eco sites in quotes in target()
#which can be done using ctrl + f , replace all 
#summary section (compares all strata)
#native vs nonnative
target<-c("BH","LB","LOA","MO","OTH","ROF","SAL","SAN","FORE")
AIMdata1<-AIMdata%>%
  select(Noxious.Cover.Pct.Any.Hit,NonNoxious.Plant.Cover.Pct.Any.Hit,Actual.Eco.Site) %>%
  filter(Actual.Eco.Site %in% target)



AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)

Covers<-c("Nonnox.", "Noxious")
NN<-ggplot(AIMdata2,aes(x=covertype,y=covervalue, col=covertype))+geom_point(alpha=0.4)
NN2<-NN+geom_boxplot()
NN3<-NN2+ggtitle("Noxious and Nonnoxious Species Comparison by Strata")+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
NN4<-NN3+scale_x_discrete(labels= Covers)+scale_color_manual(values=c("seagreen2","darkred"),labels = c("Nonnoxious", "Noxious"))+theme(axis.text.x=element_text(colour="gray20"))
NN4+facet_wrap(~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

#perennial and annual grass
AIMdata1<-AIMdata %>%
  select(Perennial.Grass.Cover.Pct.Any.Hit,Annual.Grass.Cover.Pct.Any.Hit,Actual.Eco.Site) %>%
  filter(Actual.Eco.Site %in% target)
AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)

Covers<-c("Annual Grass","Perennial Grass")
G<-ggplot(AIMdata2, aes(x=covertype,y=covervalue,col=covertype))
G2<-G+geom_boxplot()
G3<-G2+ggtitle(" Percent Cover of Annual and Perennial Grass")+labs(x="Duration",y="Average Percent Cover",colour="Duration")+scale_x_discrete(labels= Covers)
G3+facet_wrap(.~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))+ scale_color_manual(values=c("paleturquoise3", "rosybrown3"),labels = c("Perennial", "Annual"))

#surface cover
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

#stratum description, general using TerrADat form "Plots"
BH<-subset(Plotdata,Actual.Eco.Site=="BH")
LB<-subset(Plotdata,Actual.Eco.Site=="LB")
LOA<-subset(Plotdata,Actual.Eco.Site=="LOA")
MO<-subset(Plotdata,Actual.Eco.Site=="MO")
OTH<-subset(Plotdata,Actual.Eco.Site=="OTH")
ROF<-subset(Plotdata,Actual.Eco.Site=="ROF")
SAL<-subset(Plotdata,Actual.Eco.Site=="SAL")
SAN<-subset(Plotdata,Actual.Eco.Site=="SAN")
FORE<-subset(Plotdata,Actual.Eco.Site=="FORE")

summary(BH$AvgPrecip)
summary(BH$Slope)
summary(BH$Elevation)
summary(BH$EcolSite)
summary(BH$LandscapeType)
#
summary(LB$AvgPrecip)
summary(LB$Slope)
summary(LB$Elevation)
summary(LB$EcolSite)
summary(LB$LandscapeType)
#
summary(LOA$AvgPrecip)
summary(LOA$Slope)
summary(LOA$Elevation)
summary(LOA$EcolSite)
summary(LOA$LandscapeType)
#
summary(MO$AvgPrecip)
summary(MO$Slope)
summary(MO$Elevation)
summary(MO$EcolSite)
summary(MO$LandscapeType)
#
summary(OTH$AvgPrecip)
summary(OTH$Slope)
summary(OTH$Elevation)
summary(OTH$EcolSite)
summary(OTH$LandscapeType)
#
summary(ROF$AvgPrecip)
summary(ROF$Slope)
summary(ROF$Elevation)
summary(ROF$EcolSite)
summary(ROF$LandscapeType)
#
summary(SAL$AvgPrecip)
summary(SAL$Slope)
summary(SAL$Elevation)
summary(SAL$EcolSite)
summary(SAL$LandscapeType)
#
summary(SAN$AvgPrecip)
summary(SAN$Slope)
summary(SAN$Elevation)
summary(SAN$EcolSite)
summary(SAN$LandscapeType)
#
summary(FORE$AvgPrecip)
summary(FORE$Slope)
summary(FORE$Elevation)
summary(FORE$EcolSite)
summary(FORE$LandscapeType)

#stratum description, soil pits using TerrADat Soil Horizons records
BH<-subset(soildata,Actual.Eco.Site=="BH")
LB<-subset(soildata,Actual.Eco.Site=="LB")
LOA<-subset(soildata,Actual.Eco.Site=="LOA")
MO<-subset(soildata,Actual.Eco.Site=="MO")
OTH<-subset(soildata,Actual.Eco.Site=="OTH")
ROF<-subset(soildata,Actual.Eco.Site=="ROF")
SAL<-subset(soildata,Actual.Eco.Site=="SAL")
SAN<-subset(soildata,Actual.Eco.Site=="SAN")
FORE<-subset(soildata,Actual.Eco.Site=="FORE")

summary(BH$Texture)
summary(BH$RockFragments)
summary(BH$Effer)
summary(BH$ESD_PctClay)
#
summary(LB$Texture)
summary(LB$RockFragments)
summary(LB$Effer)
summary(LB$ESD_PctClay)
#
summary(LOA$Texture)
summary(LOA$RockFragments)
summary(LOA$Effer)
summary(LOA$ESD_PctClay)
#
summary(MO$Texture)
summary(MO$RockFragments)
summary(MO$Effer)
summary(MO$ESD_PctClay)
#
summary(OTH$Texture)
summary(OTH$RockFragments)
summary(OTH$Effer)
summary(OTH$ESD_PctClay)
#
summary(ROF$Texture)
summary(ROF$RockFragments)
summary(ROF$Effer)
summary(ROF$ESD_PctClay)
#
summary(SAL$Texture)
summary(SAL$RockFragments)
summary(SAL$Effer)
summary(SAL$ESD_PctClay)
#
summary(SAN$Texture)
summary(SAN$RockFragments)
summary(SAN$Effer)
summary(SAN$ESD_PctClay)
#
summary(FORE$Texture)
summary(FORE$RockFragments)
summary(FORE$Effer)
summary(FORE$ESD_PctClay)
#

#soil stability
AIMdata1<-AIMdata %>%
  select(Soil.Stability.All,Soil.Stability.Protected,Soil.Stability.Unprotected,Actual.Eco.Site)

BH<-subset(AIMdata1,Actual.Eco.Site=="BH")
LB<-subset(AIMdata1,Actual.Eco.Site=="LB")
LOA<-subset(AIMdata1,Actual.Eco.Site=="LOA")
MO<-subset(AIMdata1,Actual.Eco.Site=="MO")
OTH<-subset(AIMdata1,Actual.Eco.Site=="OTH")
ROF<-subset(AIMdata1,Actual.Eco.Site=="ROF") 
SAL<-subset(AIMdata1,Actual.Eco.Site=="SAL")
SAN<-subset(AIMdata1,Actual.Eco.Site=="SAN")
FORE<-subset(AIMdata1,Actual.Eco.Site=="FORE")

#BH
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(BH$Soil.Stability.All),digits=2),round(mean(BH$Soil.Stability.Protected),digits=2),round(mean(BH$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(BH$Soil.Stability.All),digits=2),round(min(BH$Soil.Stability.Protected),digits=2),round(min(BH$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(BH$Soil.Stability.All),digits=2),round(max(BH$Soil.Stability.Protected),digits=2),round(max(BH$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(BH$Soil.Stability.All),digits=2),round(sd(BH$Soil.Stability.Protected),digits=2),round(sd(BH$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(BH$Soil.Stability.All)/BHN2),digits=2),round(1.282*(sd(BH$Soil.Stability.Protected)/BHN2),digits=2),round(1.282*(sd(BH$Soil.Stability.Unprotected)/BHN2),digits=2))
BHdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(BHdataframe, list(
  Type = blank_bold))


#LB
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(LB$Soil.Stability.All),digits=2),round(mean(LB$Soil.Stability.Protected),digits=2),round(mean(LB$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(LB$Soil.Stability.All),digits=2),round(min(LB$Soil.Stability.Protected),digits=2),round(min(LB$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(LB$Soil.Stability.All),digits=2),round(max(LB$Soil.Stability.Protected),digits=2),round(max(LB$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(LB$Soil.Stability.All),digits=2),round(sd(LB$Soil.Stability.Protected),digits=2),round(sd(LB$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(LB$Soil.Stability.All)/LBN2),digits=2),round(1.282*(sd(LB$Soil.Stability.Protected)/LBN2),digits=2),round(1.282*(sd(LB$Soil.Stability.Unprotected)/LBN2),digits=2))
LBdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(LBdataframe, list(
  Type = blank_bold))

#LOA
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(LOA$Soil.Stability.All),digits=2),round(mean(LOA$Soil.Stability.Protected),digits=2),round(mean(LOA$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(LOA$Soil.Stability.All),digits=2),round(min(LOA$Soil.Stability.Protected),digits=2),round(min(LOA$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(LOA$Soil.Stability.All),digits=2),round(max(LOA$Soil.Stability.Protected),digits=2),round(max(LOA$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(LOA$Soil.Stability.All),digits=2),round(sd(LOA$Soil.Stability.Protected),digits=2),round(sd(LOA$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(LOA$Soil.Stability.All)/LOAN2),digits=2),round(1.282*(sd(LOA$Soil.Stability.Protected)/LOAN2),digits=2),round(1.282*(sd(LOA$Soil.Stability.Unprotected)/LOAN2),digits=2))
LOAdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(LOAdataframe, list(
  Type = blank_bold))


#MO
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(MO$Soil.Stability.All),digits=2),round(mean(MO$Soil.Stability.Protected),digits=2),round(mean(MO$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(MO$Soil.Stability.All),digits=2),round(min(MO$Soil.Stability.Protected),digits=2),round(min(MO$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(MO$Soil.Stability.All),digits=2),round(max(MO$Soil.Stability.Protected),digits=2),round(max(MO$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(MO$Soil.Stability.All),digits=2),round(sd(MO$Soil.Stability.Protected),digits=2),round(sd(MO$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(MO$Soil.Stability.All)/MON2),digits=2),round(1.282*(sd(MO$Soil.Stability.Protected)/MON2),digits=2),round(1.282*(sd(MO$Soil.Stability.Unprotected)/MON2),digits=2))
MOdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(MOdataframe, list(
  Type = blank_bold))

#OTH
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(OTH$Soil.Stability.All),digits=2),round(mean(OTH$Soil.Stability.Protected),digits=2),round(mean(OTH$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(OTH$Soil.Stability.All),digits=2),round(min(OTH$Soil.Stability.Protected),digits=2),round(min(OTH$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(OTH$Soil.Stability.All),digits=2),round(max(OTH$Soil.Stability.Protected),digits=2),round(max(OTH$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(OTH$Soil.Stability.All),digits=2),round(sd(OTH$Soil.Stability.Protected),digits=2),round(sd(OTH$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(OTH$Soil.Stability.All)/OTHN2),digits=2),round(1.282*(sd(OTH$Soil.Stability.Protected)/OTHN2),digits=2),round(1.282*(sd(OTH$Soil.Stability.Unprotected)/OTHN2),digits=2))
OTHdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(OTHdataframe, list(
  Type = blank_bold))

#ROF
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(ROF$Soil.Stability.All),digits=2),round(mean(ROF$Soil.Stability.Protected),digits=2),round(mean(ROF$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(ROF$Soil.Stability.All),digits=2),round(min(ROF$Soil.Stability.Protected),digits=2),round(min(ROF$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(ROF$Soil.Stability.All),digits=2),round(max(ROF$Soil.Stability.Protected),digits=2),round(max(ROF$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(ROF$Soil.Stability.All),digits=2),round(sd(ROF$Soil.Stability.Protected),digits=2),round(sd(ROF$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(ROF$Soil.Stability.All)/ROFN2),digits=2),round(1.282*(sd(ROF$Soil.Stability.Protected)/ROFN2),digits=2),round(1.282*(sd(ROF$Soil.Stability.Unprotected)/ROFN2),digits=2))
ROFdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(ROFdataframe, list(
  Type = blank_bold))

#SAL
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(SAL$Soil.Stability.All),digits=2),round(mean(SAL$Soil.Stability.Protected),digits=2),round(mean(SAL$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(SAL$Soil.Stability.All),digits=2),round(min(SAL$Soil.Stability.Protected),digits=2),round(min(SAL$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(SAL$Soil.Stability.All),digits=2),round(max(SAL$Soil.Stability.Protected),digits=2),round(max(SAL$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(SAL$Soil.Stability.All),digits=2),round(sd(SAL$Soil.Stability.Protected),digits=2),round(sd(SAL$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(SAL$Soil.Stability.All)/SALN2),digits=2),round(1.282*(sd(SAL$Soil.Stability.Protected)/SALN2),digits=2),round(1.282*(sd(SAL$Soil.Stability.Unprotected)/SALN2),digits=2))
SALdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(SALdataframe, list(
  Type = blank_bold))

#SAN
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(SAN$Soil.Stability.All),digits=2),round(mean(SAN$Soil.Stability.Protected),digits=2),round(mean(SAN$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(SAN$Soil.Stability.All),digits=2),round(min(SAN$Soil.Stability.Protected),digits=2),round(min(SAN$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(SAN$Soil.Stability.All),digits=2),round(max(SAN$Soil.Stability.Protected),digits=2),round(max(SAN$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(SAN$Soil.Stability.All),digits=2),round(sd(SAN$Soil.Stability.Protected),digits=2),round(sd(SAN$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(SAN$Soil.Stability.All)/SANN2),digits=2),round(1.282*(sd(SAN$Soil.Stability.Protected)/SANN2),digits=2),round(1.282*(sd(SAN$Soil.Stability.Unprotected)/SANN2),digits=2))
SANdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(SANdataframe, list(
  Type = blank_bold))

#FORE
Type<-c("All Samples","Under Plant Cover","No Cover")
Mean<-c(round(mean(FORE$Soil.Stability.All),digits=2),round(mean(FORE$Soil.Stability.Protected),digits=2),round(mean(FORE$Soil.Stability.Unprotected),digits=2))
Minimum<-c(round(min(FORE$Soil.Stability.All),digits=2),round(min(FORE$Soil.Stability.Protected),digits=2),round(min(FORE$Soil.Stability.Unprotected),digits=2))
Maximum<-c(round(max(FORE$Soil.Stability.All),digits=2),round(max(FORE$Soil.Stability.Protected),digits=2),round(max(FORE$Soil.Stability.Unprotected),digits=2))
St.Dev.<-c(round(sd(FORE$Soil.Stability.All),digits=2),round(sd(FORE$Soil.Stability.Protected),digits=2),round(sd(FORE$Soil.Stability.Unprotected),digits=2))
ME<-c(round(1.282*(sd(FORE$Soil.Stability.All)/FOREN2),digits=2),round(1.282*(sd(FORE$Soil.Stability.Protected)/FOREN2),digits=2),round(1.282*(sd(FORE$Soil.Stability.Unprotected)/FOREN2),digits=2))
FOREdataframe<-data.frame(Type,Mean,Minimum,Maximum,ME)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(FOREdataframe, list(
  Type = blank_bold))

#calculating surface cover

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

#for a comparison of all strata see summary code
#BH
SC<-ggplot(BH,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(BHpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#LB
SC<-ggplot(LB,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(LBpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#LOA
SC<-ggplot(LOA,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(LOApop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#MO
SC<-ggplot(MO,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(MOpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#OTH
SC<-ggplot(OTH,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(OTHpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#ROF
SC<-ggplot(ROF,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(ROFpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#SAL
SC<-ggplot(SAL,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(SALpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#SAN
SC<-ggplot(SAN,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(SANpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))
#FORE
SC<-ggplot(FORE,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(FOREpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = Covers)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

#Dominant Species
##if a blank table is produced, no species meet the criteria of being present on 20% of the plots. To adjust criteria change proportion on filter(dom_ss>=.2)
##SS_avg is the average species per plot. The first table produced lists species that are only on 20% of the plots and ranks the
##species from greatest cover per plot to the least cover per plot

PSPS1<-PSPSdata[!is.na(PSPSdata$AH_SpeciesCover),]

##BH STRATA
##list of dominant species in order of species with highest percent occurence per plot
BHD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="BH") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey))) 
BH<-BHD %>%
  mutate(zero=(length(unique(BHD$PrimaryKey))-N_category)) %>%
  arrange(desc(SS_avg))
PSPS3<-BH[!duplicated(BH$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
BH1<-head(PSPS3,4)
BH2<-BH1 %>%
  select(Species,zero)
formattable(BH2)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
#insert your dominant species code below, do not get rid of the quotes!
domss<-c("BOGR2","HECO26","KRLA2","PIED")
BHA<-BH %>%
  filter(Species %in% domss)
BH6<-BHA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
BH7<-as.data.frame(BH6)
#(species name, zero #)
A<-rep("BOGR2",2)
B<-rep("HECO26",22)
C<-rep("KRLA2",22)
D<-rep("PIED",30)
#(growthhabit, zero #)
A2<-rep("NonWoody",2)
B2<-rep("NonWoody",22)
C2<-rep("Woody",22)
D2<-rep("Woody",30)
Species_list<-c(A,B,C,D)
#the first number should be the total of all the zero values you are adding
AH_SpeciesCover_list<-replicate(76,0)
#the second number (after the colon) is also the total of all the zero values being added
PrimaryKey_list<-1:76
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
#the below line of code will generate a warning message due to primarykey for zeros being NA, continue with the code
BH8<-rbind(as.data.frame(BH7),zeros)
##BOX PLOT TITLES
BHpop<-paste("BH Percent Cover of Dominant Species   n=",BHN2)
LBpop<-paste("LB Percent Cover of Dominant Species   n=",LBN2)
LOApop<-paste("LOA Percent Cover of Dominant Species   n=",LOAN2)
MOpop<-paste("MO Percent Cover of Dominant Species   n=",MON2)
OTHpop<-paste("OTH Percent Cover of Dominant Species   n=",OTHN2)
ROFpop<-paste("ROF Percent Cover of Dominant Species   n=",ROFN2)
SALpop<-paste("SAL Percent Cover of Dominant Species   n=",SALN2)
SANpop<-paste("SAN Percent Cover of Dominant Species   n=",SANN2)
FOREpop<-paste("FORE Percent Cover of Dominant Species   n=",FOREN2)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(BH8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(BH8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(BHpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##LB STRATA
##list of dominant species in order of species with highest percent occurence per plot, first table gives species only on 20% of the plots
##and lists from greatest cover per plot to least (SS-avg) is average species per plot
LBD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="LB") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)))
LB<- LBD %>%
  mutate(zero=(length(unique(LBD$PrimaryKey))-N_category)) %>%
  arrange(desc(SS_avg))
PSPS3<-LB[!duplicated(LB$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
LB1<-head(PSPS3,4)
LB2<-LB1 %>%
  select(Species,zero)
formattable(LB2)


##adding plots with 0 value, replace domss and species name and zero # based on results of last step
#replace the species codes below with the species codes generated in the last table, keep the quotes
domss<-c("BOGR2","KRLA2","PASM","SATR12")
LBA<-LB %>%
  filter(Species %in% domss)
LB6<-LBA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
LB7<-as.data.frame(LB6)
#(species name, zero #)
A<-rep("BOGR2",4)
B<-rep("KRLA2",12)
C<-rep("PASM",30)
D<-rep("SATR12",33)
#(growthhabit, zero #)
A2<-rep("NonWoody",4)
B2<-rep("Woody",12)
C2<-rep("NonWoody",30)
D2<-rep("NonWoody",33)
Species_list<-c(A,B,C,D)
#the first number is the total of all zero values being addded, and so is the number after the colon two lines below
AH_SpeciesCover_list<-replicate(79,0)
PrimaryKey_list<-1:79
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
LB8<-rbind(as.data.frame(LB7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(LB8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(LB8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(LBpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##LOA STRATA
##list of dominant species in order of species with highest percent occurence per plot
LOAD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="LOA") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)))
LOA<- LOAD %>%
  mutate(zero=(length(unique(LOAD$PrimaryKey))-N_category)) %>%
  arrange(desc(SS_avg))
PSPS3<-LOA[!duplicated(LOA$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
LOA1<-head(PSPS3,4)
LOA2<-LOA1 %>%
  select(Species,zero)
formattable(LOA2)


##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","MUMO","MUTO2","PIED")
LOAA<-LOA %>%
  filter(Species %in% domss)
LOA6<-LOAA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
LOA7<-as.data.frame(LOA6)
#(species name, zero #)
A<-rep("BOGR2",2)
B<-rep("MUMO",19)
C<-rep("MUTO2",21)
D<-rep("PIED",21)
#(growthhabit, zero #)
A2<-rep("NonWoody",2)
B2<-rep("NonWoody",19)
C2<-rep("NonWoody",21)
D2<-rep("Woody",21)
Species_list<-c(A,B,C,D)
AH_SpeciesCover_list<-replicate(63,0)
PrimaryKey_list<-1:63
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
LOA8<-rbind(as.data.frame(LOA7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(LOA8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(LOA8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(LOApop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##MO STRATA
##list of dominant species in order of species with highest percent occurence per plot
MOD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="MO") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)))
MO<- MOD %>%
  mutate(zero=(length(unique(MOD$PrimaryKey))-N_category)) %>%
  arrange(desc(SS_avg))
PSPS3<-MO[!duplicated(MO$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
MO1<-head(PSPS3,4)
MO2<-MO1 %>%
  select(Species,zero)
formattable(MO2)


##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","PASM","CHGR6")
MOA<-MO %>%
  filter(Species %in% domss)
MO6<-MOA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
MO7<-as.data.frame(MO6)
#(species name, zero #)
A<-rep("BOGR2",1)
B<-rep("HECO26",15)
C<-rep("PASM",16)
D<-rep("CHGR6",4)
#(growthhabit, zero #)
A2<-rep("NonWoody",1)
B2<-rep("NonWoody",15)
C2<-rep("NonWoody",16)
D2<-rep("Woody",4)
Species_list<-c(A,B,C,D)
AH_SpeciesCover_list<-replicate(36,0)
PrimaryKey_list<-1:36
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
MO8<-rbind(as.data.frame(MO7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(MO8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(MO8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(MOpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##OTH STRATA
##list of dominant species in order of species with highest percent occurence per plot
OTHD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="OTH") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)))
OTH<- OTHD %>%
  mutate(zero=length(unique(OTHD$PrimaryKey))-N_category) %>%
  arrange(desc(SS_avg))
PSPS3<-OTH[!duplicated(OTH$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
OTH1<-head(PSPS3,4)
OTH2<-OTH1 %>%
  select(Species,zero)
formattable(OTH2)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("PIED","HECO26","PIMI","MUMO")
OTHA<-OTH %>%
  filter(Species %in% domss)
OTH6<-OTHA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
OTH7<-as.data.frame(OTH6)
#(species name, zero #)
A<-rep("PIED",10)
B<-rep("HECO26",15)
C<-rep("PIMI",15)
D<-rep("MUMO",11)
#(growthhabit, zero #)
A2<-rep("Woody",10)
B2<-rep("NonWoody",15)
C2<-rep("NonWoody",15)
D2<-rep("NonWoody",11)
Species_list<-c(A,B,C,D)
AH_SpeciesCover_list<-replicate(51,0)
PrimaryKey_list<-1:51
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
OTH8<-rbind(as.data.frame(OTH7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(OTH8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(OTH8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(OTHpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##ROF STRATA
##list of dominant species in order of species with highest percent occurence per plot
ROFD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="ROF") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey))) 
ROF<-ROFD %>%
  mutate(zero=length(unique(ROFD$PrimaryKey))-N_category) %>%
  arrange(desc(SS_avg))
PSPS3<-ROF[!duplicated(ROF$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
ROF1<-head(PSPS3,4)
ROF2<-ROF1 %>%
  select(Species,zero)
formattable(ROF2)


##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","JUMO","PIED")
ROFA<-ROF %>%
  filter(Species %in% domss)
ROF6<-ROFA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
ROF7<-as.data.frame(ROF6)
#(species name, zero #)
A<-rep("BOGR2",1)
B<-rep("HECO26",18)
C<-rep("JUMO",17)
D<-rep("PIED",7)
#(growthhabit, zero #)
A2<-rep("NonWoody",1)
B2<-rep("NonWoody",18)
C2<-rep("Woody",17)
D2<-rep("Woody",7)
Species_list<-c(A,B,C,D)
AH_SpeciesCover_list<-replicate(43,0)
PrimaryKey_list<-1:43
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
ROF8<-rbind(as.data.frame(ROF7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(ROF8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(ROF8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(ROFpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##SAL STRATA
##list of dominant species in order of species with highest percent occurence per plot
SALD<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="SAL") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)))
SAL<-SALD %>%
  mutate(zero=length(unique(SALD$PrimaryKey))-N_category) %>%
  arrange(desc(SS_avg))
PSPS3<-SAL[!duplicated(SAL$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
SAL1<-head(PSPS3,4)
SAL2<-SAL1 %>%
  select(Species,zero)
formattable(SAL2)


##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("DISP","SAVE4","ERCE2")
SALA<-SAL %>%
  filter(Species %in% domss)
SAL6<-SALA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
SAL7<-as.data.frame(SAL6)
#(species name, zero #)
A<-rep("DISP",6)
B<-rep("SAVE4",2)
C<-rep("ERCE2",12)

#(growthhabit, zero #)
A2<-rep("NonWoody",6)
B2<-rep("Woody",2)
C2<-rep("NonWoody",12)

Species_list<-c(A,B,C)
AH_SpeciesCover_list<-replicate(20,0)
PrimaryKey_list<-1:20
GrowthHabit_List<-c(A2,B2,C2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
SAL8<-rbind(as.data.frame(SAL7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(SAL8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(SAL8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(SALpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##SAN STRATA
##list of dominant species in order of species with highest percent occurence per plot
SAND<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="SAN") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)))
SAN<-SAND %>%
  mutate(zero=length(unique(SAND$PrimaryKey))-N_category) %>%
  arrange(desc(SS_avg))
PSPS3<-SAN[!duplicated(SAN$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
SAN1<-head(PSPS3,4)
SAN2<-SAN1 %>%
  select(Species,zero)
formattable(SAN2)


##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","ACHY","SPCR","DISP")
SANA<-SAN %>%
  filter(Species %in% domss)
SAN6<-SANA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
SAN7<-as.data.frame(SAN6)
#(species name, zero #)
A<-rep("BOGR2",9)
B<-rep("ACHY",9)
C<-rep("SPCR",13)
D<-rep("DISP",16)
#(growthhabit, zero #)
A2<-rep("NonWoody",9)
B2<-rep("NonWoody",9)
C2<-rep("NonWoody",13)
D2<-rep("NonWoody",16)
Species_list<-c(A,B,C,D)
AH_SpeciesCover_list<-replicate(47,0)
PrimaryKey_list<-1:47
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
SAN8<-rbind(as.data.frame(SAN7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(SAN8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(SAN8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(SANpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

##FORE STRATA
##list of dominant species in order of species with highest percent occurence per plot
FORED<- PSPS1 %>%
  select(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site,GrowthHabit) %>%
  group_by(Actual.Eco.Site) %>%
  filter(Actual.Eco.Site=="FORE") %>%
  group_by(Species) %>%
  mutate(N_category=n()) %>%
  count(PrimaryKey,AH_SpeciesCover,Species,Actual.Eco.Site, GrowthHabit,N_category) %>%
  ungroup() %>%
  mutate(dom_ss=N_category/length(unique(PrimaryKey))) %>%
  filter(dom_ss >= .2) %>%
  group_by(Species) %>%
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey))) 
FORE<-FORED %>%
  mutate(zero=length(unique(FORED$PrimaryKey))-N_category) %>%
  arrange(desc(SS_avg))
PSPS3<-FORE[!duplicated(FORE$Species),]
PSPS4<- PSPS3 %>%
  select(Species,SS_avg)

formattable(PSPS4)

##the zero output will show how many plots for each species need to have a 0 value added in order to properly represent the data
FORE1<-head(PSPS3,4)
FORE2<-FORE1 %>%
  select(Species,zero)
formattable(FORE2)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("PIFL2","PIED","PSME","FEAR2")
FOREA<-FORE %>%
  filter(Species %in% domss)
FORE6<-FOREA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
FORE7<-as.data.frame(FORE6)
#(species name, zero #)
A<-rep("PIFL2",7)
B<-rep("PIED",6)
C<-rep("PSME",4)
D<-rep("FEAR2",9)
#(growthhabit, zero #)
A2<-rep("Woody",7)
B2<-rep("Woody",6)
C2<-rep("Woody",4)
D2<-rep("NonWoody",9)
Species_list<-c(A,B,C,D)
AH_SpeciesCover_list<-replicate(26,0)
PrimaryKey_list<-1:26
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
FORE8<-rbind(as.data.frame(FORE7),zeros)

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(FORE8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(FORE8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(FOREpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale

#canopy gap chart
Gapdata<-AIMdata %>%
  select(Gap.Cover.25.to.50.Pct,Gap.Cover.51.to.100.Pct,Gap.Cover.101.to.200.Pct,Gap.Cover.200.Plus.Pct,Actual.Eco.Site)
Gapdata$CanopyCover<-100-(AIMdata$Gap.Cover.25.to.50.Pct+AIMdata$Gap.Cover.51.to.100.Pct+AIMdata$Gap.Cover.101.to.200.Pct+AIMdata$Gap.Cover.200.Plus.Pct)
Gapdata$Gaps<-Gapdata$Gap.Cover.25.to.50.Pct+Gapdata$Gap.Cover.51.to.100.Pct+Gapdata$Gap.Cover.101.to.200.Pct+Gapdata$Gap.Cover.200.Plus.Pct

BHG<-subset(Gapdata,Actual.Eco.Site=="BH")
LBG<-subset(Gapdata,Actual.Eco.Site=="LB")
LOAG<-subset(Gapdata,Actual.Eco.Site=="LOA")
MOG<-subset(Gapdata,Actual.Eco.Site=="MO")
OTHG<-subset(Gapdata,Actual.Eco.Site=="OTH")
ROFG<-subset(Gapdata,Actual.Eco.Site=="ROF")
SALG<-subset(Gapdata,Actual.Eco.Site=="SAL")
SANG<-subset(Gapdata,Actual.Eco.Site=="SAN")
FOREG<-subset(Gapdata,Actual.Eco.Site=="FORE")
#
summary(BHG$Gaps)
summary(BHG$CanopyCover)
#
summary(LBG$Gaps)
summary(LBG$CanopyCover)
#
summary(LOAG$Gaps)
summary(LOAG$CanopyCover)
#
summary(MOG$Gaps)
summary(MOG$CanopyCover)
#
summary(OTHG$Gaps)
summary(OTHG$CanopyCover)
#
summary(ROFG$Gaps)
summary(ROFG$CanopyCover)
#
summary(SALG$Gaps)
summary(SALG$CanopyCover)
#
summary(SANG$Gaps)
summary(SANG$CanopyCover)
#
summary(FOREG$Gaps)
summary(FOREG$CanopyCover)
#
#gap barchart
Gapdata<-AIMdata %>%
  select(Gap.Cover.25.to.50.Pct,Gap.Cover.51.to.100.Pct,Gap.Cover.101.to.200.Pct,Gap.Cover.200.Plus.Pct,Actual.Eco.Site)
Gapdata$CanopyCover<-100-(AIMdata$Gap.Cover.25.to.50.Pct+AIMdata$Gap.Cover.51.to.100.Pct+AIMdata$Gap.Cover.101.to.200.Pct+AIMdata$Gap.Cover.200.Plus.Pct)
Gapdata2<-gather(Gapdata,Gapsize,Percentofplot,-Actual.Eco.Site)
Gapdata2$Gapsize <- factor(Gapdata2$Gapsize, levels = c("CanopyCover","Gap.Cover.25.to.50.Pct", "Gap.Cover.51.to.100.Pct", "Gap.Cover.101.to.200.Pct","Gap.Cover.200.Plus.Pct"))

BHG2<-subset(Gapdata2,Actual.Eco.Site=="BH")
LBG2<-subset(Gapdata2,Actual.Eco.Site=="LB")
LOAG2<-subset(Gapdata2,Actual.Eco.Site=="LOA")
MOG2<-subset(Gapdata2,Actual.Eco.Site=="MO")
OTHG2<-subset(Gapdata2,Actual.Eco.Site=="OTH")
ROFG2<-subset(Gapdata2,Actual.Eco.Site=="ROF")
SALG2<-subset(Gapdata2,Actual.Eco.Site=="SAL")
SANG2<-subset(Gapdata2,Actual.Eco.Site=="SAN")
FOREG2<-subset(Gapdata2,Actual.Eco.Site=="FORE")

BHpop<-paste("BH Proportion Canopy Gap and Cover   n=",BHN2)
LBpop<-paste("LB Proportion Canopy Gap and Cover   n=",LBN2)
LOApop<-paste("LOA Proportion Canopy Gap and Cover   n=",LOAN2)
MOpop<-paste("MO Proportion Canopy Gap and Cover   n=",MON2)
OTHpop<-paste("OTH Proportion Canopy Gap and Cover   n=",OTHN2)
ROFpop<-paste("ROF Proportion Canopy Gap and Cover   n=",ROFN2)
SALpop<-paste("SAL Proportion Canopy Gap and Cover   n=",SALN2)
SANpop<-paste("SAN Proportion Canopy Gap and Cover   n=",SANN2)
FOREpop<-paste("FORE Proportion Canopy Gap and Cover   n=",FOREN2)

GC<-ggplot(BHG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(LBG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(LBpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(LOAG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(LOApop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(MOG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(MOpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(OTHG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(OTHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(ROFG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(ROFpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(SALG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(SALpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(SANG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(SANpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
GC<-ggplot(FOREG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(FOREpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)
#
#surface indicator summary tables, sagebrush height not included, tree and succulent height can be calculated using LPI detail

#SPECIES RICHNESS
BHSR<-subset(SPdata,Actual.Eco.Site=="BH")
LBSR<-subset(SPdata,Actual.Eco.Site=="LB")
LOASR<-subset(SPdata,Actual.Eco.Site=="LOA")
MOSR<-subset(SPdata,Actual.Eco.Site=="MO")
OTHSR<-subset(SPdata,Actual.Eco.Site=="OTH")
ROFSR<-subset(SPdata,Actual.Eco.Site=="ROF")
SALSR<-subset(SPdata,Actual.Eco.Site=="SAL")
SANSR<-subset(SPdata,Actual.Eco.Site=="SAN")
FORESR<-subset(SPdata,Actual.Eco.Site=="FORE")

BH<-subset(AIMdata,Actual.Eco.Site=="BH")
LB<-subset(AIMdata,Actual.Eco.Site=="LB")
LOA<-subset(AIMdata,Actual.Eco.Site=="LOA")
MO<-subset(AIMdata,Actual.Eco.Site=="MO")
OTH<-subset(AIMdata,Actual.Eco.Site=="OTH")
ROF<-subset(AIMdata,Actual.Eco.Site=="ROF")
SAL<-subset(AIMdata,Actual.Eco.Site=="SAL")
SAN<-subset(AIMdata,Actual.Eco.Site=="SAN")
FORE<-subset(AIMdata,Actual.Eco.Site=="FORE")

#for the below code replace "insert dom ss..." with dominant species names for the correct functional group. if none = "NA" . If more than one
#for a function al group the name has to be added to Indicator as "species name", 
#then added to average using round(mean(BHA$Species=="insert stratas dominant grass species, if none = NA"), digits=2),
#added to min using round(min(BHA$Species=="insert stratas dominant grass species, if none = NA"), digits=2),  
#added to max and St.Dev using same code replacing the "min" with "max" and "sd"
#calculate shrub and tree height data using LPI detail

PS2<-PSPSdata[!duplicated(PSPSdata$Species),]

STN$GrowthHabitsub<-PS2$GrowthHabitSub[match(STN$SpeciesWoody,PS2$Species)]  

BHST<-subset(STN,Actual.Eco.Site=="BH")
LBST<-subset(STN,Actual.Eco.Site=="LB")
LOAST<-subset(STN,Actual.Eco.Site=="LOA")
MOST<-subset(STN,Actual.Eco.Site=="MO")
OTHST<-subset(STN,Actual.Eco.Site=="OTH")
ROFST<-subset(STN,Actual.Eco.Site=="ROF")
SALST<-subset(STN,Actual.Eco.Site=="SAL")
SANST<-subset(STN,Actual.Eco.Site=="SAN")
FOREST<-subset(STN,Actual.Eco.Site=="FORE")

BHT<-BHST %>% 
  filter(GrowthHabitsub=="Tree")
BHS<-BHST %>%
  filter(GrowthHabitsub=="Shrub")
LBT<-LBST %>% 
  filter(GrowthHabitsub=="Tree")
LBS<-LBST %>%
  filter(GrowthHabitsub=="Shrub")
LOAT<-LOAST %>% 
  filter(GrowthHabitsub=="Tree")
LOAS<-LOAST %>%
  filter(GrowthHabitsub=="Shrub")
MOT<-MOST %>% 
  filter(GrowthHabitsub=="Tree")
MOS<-MOST %>%
  filter(GrowthHabitsub=="Shrub")
OTHT<-OTHST %>% 
  filter(GrowthHabitsub=="Tree")
OTHS<-OTHST %>%
  filter(GrowthHabitsub=="Shrub")
ROFT<-ROFST %>% 
  filter(GrowthHabitsub=="Tree")
ROFS<-ROFST %>%
  filter(GrowthHabitsub=="Shrub")
SALT<-SALST %>% 
  filter(GrowthHabitsub=="Tree")
SALS<-SALST %>%
  filter(GrowthHabitsub=="Shrub")
SANT<-SANST %>% 
  filter(GrowthHabitsub=="Tree")
SANS<-SANST %>%
  filter(GrowthHabitsub=="Shrub")
FORET<-FOREST %>% 
  filter(GrowthHabitsub=="Tree")
FORES<-FOREST %>%
  filter(GrowthHabitsub=="Shrub")

#BH
#replace your dominant species codes (determined in a previous step) in the below code
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","BOGR2","HECO26",
             "Dominant Herb. Cover (%)", "NA",
             "Dominant Woody Cover (%)","KRLA2","PIED",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

#replace with your dominant species plant codes. If only one plant code remove everything after digits=2) except the parantheses
#for example the code below would become domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
#if there is no dominant species for the growth form list "NA" as seen below in domherme<-"NA"
Species1<- subset(BH8,Species=="BOGR2")
Species2<-subset(BH8,Species=="HECO26")
Species3<-subset(BH8,Species=="KRLA2")
Species4<-subset(BH8,Species=="PIED")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species2$AH_SpeciesCover, na.rm=TRUE),digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/BHN2)),digits = 2) , round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/BHN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/BHN2)),digits=2),round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/BHN2)),digits=2))

Mean<-c("",round(mean(BH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(BH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(BH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(BH$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(BH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(BH$succulent, na.rm=TRUE),digits=2),
        round(mean(BH$tree, na.rm=TRUE),digits=2),round(mean(BH$noxiousforb, na.rm=TRUE),digits=2),round(mean(BH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(BH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(BH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(BHT$HeightWoody, na.rm=TRUE), digits=2),round(mean(BHS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(BHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(BHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(BHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(BHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(BHG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(BHSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(BH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(BH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(BH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(BH$shrub.subshrub, na.rm=TRUE), digits=2), round(min(BH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(BH$succulent, na.rm=TRUE),digits=2),
           round(min(BH$tree, na.rm=TRUE),digits=2),round(min(BH$noxiousforb, na.rm=TRUE),digits=2),round(min(BH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(BH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(BH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(BHT$HeightWoody, na.rm=TRUE), digits=2),round(min(BHS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(BHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(BHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(BHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(BHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(BHG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(BHSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(BH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(BH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(BH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(BH$shrub.subshrub, na.rm=TRUE), digits=2), round(max(BH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(BH$succulent, na.rm=TRUE),digits=2),
           round(max(BH$tree, na.rm=TRUE),digits=2),round(max(BH$noxiousforb, na.rm=TRUE),digits=2),round(max(BH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(BH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(BH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(BHT$HeightWoody, na.rm=TRUE), digits=2),round(max(BHS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(BHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(BHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(BHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(BHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(BHG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(BHSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(BH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(BH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(BH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(BH$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(BH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(BH$succulent, na.rm=TRUE),digits=2),
           round(sd(BH$tree, na.rm=TRUE),digits=2),round(sd(BH$noxiousforb, na.rm=TRUE),digits=2),round(sd(BH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(BH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(BH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(BHT$HeightWoody, na.rm=TRUE), digits=2),round(sd(BHS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(BHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(BHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(BHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(BHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(BHG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(BHSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(BH$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/BHN2),digits=2), round((1.282*sd(BH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BH$Bare.Soil.Pct, na.rm=TRUE)/BHN2),digits=2),
      "", round((1.282*sd(BH$shrub.subshrub, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BH$succulent, na.rm=TRUE)/BHN2),digits=2),
      round((1.282*sd(BH$tree, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BH$noxiousforb, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BH$nonnoxiousforb, na.rm=TRUE)/BHN2),digits=2),"",
      round((1.282*sd(BH$Average.Grass.Height.cm, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BH$Average.Forb.Height.cm, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BHT$HeightWoody, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BHS$HeightWoody, na.rm=TRUE)/BHN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(BHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/BHN2),digits=2),
      round((1.282*sd(BHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/BHN2),digits=2),  round((1.282*sd(BHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/BHN2),digits=2),
      round((1.282*sd(BHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/BHN2),digits=2),"", round((1.282*sd(BHG$CanopyCover, na.rm=TRUE)/BHN2),digits=2),"", round((1.282*sd(BHSR$SpeciesCount, na.rm=TRUE)/BHN2),digits=2))


BHdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
BHdataframe<-BHdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
BHsummtab<-formattable(BHdataframe, list(
  Indicator = blank_bold))
BHsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(BHsummtab,"BHsummtab.csv")

#LB
#replace with your dominant species in the quotes below
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","BOGR2","PASM",
             "Dominant Herb. Cover (%)", "SATR12",
             "Dominant Woody Cover (%)","KRLA2",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

#replace with your species code (see BH strata for more detail)
Species1<- subset(LB8,Species=="BOGR2")
Species2<-subset(LB8,Species=="PASM")
Species3<-subset(LB8,Species=="SATR12")
Species4<-subset(LB8,Species=="KRLA2")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-c(round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodme<-c(round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-c(round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodma<-c(round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-c(round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodmi<-c(round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodsd<-c(round(sd(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/LBN2)),digits = 2) , round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/LBN2)),digits=2))
domherbCI<-c(round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/LBN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/LBN2)),digits=2))

Mean<-c("",round(mean(LB$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(LB$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(LB$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(LB$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(LB$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(LB$succulent, na.rm=TRUE),digits=2),
        round(mean(LB$tree, na.rm=TRUE),digits=2),round(mean(LB$noxiousforb, na.rm=TRUE),digits=2),round(mean(LB$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(LB$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(LB$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(LBT$HeightWoody, na.rm=TRUE), digits=2),round(mean(LBS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(LBG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(LBG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(LBG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(LBG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(LBG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(LBSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(LB$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(LB$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(LB$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(LB$shrub.subshrub, na.rm=TRUE), digits=2), round(min(LB$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(LB$succulent, na.rm=TRUE),digits=2),
           round(min(LB$tree, na.rm=TRUE),digits=2),round(min(LB$noxiousforb, na.rm=TRUE),digits=2),round(min(LB$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(LB$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(LB$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(LBT$HeightWoody, na.rm=TRUE), digits=2),round(min(LBS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(LBG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(LBG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(LBG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(LBG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(LBG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(LBSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(LB$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(LB$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(LB$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(LB$shrub.subshrub, na.rm=TRUE), digits=2), round(max(LB$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(LB$succulent, na.rm=TRUE),digits=2),
           round(max(LB$tree, na.rm=TRUE),digits=2),round(max(LB$noxiousforb, na.rm=TRUE),digits=2),round(max(LB$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(LB$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(LB$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(LBT$HeightWoody, na.rm=TRUE), digits=2),round(max(LBS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(LBG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(LBG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(LBG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(LBG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(LBG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(LBSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(LB$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(LB$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(LB$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(LB$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(LB$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(LB$succulent, na.rm=TRUE),digits=2),
           round(sd(LB$tree, na.rm=TRUE),digits=2),round(sd(LB$noxiousforb, na.rm=TRUE),digits=2),round(sd(LB$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(LB$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(LB$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(LBT$HeightWoody, na.rm=TRUE), digits=2),round(sd(LBS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(LBG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(LBG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(LBG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(LBG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(LBG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(LBSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(LB$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/LBN2),digits=2), round((1.282*sd(LB$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LB$Bare.Soil.Pct, na.rm=TRUE)/LBN2),digits=2),
      "", round((1.282*sd(LB$shrub.subshrub, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LB$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LB$succulent, na.rm=TRUE)/LBN2),digits=2),
      round((1.282*sd(LB$tree, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LB$noxiousforb, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LB$nonnoxiousforb, na.rm=TRUE)/LBN2),digits=2),"",
      round((1.282*sd(LB$Average.Grass.Height.cm, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LB$Average.Forb.Height.cm, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LBT$HeightWoody, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LBS$HeightWoody, na.rm=TRUE)/LBN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(LBG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/LBN2),digits=2),
      round((1.282*sd(LBG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/LBN2),digits=2),  round((1.282*sd(LBG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/LBN2),digits=2),
      round((1.282*sd(LBG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/LBN2),digits=2),"", round((1.282*sd(LBG$CanopyCover, na.rm=TRUE)/LBN2),digits=2),"", round((1.282*sd(LBSR$SpeciesCount, na.rm=TRUE)/LBN2),digits=2))


LBdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
LBdataframe<-LBdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))

target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
LBsummtab<-formattable(LBdataframe, list(
  Indicator = blank_bold))
LBsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(LBsummtab,"LBsummtab.csv")

#LOA
#replace with your species code in the quotes below
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","BOGR2",
             "Dominant Herb. Cover (%)", "MUMO","MUTO2",
             "Dominant Woody Cover (%)","PIED",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

#replace with your species code (see BH strata for more detail)
Species1<- subset(LOA8,Species=="BOGR2")
Species2<-subset(LOA8,Species=="MUMO")
Species3<-subset(LOA8,Species=="MUTO2")
Species4<-subset(LOA8,Species=="PIED")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-c(round(mean(Species2$AH_SpeciesCover, na.rm=TRUE),digits=2),round(mean(Species3$AH_SpeciesCover, na.rm=TRUE),digits=2))
domwoodme<-c(round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-c(round(max(Species2$AH_SpeciesCover, na.rm=TRUE),digits=2),round(max(Species3$AH_SpeciesCover, na.rm=TRUE),digits=2))
domwoodma<-c(round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-c(round(min(Species2$AH_SpeciesCover, na.rm=TRUE),digits=2),round(min(Species3$AH_SpeciesCover, na.rm=TRUE),digits=2))
domwoodmi<-c(round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(Species2$AH_SpeciesCover, na.rm=TRUE),digits=2),round(sd(Species3$AH_SpeciesCover, na.rm=TRUE),digits=2))
domwoodsd<-c(round(sd(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/LOAN2)),digits = 2))
domherbCI<-c(round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/LOAN2)),digits=2),round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/LOAN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/LOAN2)),digits=2))

Mean<-c("",round(mean(LOA$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(LOA$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(LOA$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(LOA$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(LOA$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(LOA$succulent, na.rm=TRUE),digits=2),
        round(mean(LOA$tree, na.rm=TRUE),digits=2),round(mean(LOA$noxiousforb, na.rm=TRUE),digits=2),round(mean(LOA$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(LOA$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(LOA$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(LOAT$HeightWoody, na.rm=TRUE), digits=2),round(mean(LOAS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(LOAG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(LOAG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(LOAG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(LOAG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(LOAG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(LOASR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(LOA$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(LOA$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(LOA$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(LOA$shrub.subshrub, na.rm=TRUE), digits=2), round(min(LOA$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(LOA$succulent, na.rm=TRUE),digits=2),
           round(min(LOA$tree, na.rm=TRUE),digits=2),round(min(LOA$noxiousforb, na.rm=TRUE),digits=2),round(min(LOA$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(LOA$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(LOA$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(LOAT$HeightWoody, na.rm=TRUE), digits=2),round(min(LOAS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(LOAG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(LOAG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(LOAG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(LOAG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(LOAG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(LOASR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(LOA$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(LOA$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(LOA$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(LOA$shrub.subshrub, na.rm=TRUE), digits=2), round(max(LOA$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(LOA$succulent, na.rm=TRUE),digits=2),
           round(max(LOA$tree, na.rm=TRUE),digits=2),round(max(LOA$noxiousforb, na.rm=TRUE),digits=2),round(max(LOA$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(LOA$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(LOA$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(LOAT$HeightWoody, na.rm=TRUE), digits=2),round(max(LOAS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(LOAG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(LOAG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(LOAG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(LOAG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(LOAG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(LOASR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(LOA$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(LOA$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(LOA$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(LOA$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(LOA$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(LOA$succulent, na.rm=TRUE),digits=2),
           round(sd(LOA$tree, na.rm=TRUE),digits=2),round(sd(LOA$noxiousforb, na.rm=TRUE),digits=2),round(sd(LOA$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(LOA$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(LOA$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(LOAT$HeightWoody, na.rm=TRUE), digits=2),round(sd(LOAS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(LOAG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(LOAG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(LOAG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(LOAG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(LOAG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(LOASR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(LOA$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/LOAN2),digits=2), round((1.282*sd(LOA$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOA$Bare.Soil.Pct, na.rm=TRUE)/LOAN2),digits=2),
      "", round((1.282*sd(LOA$shrub.subshrub, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOA$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOA$succulent, na.rm=TRUE)/LOAN2),digits=2),
      round((1.282*sd(LOA$tree, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOA$noxiousforb, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOA$nonnoxiousforb, na.rm=TRUE)/LOAN2),digits=2),"",
      round((1.282*sd(LOA$Average.Grass.Height.cm, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOA$Average.Forb.Height.cm, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOAT$HeightWoody, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOAS$HeightWoody, na.rm=TRUE)/LOAN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(LOAG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/LOAN2),digits=2),
      round((1.282*sd(LOAG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/LOAN2),digits=2),  round((1.282*sd(LOAG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/LOAN2),digits=2),
      round((1.282*sd(LOAG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/LOAN2),digits=2),"", round((1.282*sd(LOAG$CanopyCover, na.rm=TRUE)/LOAN2),digits=2),"", round((1.282*sd(LOASR$SpeciesCount, na.rm=TRUE)/LOAN2),digits=2))


LOAdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
LOAdataframe<-LOAdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))

target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
LOAsummtab<-formattable(LOAdataframe, list(
  Indicator = blank_bold))
LOAsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(LOAsummtab,"LOAsummtab.csv")

#MO
#see BH strata for more detail on replacement
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","BOGR2","HECO26","PASM",
             "Dominant Herb. Cover (%)", "NA",
             "Dominant Woody Cover (%)","CHGR6",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

Species1<- subset(MO8,Species=="BOGR2")
Species2<-subset(MO8,Species=="HECO26")
Species3<-subset(MO8,Species=="PASM")
Species4<-subset(MO8,Species=="CHGR6")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/MON2)),digits = 2) , round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/MON2)),digits=2), round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/MON2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/MON2)),digits=2))

Mean<-c("",round(mean(MO$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(MO$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(MO$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(MO$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(MO$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(MO$succulent, na.rm=TRUE),digits=2),
        round(mean(MO$tree, na.rm=TRUE),digits=2),round(mean(MO$noxiousforb, na.rm=TRUE),digits=2),round(mean(MO$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(MO$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(MO$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(MOT$HeightWoody, na.rm=TRUE), digits=2),round(mean(MOS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(MOG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(MOG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(MOG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(MOG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(MOG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(MOSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(MO$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(MO$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(MO$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(MO$shrub.subshrub, na.rm=TRUE), digits=2), round(min(MO$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(MO$succulent, na.rm=TRUE),digits=2),
           round(min(MO$tree, na.rm=TRUE),digits=2),round(min(MO$noxiousforb, na.rm=TRUE),digits=2),round(min(MO$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(MO$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(MO$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(MOT$HeightWoody, na.rm=TRUE), digits=2),round(min(MOS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(MOG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(MOG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(MOG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(MOG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(MOG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(MOSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(MO$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(MO$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(MO$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(MO$shrub.subshrub, na.rm=TRUE), digits=2), round(max(MO$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(MO$succulent, na.rm=TRUE),digits=2),
           round(max(MO$tree, na.rm=TRUE),digits=2),round(max(MO$noxiousforb, na.rm=TRUE),digits=2),round(max(MO$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(MO$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(MO$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(MOT$HeightWoody, na.rm=TRUE), digits=2),round(max(MOS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(MOG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(MOG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(MOG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(MOG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(MOG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(MOSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(MO$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(MO$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(MO$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(MO$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(MO$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(MO$succulent, na.rm=TRUE),digits=2),
           round(sd(MO$tree, na.rm=TRUE),digits=2),round(sd(MO$noxiousforb, na.rm=TRUE),digits=2),round(sd(MO$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(MO$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(MO$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(MOT$HeightWoody, na.rm=TRUE), digits=2),round(sd(MOS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(MOG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(MOG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(MOG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(MOG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(MOG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(MOSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(MO$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/MON2),digits=2), round((1.282*sd(MO$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MO$Bare.Soil.Pct, na.rm=TRUE)/MON2),digits=2),
      "", round((1.282*sd(MO$shrub.subshrub, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MO$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MO$succulent, na.rm=TRUE)/MON2),digits=2),
      round((1.282*sd(MO$tree, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MO$noxiousforb, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MO$nonnoxiousforb, na.rm=TRUE)/MON2),digits=2),"",
      round((1.282*sd(MO$Average.Grass.Height.cm, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MO$Average.Forb.Height.cm, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MOT$HeightWoody, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MOS$HeightWoody, na.rm=TRUE)/MON2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(MOG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/MON2),digits=2),
      round((1.282*sd(MOG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/MON2),digits=2),  round((1.282*sd(MOG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/MON2),digits=2),
      round((1.282*sd(MOG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/MON2),digits=2),"", round((1.282*sd(MOG$CanopyCover, na.rm=TRUE)/MON2),digits=2),"", round((1.282*sd(MOSR$SpeciesCount, na.rm=TRUE)/MON2),digits=2))


MOdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
MOdataframe<-MOdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))

target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
MOsummtab<-formattable(MOdataframe, list(
  Indicator = blank_bold))
MOsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(MOsummtab,"MOsummtab.csv")

#OTH
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","HECO26","PIMI",
             "Dominant Herb. Cover (%)", "MUMO",
             "Dominant Woody Cover (%)","PIED",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

Species1<- subset(OTH8,Species=="HECO26")
Species2<-subset(OTH8,Species=="PIMI")
Species3<-subset(OTH8,Species=="MUMO")
Species4<-subset(OTH8,Species=="PIED")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover), digits=2),round(mean(Species2$AH_SpeciesCover), digits=2))
domherbme<-c(round(mean(Species3$AH_SpeciesCover), digits=2))
domwoodme<-c(round(mean(Species4$AH_SpeciesCover), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover), digits=2),round(max(Species2$AH_SpeciesCover), digits=2))
domherbma<-c(round(max(Species3$AH_SpeciesCover), digits=2))
domwoodma<-c(round(max(Species4$AH_SpeciesCover), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover), digits=2),round(min(Species2$AH_SpeciesCover), digits=2))
domherbmi<-c(round(min(Species3$AH_SpeciesCover), digits=2))
domwoodmi<-c(round(min(Species4$AH_SpeciesCover), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover), digits=2),round(sd(Species2$AH_SpeciesCover), digits=2))
domherbsd<-c(round(sd(Species3$AH_SpeciesCover), digits=2))
domwoodsd<-c(round(sd(Species4$AH_SpeciesCover), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover)/OTHN2)),digits = 2),round((1.282*(sd(Species2$AH_SpeciesCover)/OTHN2)),digits = 2))
domherbCI<-c(round((1.282*(sd(Species3$AH_SpeciesCover)/OTHN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(Species4$AH_SpeciesCover)/OTHN2)),digits=2))

Mean<-c("",round(mean(OTH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(OTH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(OTH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(OTH$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(OTH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(OTH$succulent, na.rm=TRUE),digits=2),
        round(mean(OTH$tree, na.rm=TRUE),digits=2),round(mean(OTH$noxiousforb, na.rm=TRUE),digits=2),round(mean(OTH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(OTH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(OTH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(OTHT$HeightWoody, na.rm=TRUE), digits=2),round(mean(OTHS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(OTHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(OTHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(OTHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(OTHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(OTHG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(OTHSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(OTH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(OTH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(OTH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(OTH$shrub.subshrub, na.rm=TRUE), digits=2), round(min(OTH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(OTH$succulent, na.rm=TRUE),digits=2),
           round(min(OTH$tree, na.rm=TRUE),digits=2),round(min(OTH$noxiousforb, na.rm=TRUE),digits=2),round(min(OTH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(OTH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(OTH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(OTHT$HeightWoody, na.rm=TRUE), digits=2),round(min(OTHS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(OTHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(OTHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(OTHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(OTHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(OTHG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(OTHSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(OTH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(OTH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(OTH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(OTH$shrub.subshrub, na.rm=TRUE), digits=2), round(max(OTH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(OTH$succulent, na.rm=TRUE),digits=2),
           round(max(OTH$tree, na.rm=TRUE),digits=2),round(max(OTH$noxiousforb, na.rm=TRUE),digits=2),round(max(OTH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(OTH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(OTH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(OTHT$HeightWoody, na.rm=TRUE), digits=2),round(max(OTHS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(OTHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(OTHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(OTHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(OTHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(OTHG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(OTHSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(OTH$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(OTH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(OTH$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(OTH$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(OTH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(OTH$succulent, na.rm=TRUE),digits=2),
           round(sd(OTH$tree, na.rm=TRUE),digits=2),round(sd(OTH$noxiousforb, na.rm=TRUE),digits=2),round(sd(OTH$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(OTH$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(OTH$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(OTHT$HeightWoody, na.rm=TRUE), digits=2),round(sd(OTHS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(OTHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(OTHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(OTHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(OTHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(OTHG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(OTHSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(OTH$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/OTHN2),digits=2), round((1.282*sd(OTH$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTH$Bare.Soil.Pct, na.rm=TRUE)/OTHN2),digits=2),
      "", round((1.282*sd(OTH$shrub.subshrub, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTH$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTH$succulent, na.rm=TRUE)/OTHN2),digits=2),
      round((1.282*sd(OTH$tree, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTH$noxiousforb, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTH$nonnoxiousforb, na.rm=TRUE)/OTHN2),digits=2),"",
      round((1.282*sd(OTH$Average.Grass.Height.cm, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTH$Average.Forb.Height.cm, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTHT$HeightWoody, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTHS$HeightWoody, na.rm=TRUE)/OTHN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(OTHG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/OTHN2),digits=2),
      round((1.282*sd(OTHG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/OTHN2),digits=2),  round((1.282*sd(OTHG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/OTHN2),digits=2),
      round((1.282*sd(OTHG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/OTHN2),digits=2),"", round((1.282*sd(OTHG$CanopyCover, na.rm=TRUE)/OTHN2),digits=2),"", round((1.282*sd(OTHSR$SpeciesCount, na.rm=TRUE)/OTHN2),digits=2))


OTHdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
OTHdataframe<-OTHdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
OTHsummtab<-formattable(OTHdataframe, list(
  Indicator = blank_bold))
OTHsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(OTHsummtab,"OTHsummtab.csv")

#ROF
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","HECO26","BOGR2",
             "Dominant Herb. Cover (%)", "NA",
             "Dominant Woody Cover (%)","PIED","JUMO",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

Species1<- subset(ROF8,Species=="HECO26")
Species2<-subset(ROF8,Species=="BOGR2")
Species3<-subset(ROF8,Species=="PIED")
Species4<-subset(ROF8,Species=="JUMO")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/ROFN2)),digits = 2),round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/ROFN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/ROFN2)),digits=2),round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/ROFN2)),digits=2))

Mean<-c("",round(mean(ROF$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(ROF$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(ROF$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(ROF$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(ROF$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(ROF$succulent, na.rm=TRUE),digits=2),
        round(mean(ROF$tree, na.rm=TRUE),digits=2),round(mean(ROF$noxiousforb, na.rm=TRUE),digits=2),round(mean(ROF$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(ROF$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(ROF$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(ROFT$HeightWoody, na.rm=TRUE), digits=2),round(mean(ROFS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(ROFG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(ROFG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(ROFG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(ROFG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(ROFG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(ROFSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(ROF$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(ROF$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(ROF$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(ROF$shrub.subshrub, na.rm=TRUE), digits=2), round(min(ROF$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(ROF$succulent, na.rm=TRUE),digits=2),
           round(min(ROF$tree, na.rm=TRUE),digits=2),round(min(ROF$noxiousforb, na.rm=TRUE),digits=2),round(min(ROF$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(ROF$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(ROF$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(ROFT$HeightWoody, na.rm=TRUE), digits=2),round(min(ROFS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(ROFG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(ROFG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(ROFG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(ROFG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(ROFG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(ROFSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(ROF$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(ROF$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(ROF$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(ROF$shrub.subshrub, na.rm=TRUE), digits=2), round(max(ROF$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(ROF$succulent, na.rm=TRUE),digits=2),
           round(max(ROF$tree, na.rm=TRUE),digits=2),round(max(ROF$noxiousforb, na.rm=TRUE),digits=2),round(max(ROF$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(ROF$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(ROF$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(ROFT$HeightWoody, na.rm=TRUE), digits=2),round(max(ROFS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(ROFG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(ROFG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(ROFG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(ROFG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(ROFG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(ROFSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(ROF$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(ROF$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(ROF$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(ROF$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(ROF$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(ROF$succulent, na.rm=TRUE),digits=2),
           round(sd(ROF$tree, na.rm=TRUE),digits=2),round(sd(ROF$noxiousforb, na.rm=TRUE),digits=2),round(sd(ROF$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(ROF$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(ROF$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(ROFT$HeightWoody, na.rm=TRUE), digits=2),round(sd(ROFS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(ROFG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(ROFG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(ROFG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(ROFG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(ROFG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(ROFSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(ROF$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/ROFN2),digits=2), round((1.282*sd(ROF$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROF$Bare.Soil.Pct, na.rm=TRUE)/ROFN2),digits=2),
      "", round((1.282*sd(ROF$shrub.subshrub, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROF$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROF$succulent, na.rm=TRUE)/ROFN2),digits=2),
      round((1.282*sd(ROF$tree, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROF$noxiousforb, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROF$nonnoxiousforb, na.rm=TRUE)/ROFN2),digits=2),"",
      round((1.282*sd(ROF$Average.Grass.Height.cm, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROF$Average.Forb.Height.cm, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROFT$HeightWoody, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROFS$HeightWoody, na.rm=TRUE)/ROFN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(ROFG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/ROFN2),digits=2),
      round((1.282*sd(ROFG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/ROFN2),digits=2),  round((1.282*sd(ROFG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/ROFN2),digits=2),
      round((1.282*sd(ROFG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/ROFN2),digits=2),"", round((1.282*sd(ROFG$CanopyCover, na.rm=TRUE)/ROFN2),digits=2),"", round((1.282*sd(ROFSR$SpeciesCount, na.rm=TRUE)/ROFN2),digits=2))


ROFdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
ROFdataframe<-ROFdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
ROFsummtab<-formattable(ROFdataframe, list(
  Indicator = blank_bold))
ROFsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(ROFsummtab,"ROFsummtab.csv")

#SAL
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","DISP",
             "Dominant Herb. Cover (%)", "ERCE2",
             "Dominant Woody Cover (%)","SAVE4",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")
Species1<- subset(SAL8,Species=="DISP")
Species2<-subset(SAL8,Species=="ERCE2")
Species3<-subset(SAL8,Species=="SAVE4")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-c(round(mean(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodme<-c(round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-c(round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodma<-c(round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-c(round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodmi<-c(round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2))
domwoodsd<-c(round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/SALN2)),digits = 2)) 
domherbCI<-c(round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/SALN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/SALN2)),digits=2))

Mean<-c("",round(mean(SAL$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(SAL$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(SAL$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(SAL$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(SAL$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(SAL$succulent, na.rm=TRUE),digits=2),
        round(mean(SAL$tree, na.rm=TRUE),digits=2),round(mean(SAL$noxiousforb, na.rm=TRUE),digits=2),round(mean(SAL$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(SAL$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(SAL$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(SALT$HeightWoody, na.rm=TRUE), digits=2),round(mean(SALS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(SALG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(SALG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(SALG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(SALG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(SALG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(SALSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(SAL$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(SAL$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(SAL$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(SAL$shrub.subshrub, na.rm=TRUE), digits=2), round(min(SAL$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(SAL$succulent, na.rm=TRUE),digits=2),
           round(min(SAL$tree, na.rm=TRUE),digits=2),round(min(SAL$noxiousforb, na.rm=TRUE),digits=2),round(min(SAL$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(SAL$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(SAL$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(SALT$HeightWoody, na.rm=TRUE), digits=2),round(min(SALS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(SALG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(SALG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(SALG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(SALG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(SALG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(SALSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(SAL$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(SAL$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(SAL$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(SAL$shrub.subshrub, na.rm=TRUE), digits=2), round(max(SAL$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(SAL$succulent, na.rm=TRUE),digits=2),
           round(max(SAL$tree, na.rm=TRUE),digits=2),round(max(SAL$noxiousforb, na.rm=TRUE),digits=2),round(max(SAL$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(SAL$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(SAL$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(SALT$HeightWoody, na.rm=TRUE), digits=2),round(max(SALS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(SALG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(SALG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(SALG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(SALG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(SALG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(SALSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(SAL$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(SAL$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(SAL$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(SAL$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(SAL$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(SAL$succulent, na.rm=TRUE),digits=2),
           round(sd(SAL$tree, na.rm=TRUE),digits=2),round(sd(SAL$noxiousforb, na.rm=TRUE),digits=2),round(sd(SAL$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(SAL$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(SAL$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(SALT$HeightWoody, na.rm=TRUE), digits=2),round(sd(SALS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(SALG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(SALG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(SALG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(SALG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(SALG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(SALSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(SAL$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/SALN2),digits=2), round((1.282*sd(SAL$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SAL$Bare.Soil.Pct, na.rm=TRUE)/SALN2),digits=2),
      "", round((1.282*sd(SAL$shrub.subshrub, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SAL$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SAL$succulent, na.rm=TRUE)/SALN2),digits=2),
      round((1.282*sd(SAL$tree, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SAL$noxiousforb, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SAL$nonnoxiousforb, na.rm=TRUE)/SALN2),digits=2),"",
      round((1.282*sd(SAL$Average.Grass.Height.cm, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SAL$Average.Forb.Height.cm, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SALT$HeightWoody, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SALS$HeightWoody, na.rm=TRUE)/SALN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(SALG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/SALN2),digits=2),
      round((1.282*sd(SALG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/SALN2),digits=2),  round((1.282*sd(SALG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/SALN2),digits=2),
      round((1.282*sd(SALG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/SALN2),digits=2),"", round((1.282*sd(SALG$CanopyCover, na.rm=TRUE)/SALN2),digits=2),"", round((1.282*sd(SALSR$SpeciesCount, na.rm=TRUE)/SALN2),digits=2))


SALdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
SALdataframe<-SALdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
SALsummtab<-formattable(SALdataframe, list(
  Indicator = blank_bold))
SALsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(SALsummtab,"SALsummtab.csv")

#SAN
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","BOGR2","DISP","ACHY","SPCR",
             "Dominant Herb. Cover (%)", "NA",
             "Dominant Woody Cover (%)","NA",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

Species1<- subset(SAN8,Species=="BOGR2")
Species2<-subset(SAN8,Species=="DISP")
Species3<-subset(SAN8,Species=="ACHY")
Species4<-subset(SAN8,Species=="SPCR")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-"NA"

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-"NA"

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-"NA"

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-"NA"

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/SANN2)),digits = 2) , round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/SANN2)),digits=2), round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/SANN2)),digits=2), round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/SANN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-"NA"

Mean<-c("",round(mean(SAN$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(SAN$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(SAN$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(SAN$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(SAN$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(SAN$succulent, na.rm=TRUE),digits=2),
        round(mean(SAN$tree, na.rm=TRUE),digits=2),round(mean(SAN$noxiousforb, na.rm=TRUE),digits=2),round(mean(SAN$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(SAN$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(SAN$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(SANT$HeightWoody, na.rm=TRUE), digits=2),round(mean(SANS$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(SANG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(SANG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(SANG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(SANG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(SANG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(SANSR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(SAN$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(SAN$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(SAN$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(SAN$shrub.subshrub, na.rm=TRUE), digits=2), round(min(SAN$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(SAN$succulent, na.rm=TRUE),digits=2),
           round(min(SAN$tree, na.rm=TRUE),digits=2),round(min(SAN$noxiousforb, na.rm=TRUE),digits=2),round(min(SAN$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(SAN$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(SAN$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(SANT$HeightWoody, na.rm=TRUE), digits=2),round(min(SANS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(SANG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(SANG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(SANG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(SANG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(SANG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(SANSR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(SAN$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(SAN$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(SAN$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(SAN$shrub.subshrub, na.rm=TRUE), digits=2), round(max(SAN$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(SAN$succulent, na.rm=TRUE),digits=2),
           round(max(SAN$tree, na.rm=TRUE),digits=2),round(max(SAN$noxiousforb, na.rm=TRUE),digits=2),round(max(SAN$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(SAN$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(SAN$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(SANT$HeightWoody, na.rm=TRUE), digits=2),round(max(SANS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(SANG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(SANG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(SANG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(SANG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(SANG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(SANSR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(SAN$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(SAN$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(SAN$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(SAN$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(SAN$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(SAN$succulent, na.rm=TRUE),digits=2),
           round(sd(SAN$tree, na.rm=TRUE),digits=2),round(sd(SAN$noxiousforb, na.rm=TRUE),digits=2),round(sd(SAN$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(SAN$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(SAN$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(SANT$HeightWoody, na.rm=TRUE), digits=2),round(sd(SANS$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(SANG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(SANG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(SANG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(SANG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(SANG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(SANSR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(SAN$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/SANN2),digits=2), round((1.282*sd(SAN$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SAN$Bare.Soil.Pct, na.rm=TRUE)/SANN2),digits=2),
      "", round((1.282*sd(SAN$shrub.subshrub, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SAN$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SAN$succulent, na.rm=TRUE)/SANN2),digits=2),
      round((1.282*sd(SAN$tree, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SAN$noxiousforb, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SAN$nonnoxiousforb, na.rm=TRUE)/SANN2),digits=2),"",
      round((1.282*sd(SAN$Average.Grass.Height.cm, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SAN$Average.Forb.Height.cm, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SANT$HeightWoody, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SANS$HeightWoody, na.rm=TRUE)/SANN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(SANG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/SANN2),digits=2),
      round((1.282*sd(SANG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/SANN2),digits=2),  round((1.282*sd(SANG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/SANN2),digits=2),
      round((1.282*sd(SANG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/SANN2),digits=2),"", round((1.282*sd(SANG$CanopyCover, na.rm=TRUE)/SANN2),digits=2),"", round((1.282*sd(SANSR$SpeciesCount, na.rm=TRUE)/SANN2),digits=2))


SANdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
SANdataframe<-SANdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
SANsummtab<-formattable(SANdataframe, list(
  Indicator = blank_bold))
SANsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(SANsummtab,"SANsummtab.csv")

#FORE
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","FEAR2",
             "Dominant Herb. Cover (%)", "NA",
             "Dominant Woody Cover (%)","PIFL2","PIED","PSME",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

Species1<- subset(FORE8,Species=="FEAR2")
Species2<-subset(FORE8,Species=="PIFL2")
Species3<-subset(FORE8,Species=="PIED")
Species4<-subset(FORE8,Species=="PSME")
domgrassme<-c(round(mean(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(mean(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassma<-c(round(max(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(max(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(min(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(Species1$AH_SpeciesCover, na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(Species2$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species3$AH_SpeciesCover, na.rm=TRUE), digits=2),round(sd(Species4$AH_SpeciesCover, na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(Species1$AH_SpeciesCover, na.rm=TRUE)/FOREN2)),digits = 2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(Species2$AH_SpeciesCover, na.rm=TRUE)/FOREN2)),digits=2),round((1.282*(sd(Species3$AH_SpeciesCover, na.rm=TRUE)/FOREN2)),digits=2),round((1.282*(sd(Species4$AH_SpeciesCover, na.rm=TRUE)/FOREN2)),digits=2))

Mean<-c("",round(mean(FORE$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(FORE$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(mean(FORE$Bare.Soil.Pct, na.rm=TRUE), digits=2),
        "",round(mean(FORE$shrub.subshrub, na.rm=TRUE), digits=2), round(mean(FORE$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(mean(FORE$succulent, na.rm=TRUE),digits=2),
        round(mean(FORE$tree, na.rm=TRUE),digits=2),round(mean(FORE$noxiousforb, na.rm=TRUE),digits=2),round(mean(FORE$nonnoxiousforb, na.rm=TRUE),digits=2),"",
        round(mean(FORE$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(mean(FORE$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(mean(FORET$HeightWoody, na.rm=TRUE), digits=2),round(mean(FORES$HeightWoody, na.rm=TRUE), digits=2),"",
        domgrassme,"",domherbme,"",
        domwoodme,"",round(mean(FOREG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
        round(mean(FOREG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(mean(FOREG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
        round(mean(FOREG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(mean(FOREG$CanopyCover, na.rm=TRUE), digits=2),"",round(mean(FORESR$SpeciesCount, na.rm=TRUE), digits=2))

Minimum<-c("",round(min(FORE$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(FORE$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(min(FORE$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(min(FORE$shrub.subshrub, na.rm=TRUE), digits=2), round(min(FORE$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(min(FORE$succulent, na.rm=TRUE),digits=2),
           round(min(FORE$tree, na.rm=TRUE),digits=2),round(min(FORE$noxiousforb, na.rm=TRUE),digits=2),round(min(FORE$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(min(FORE$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(min(FORE$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(min(FORET$HeightWoody, na.rm=TRUE), digits=2),round(min(FORES$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassmi,"",domherbmi,"",
           domwoodmi,"",round(min(FOREG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(min(FOREG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(min(FOREG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(min(FOREG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(min(FOREG$CanopyCover, na.rm=TRUE), digits=2),"",round(min(FORESR$SpeciesCount, na.rm=TRUE), digits=2))

Maximum<-c("",round(max(FORE$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(FORE$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(max(FORE$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(max(FORE$shrub.subshrub, na.rm=TRUE), digits=2), round(max(FORE$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(max(FORE$succulent, na.rm=TRUE),digits=2),
           round(max(FORE$tree, na.rm=TRUE),digits=2),round(max(FORE$noxiousforb, na.rm=TRUE),digits=2),round(max(FORE$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(max(FORE$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(max(FORE$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(max(FORET$HeightWoody, na.rm=TRUE), digits=2),round(max(FORES$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrassma,"",domherbma,"",
           domwoodma,"",round(max(FOREG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(max(FOREG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(max(FOREG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(max(FOREG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(max(FOREG$CanopyCover, na.rm=TRUE), digits=2),"",round(max(FORESR$SpeciesCount, na.rm=TRUE), digits=2))

St.Dev.<-c("",round(sd(FORE$Rock.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(FORE$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE), digits=2),round(sd(FORE$Bare.Soil.Pct, na.rm=TRUE), digits=2),
           "",round(sd(FORE$shrub.subshrub, na.rm=TRUE), digits=2), round(sd(FORE$Grass.Cover.Pct.Any.Hit, na.rm=TRUE),digits=2),round(sd(FORE$succulent, na.rm=TRUE),digits=2),
           round(sd(FORE$tree, na.rm=TRUE),digits=2),round(sd(FORE$noxiousforb, na.rm=TRUE),digits=2),round(sd(FORE$nonnoxiousforb, na.rm=TRUE),digits=2),"",
           round(sd(FORE$Average.Grass.Height.cm, na.rm=TRUE),digits=2),round(sd(FORE$Average.Forb.Height.cm, na.rm=TRUE), digits=2),round(sd(FORET$HeightWoody, na.rm=TRUE), digits=2),round(sd(FORES$HeightWoody, na.rm=TRUE), digits=2),"",
           domgrasssd,"",domherbsd,"",
           domwoodsd,"",round(sd(FOREG$Gap.Cover.25.to.50.Pct, na.rm=TRUE), digits=2),
           round(sd(FOREG$Gap.Cover.51.to.100.Pct, na.rm=TRUE), digits=2),round(sd(FOREG$Gap.Cover.101.to.200.Pct, na.rm=TRUE), digits=2),
           round(sd(FOREG$Gap.Cover.200.Plus.Pct, na.rm=TRUE), digits=2),"",round(sd(FOREG$CanopyCover, na.rm=TRUE), digits=2),"",round(sd(FORESR$SpeciesCount, na.rm=TRUE), digits=2))

ME<-c("", round((1.282*sd(FORE$Rock.Cover.Pct.First.Hit, na.rm=TRUE)/FOREN2),digits=2), round((1.282*sd(FORE$Total.Litter.Cover.Pct.First.Hit, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORE$Bare.Soil.Pct, na.rm=TRUE)/FOREN2),digits=2),
      "", round((1.282*sd(FORE$shrub.subshrub, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORE$Grass.Cover.Pct.Any.Hit, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORE$succulent, na.rm=TRUE)/FOREN2),digits=2),
      round((1.282*sd(FORE$tree, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORE$noxiousforb, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORE$nonnoxiousforb, na.rm=TRUE)/FOREN2),digits=2),"",
      round((1.282*sd(FORE$Average.Grass.Height.cm, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORE$Average.Forb.Height.cm, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORET$HeightWoody, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FORES$HeightWoody, na.rm=TRUE)/FOREN2),digits=2),"",
      domgrassCI,"",domherbCI,"",
      domwoodCI,"", round((1.282*sd(FOREG$Gap.Cover.25.to.50.Pct, na.rm=TRUE)/FOREN2),digits=2),
      round((1.282*sd(FOREG$Gap.Cover.51.to.100.Pct, na.rm=TRUE)/FOREN2),digits=2),  round((1.282*sd(FOREG$Gap.Cover.101.to.200.Pct, na.rm=TRUE)/FOREN2),digits=2),
      round((1.282*sd(FOREG$Gap.Cover.200.Plus.Pct, na.rm=TRUE)/FOREN2),digits=2),"", round((1.282*sd(FOREG$CanopyCover, na.rm=TRUE)/FOREN2),digits=2),"", round((1.282*sd(FORESR$SpeciesCount, na.rm=TRUE)/FOREN2),digits=2))


FOREdataframe<-data.frame(Indicator,Mean,Minimum,Maximum,St.Dev.,ME)
FOREdataframe<-FOREdataframe %>% mutate_if(is.factor, list(~na_if(., Inf))) %>% 
  mutate_if(is.factor, list(~na_if(., -Inf)))%>% 
  mutate_if(is.factor, list(~na_if(., "NaN")))
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
FOREsummtab<-formattable(FOREdataframe, list(
  Indicator = blank_bold))
FOREsummtab
#run if table is too large to view in export, this will send it to excel
write.csv(FOREsummtab,"FOREsummtab.csv")
