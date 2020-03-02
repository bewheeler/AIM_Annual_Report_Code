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
strata<-read.csv("~/SLVFOstrata_assignment1.csv")
strata$Actual.Eco.Site<-AIMdata$Actual.Eco.Site[match(strata$PrimaryKey,AIMdata$Primary.Key)]

#stratum description, general using TerrADat form "Plots"
head(Plotdata$Actual.Eco.Site)

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

summary(LB$AvgPrecip)
summary(LB$Slope)
summary(LB$Elevation)
summary(LB$EcolSite)
summary(LB$LandscapeType)

summary(LOA$AvgPrecip)
summary(LOA$Slope)
summary(LOA$Elevation)
summary(LOA$EcolSite)
summary(LOA$LandscapeType)

summary(MO$AvgPrecip)
summary(MO$Slope)
summary(MO$Elevation)
summary(MO$EcolSite)
summary(MO$LandscapeType)

summary(OTH$AvgPrecip)
summary(OTH$Slope)
summary(OTH$Elevation)
summary(OTH$EcolSite)
summary(OTH$LandscapeType)

summary(ROF$AvgPrecip)
summary(ROF$Slope)
summary(ROF$Elevation)
summary(ROF$EcolSite)
summary(ROF$LandscapeType)

summary(SAL$AvgPrecip)
summary(SAL$Slope)
summary(SAL$Elevation)
summary(SAL$EcolSite)
summary(SAL$LandscapeType)

summary(SAN$AvgPrecip)
summary(SAN$Slope)
summary(SAN$Elevation)
summary(SAN$EcolSite)
summary(SAN$LandscapeType)

summary(FORE$AvgPrecip)
summary(FORE$Slope)
summary(FORE$Elevation)
summary(FORE$EcolSite)
summary(FORE$LandscapeType)

#stratum description, soil pits using TerrADat Soil Horizons records
View(soildata)
head(soildata$Actual.Eco.Site)

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

summary(LB$Texture)
summary(LB$RockFragments)
summary(LB$Effer)
summary(LB$ESD_PctClay)

summary(LOA$Texture)
summary(LOA$RockFragments)
summary(LOA$Effer)
summary(LOA$ESD_PctClay)

summary(MO$Texture)
summary(MO$RockFragments)
summary(MO$Effer)
summary(MO$ESD_PctClay)

summary(OTH$Texture)
summary(OTH$RockFragments)
summary(OTH$Effer)
summary(OTH$ESD_PctClay)

summary(ROF$Texture)
summary(ROF$RockFragments)
summary(ROF$Effer)
summary(ROF$ESD_PctClay)

summary(SAL$Texture)
summary(SAL$RockFragments)
summary(SAL$Effer)
summary(SAL$ESD_PctClay)

summary(SAN$Texture)
summary(SAN$RockFragments)
summary(SAN$Effer)
summary(SAN$ESD_PctClay)

summary(FORE$Texture)
summary(FORE$RockFragments)
summary(FORE$Effer)
summary(FORE$ESD_PctClay)