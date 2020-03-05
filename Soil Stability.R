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

#replace abbrevofyourecosite<-subset(Plotdata,Actual.Eco.Site=="abbrevofyourecosite")
#which can be done using ctrl + f , replace all 
#summary section (collaborates all strata)
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