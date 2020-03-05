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

#surface cover, addinging needed columns to AIMdata
AIMdata$shrub.subshrub<-AIMdata$Shrub.Cover.Pct.Any.Hit+AIMdata$Noxious.SubShrub.Cover.Pct.Any.Hit+AIMdata$NonNoxious.SubShrub.Cover.Pct.Any.Hit
AIMdata$succulent<-AIMdata$Noxious.Succulent.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Succulent.Cover.Pct.Any.Hit
AIMdata$tree<-AIMdata$Noxious.Tree.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Tree.Cover.Pct.Any.Hit
AIMdata$noxiousforb<-AIMdata$Noxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$Noxious.Perennial.Forb.Cover.Pct.Any.Hit
AIMdata$nonnoxiousforb<-AIMdata$NonNoxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Perennial.Forb.Cover.Pct.Any.Hit


#continuing with calculating surface cover

AIMdata1<-AIMdata %>%
  select(Bare.Soil.Pct,Forb.Cover.Pct.Any.Hit,Grass.Cover.Pct.Any.Hit,Total.Litter.Cover.Pct.First.Hit,Rock.Cover.Pct.First.Hit,shrub.subshrub,succulent,tree,Foliar.Cover.Pct,Actual.Eco.Site)

AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site) 
Covers<-c("Bare Soil", "Forb","Grass", "Litter", "Rock","Shrub","Succulent","Tree","Foliar")
AIMdata2$covertype <- factor(AIMdata2$covertype, levels = c("Bare.Soil.Pct","Forb.Cover.Pct.Any.Hit","Grass.Cover.Pct.Any.Hit","Total.Litter.Cover.Pct.First.Hit","Rock.Cover.Pct.First.Hit","shrub.subshrub","succulent","tree","Foliar.Cover.Pct","Actual.Eco.Site"))

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
LOAT<-LOST %>% 
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

#replace with your dominant grass species plant code. If only one plant code remove everything after digits=2) except the parantheses
#for example the code below would become domgrassme<-c(round(mean(BHA$Species=="BOGR2", na.rm=TRUE), digits=2))
#if there is no dominant species for the growth form list "NA" as seen below in domherme<-"NA"
domgrassme<-c(round(mean(BHA$Species=="BOGR2", na.rm=TRUE), digits=2),round(mean(BHA$Species=="HECO26", na.rm=TRUE),digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(BHA$Species=="ARTRV", na.rm=TRUE), digits=2),round(mean(BHA$Species=="PIED", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(BHA$Species=="BOGR2", na.rm=TRUE), digits=2),round(max(BHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(BHA$Species=="ARTRV", na.rm=TRUE), digits=2),round(max(BHA$Species=="PIED", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(BHA$Species=="BOGR2", na.rm=TRUE), digits=2),round(min(BHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(BHA$Species=="ARTRV", na.rm=TRUE), digits=2),round(min(BHA$Species=="PIED", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(BHA$Species=="BOGR2", na.rm=TRUE), digits=2),round(sd(BHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(BHA$Species=="ARTRV", na.rm=TRUE), digits=2),round(sd(BHA$Species=="PIED", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(BHA$Species=="BOGR2", na.rm=TRUE)/BHN2)),digits = 2) , round((1.282*(sd(BHA$Species=="HECO26", na.rm=TRUE)/BHN2)),digits=2), round((1.282*(sd(BHA$Species=="AGCR", na.rm=TRUE)/BHN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(BHA$Species=="ARTRV", na.rm=TRUE)/BHN2)),digits=2))

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
domgrassme<-c(round(mean(LBA$Species=="BOGR2", na.rm=TRUE), digits=2),round(mean(LBA$Species=="PASM", na.rm=TRUE), digits=2))
domherbme<-c(round(mean(LBA$Species=="SATR12", na.rm=TRUE), digits=2))
domwoodme<-c(round(mean(LBA$Species=="KRLA2", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(LBA$Species=="BOGR2", na.rm=TRUE), digits=2),round(max(LBA$Species=="PASM", na.rm=TRUE), digits=2))
domherbma<-c(round(max(LBA$Species=="SATR12", na.rm=TRUE), digits=2))
domwoodma<-c(round(max(LBA$Species=="KRLA2", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(LBA$Species=="BOGR2", na.rm=TRUE), digits=2),round(min(LBA$Species=="PASM", na.rm=TRUE), digits=2))
domherbmi<-c(round(min(LBA$Species=="SATR12", na.rm=TRUE), digits=2))
domwoodmi<-c(round(min(LBA$Species=="KRLA2", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(LBA$Species=="BOGR2", na.rm=TRUE), digits=2),round(sd(LBA$Species=="PASM", na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(LBA$Species=="SATR12", na.rm=TRUE), digits=2))
domwoodsd<-c(round(sd(LBA$Species=="KRLA2", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(LBA$Species=="BOGR2", na.rm=TRUE)/LBN2)),digits = 2) , round((1.282*(sd(LBA$Species=="PASM", na.rm=TRUE)/LBN2)),digits=2))
domherbCI<-c(round((1.282*(sd(LBA$Species=="SATR12", na.rm=TRUE)/LBN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(LBA$Species=="KRLA2", na.rm=TRUE)/LBN2)),digits=2))

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
domgrassme<-c(round(mean(LOAA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbme<-c(round(mean(LOAA$Species=="MUMO", na.rm=TRUE),digits=2),round(mean(LOAA$Species=="MUTO2", na.rm=TRUE),digits=2))
domwoodme<-c(round(mean(LOAA$Species=="PIED", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(LOAA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbma<-c(round(max(LOAA$Species=="MUMO", na.rm=TRUE),digits=2),round(max(LOAA$Species=="MUTO2", na.rm=TRUE),digits=2))
domwoodma<-c(round(max(LOAA$Species=="PIED", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(LOAA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbmi<-c(round(min(LOAA$Species=="MUMO", na.rm=TRUE),digits=2),round(min(LOAA$Species=="MUTO2", na.rm=TRUE),digits=2))
domwoodmi<-c(round(min(LOAA$Species=="PIED", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(LOAA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(LOAA$Species=="MUMO", na.rm=TRUE),digits=2),round(sd(LOAA$Species=="MUTO2", na.rm=TRUE),digits=2))
domwoodsd<-c(round(sd(LOAA$Species=="PIED", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(LOAA$Species=="BOGR2", na.rm=TRUE)/LOAN2)),digits = 2))
domherbCI<-c(round((1.282*(sd(LOAA$Species=="MUMO", na.rm=TRUE)/LOAN2)),digits=2),round((1.282*(sd(LOAA$Species=="MUTO2", na.rm=TRUE)/LOAN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(LOAA$Species=="ARTRV", na.rm=TRUE)/LOAN2)),digits=2))

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

domgrassme<-c(round(mean(MOA$Species=="BOGR2", na.rm=TRUE), digits=2),round(mean(MOA$Species=="HECO26", na.rm=TRUE), digits=2),round(mean(MOA$Species=="PASM", na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(MOA$Species=="CHGR6", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(MOA$Species=="BOGR2", na.rm=TRUE), digits=2),round(max(MOA$Species=="HECO26", na.rm=TRUE), digits=2),round(max(MOA$Species=="PASM", na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(MOA$Species=="CHGR6", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(MOA$Species=="BOGR2", na.rm=TRUE), digits=2),round(min(MOA$Species=="HECO26", na.rm=TRUE), digits=2),round(min(MOA$Species=="PASM", na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(MOA$Species=="CHGR6", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(MOA$Species=="BOGR2", na.rm=TRUE), digits=2),round(sd(MOA$Species=="HECO26", na.rm=TRUE), digits=2),round(sd(MOA$Species=="PASM", na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(MOA$Species=="CHGR6", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(MOA$Species=="BOGR2", na.rm=TRUE)/MON2)),digits = 2) , round((1.282*(sd(MOA$Species=="HECO26", na.rm=TRUE)/MON2)),digits=2), round((1.282*(sd(MOA$Species=="PASM", na.rm=TRUE)/MON2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(MOA$Species=="CHGR6", na.rm=TRUE)/MON2)),digits=2))

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
             "Dominant Grass Cover (%)","HECO26",
             "Dominant Herb. Cover (%)", "MUMO",
             "Dominant Woody Cover (%)","PIED","PIMI",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

domgrassme<-c(round(mean(OTHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbme<-c(round(mean(OTHA$Species=="MUMO", na.rm=TRUE), digits=2))
domwoodme<-c(round(mean(OTHA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(OTHA$Species=="PIMI", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(OTHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbma<-c(round(max(OTHA$Species=="MUMO", na.rm=TRUE), digits=2))
domwoodma<-c(round(max(OTHA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(OTHA$Species=="PIMI", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(OTHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbmi<-c(round(min(OTHA$Species=="MUMO", na.rm=TRUE), digits=2))
domwoodmi<-c(round(min(OTHA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(OTHA$Species=="PIMI", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(OTHA$Species=="HECO26", na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(OTHA$Species=="MUMO", na.rm=TRUE), digits=2))
domwoodsd<-c(round(sd(OTHA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(OTHA$Species=="PIMI", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(OTHA$Species=="HECO26", na.rm=TRUE)/OTHN2)),digits = 2))
domherbCI<-c(round((1.282*(sd(OTHA$Species=="MUMO", na.rm=TRUE)/OTHN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(OTHA$Species=="PIED", na.rm=TRUE)/OTHN2)),digits=2),round((1.282*(sd(OTHA$Species=="PIMI", na.rm=TRUE)/OTHN2)),digits=2))

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

domgrassme<-c(round(mean(ROFA$Species=="HECO26", na.rm=TRUE), digits=2),round(mean(ROFA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(ROFA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(ROFA$Species=="JUMO", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(ROFA$Species=="HECO26", na.rm=TRUE), digits=2),round(max(ROFA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(ROFA$Species=="PIED", na.rm=TRUE), digits=2),round(max(ROFA$Species=="JUMO", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(ROFA$Species=="HECO26", na.rm=TRUE), digits=2),round(min(ROFA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(ROFA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(ROFA$Species=="JUMO", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(ROFA$Species=="HECO26", na.rm=TRUE), digits=2),round(sd(ROFA$Species=="BOGR2", na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(ROFA$Species=="PIED", na.rm=TRUE), digits=2),round(mean(ROFA$Species=="JUMO", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(ROFA$Species=="HECO26", na.rm=TRUE)/ROFN2)),digits = 2),round((1.282*(sd(ROFA$Species=="BOGR2", na.rm=TRUE)/ROFN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(ROFA$Species=="PIED", na.rm=TRUE)/ROFN2)),digits=2),round((1.282*(sd(ROFA$Species=="JUMO", na.rm=TRUE)/ROFN2)),digits=2))

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

domgrassme<-c(round(mean(SALA$Species=="DISP", na.rm=TRUE), digits=2))
domherbme<-c(round(mean(SALA$Species=="ERCE2", na.rm=TRUE), digits=2))
domwoodme<-c(round(mean(SALA$Species=="SAVE4", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(SALA$Species=="DISP", na.rm=TRUE), digits=2))
domherbma<-c(round(max(SALA$Species=="ERCE2", na.rm=TRUE), digits=2))
domwoodma<-c(round(max(SALA$Species=="SAVE4", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(SALA$Species=="DISP", na.rm=TRUE), digits=2))
domwoodmi<-c(round(min(SALA$Species=="SAVE4", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(SALA$Species=="DISP", na.rm=TRUE), digits=2))
domherbsd<-c(round(sd(SALA$Species=="ERCE2", na.rm=TRUE), digits=2))
domwoodsd<-c(round(sd(SALA$Species=="SAVE4", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(SALA$Species=="DISP", na.rm=TRUE)/SALN2)),digits = 2)) 
domherbCI<-c(round((1.282*(sd(SALA$Species=="ERCE2", na.rm=TRUE)/SALN2)),digits=2))
domwoodCI<-c(round((1.282*(sd(SALA$Species=="SAVE4", na.rm=TRUE)/SALN2)),digits=2))

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

domgrassme<-c(round(mean(SANA$Species=="BOGR2", na.rm=TRUE), digits=2),round(mean(SANA$Species=="DISP", na.rm=TRUE), digits=2),round(mean(SANA$Species=="ACHY", na.rm=TRUE), digits=2),round(mean(SANA$Species=="SPCR", na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(SANA$Species=="NA", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(SANA$Species=="BOGR2", na.rm=TRUE), digits=2),round(max(SANA$Species=="DISP", na.rm=TRUE), digits=2),round(max(SANA$Species=="ACHY", na.rm=TRUE), digits=2),round(max(SANA$Species=="SPCR", na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(SANA$Species=="NA", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(SANA$Species=="BOGR2", na.rm=TRUE), digits=2),round(min(SANA$Species=="DISP", na.rm=TRUE), digits=2),round(min(SANA$Species=="ACHY", na.rm=TRUE), digits=2),round(min(SANA$Species=="SPCR", na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(SANA$Species=="NA", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(SANA$Species=="BOGR2", na.rm=TRUE), digits=2),round(sd(SANA$Species=="DISP", na.rm=TRUE), digits=2),round(sd(SANA$Species=="ACHY", na.rm=TRUE), digits=2),round(sd(SANA$Species=="SPCR", na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(SANA$Species=="NA", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(SANA$Species=="BOGR2", na.rm=TRUE)/SANN2)),digits = 2) , round((1.282*(sd(SANA$Species=="DISP", na.rm=TRUE)/SANN2)),digits=2), round((1.282*(sd(SANA$Species=="ACHY", na.rm=TRUE)/SANN2)),digits=2), round((1.282*(sd(SANA$Species=="SPCR", na.rm=TRUE)/SANN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(SANA$Species=="NA", na.rm=TRUE)/SANN2)),digits=2))

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
             "Dominant Grass Cover (%)","ACSC11","ELEL5","AGCR",
             "Dominant Herb. Cover (%)", "NA",
             "Dominant Woody Cover (%)","ARTRV",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

domgrassme<-c(round(mean(FOREA$Species=="ACSC11", na.rm=TRUE), digits=2),round(mean(FOREA$Species=="ELEL5", na.rm=TRUE), digits=2),round(mean(FOREA$Species=="AGCR", na.rm=TRUE), digits=2))
domherbme<-"NA"
domwoodme<-c(round(mean(FOREA$Species=="ARTRV", na.rm=TRUE), digits=2))

domgrassma<-c(round(max(FOREA$Species=="ACSC11", na.rm=TRUE), digits=2),round(max(FOREA$Species=="ELEL5", na.rm=TRUE), digits=2),round(max(FOREA$Species=="AGCR", na.rm=TRUE), digits=2))
domherbma<-"NA"
domwoodma<-c(round(max(FOREA$Species=="ARTRV", na.rm=TRUE), digits=2))

domgrassmi<-c(round(min(FOREA$Species=="ACSC11", na.rm=TRUE), digits=2),round(min(FOREA$Species=="ELEL5", na.rm=TRUE), digits=2),round(min(FOREA$Species=="AGCR", na.rm=TRUE), digits=2))
domherbmi<-"NA"
domwoodmi<-c(round(min(FOREA$Species=="ARTRV", na.rm=TRUE), digits=2))

domgrasssd<-c(round(sd(FOREA$Species=="ACSC11", na.rm=TRUE), digits=2),round(sd(FOREA$Species=="ELEL5", na.rm=TRUE), digits=2),round(sd(FOREA$Species=="AGCR", na.rm=TRUE), digits=2))
domherbsd<-"NA"
domwoodsd<-c(round(sd(FOREA$Species=="ARTRV", na.rm=TRUE), digits=2))

domgrassCI<-c(round((1.282*(sd(FOREA$Species=="ACSC11", na.rm=TRUE)/FOREN2)),digits = 2) , round((1.282*(sd(FOREA$Species=="ELEL5", na.rm=TRUE)/FOREN2)),digits=2), round((1.282*(sd(FOREA$Species=="AGCR", na.rm=TRUE)/FOREN2)),digits=2))
domherbCI<-"NA"
domwoodCI<-c(round((1.282*(sd(FOREA$Species=="ARTRV", na.rm=TRUE)/FOREN2)),digits=2))

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