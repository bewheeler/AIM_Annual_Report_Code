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

#surface cover, addinging needed columns to AIMdata
AIMdata$shrub.subshrub<-AIMdata$Shrub.Cover.Pct.Any.Hit+AIMdata$Noxious.SubShrub.Cover.Pct.Any.Hit+AIMdata$NonNoxious.SubShrub.Cover.Pct.Any.Hit
AIMdata$succulent<-AIMdata$Noxious.Succulent.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Succulent.Cover.Pct.Any.Hit
AIMdata$tree<-AIMdata$Noxious.Tree.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Tree.Cover.Pct.Any.Hit
AIMdata$noxiousforb<-AIMdata$Noxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$Noxious.Perennial.Forb.Cover.Pct.Any.Hit
AIMdata$nonnoxiousforb<-AIMdata$NonNoxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Perennial.Forb.Cover.Pct.Any.Hit


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

summary(BHG$Gaps)
summary(BHG$CanopyCover)

summary(LBG$Gaps)
summary(LBG$CanopyCover)

summary(LOAG$Gaps)
summary(LOAG$CanopyCover)

summary(MOG$Gaps)
summary(MOG$CanopyCover)

summary(OTHG$Gaps)
summary(OTHG$CanopyCover)

summary(ROFG$Gaps)
summary(ROFG$CanopyCover)

summary(SALG$Gaps)
summary(SALG$CanopyCover)

summary(SANG$Gaps)
summary(SANG$CanopyCover)

summary(FOREG$Gaps)
summary(FOREG$CanopyCover)

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
#
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
