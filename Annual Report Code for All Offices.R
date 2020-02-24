#packages installed= data.table, dplyr, formattable, tidyr using function install.packages("")
library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(formattable)
library(qwraps2)


#Actual.Eco.Site is a column I added to each TerrADat csv to assign the strata abbreviation
#Actual.Eco.Site has to be assiged for to each plot in all CSV forms for this code to work
#an easy way to only need to do this is assign whatever abbreviation to each plot in the query form
#with the column name Actual.Eco.Site . then use R to add it to all other CSVs by using code
#below strata(Query results for all offices) is the csv which I assigned strata abbreviations to
#all CSVs are saved in my documents(thus the path below)
strata<-read.csv("~/Plotdata.csv")
AIMdata<-read.csv("~/Allyears_query.csv")
Plotdata<-read.csv("~/Allyears_plots.csv")
Plotdata$Actual.Eco.Site<-strata$Actual.Eco.Site[match(Plotdata$PrimaryKey,strata$PrimaryKey)]
AIMdata$Actual.Eco.Site<-strata$Actual.Eco.Site[match(AIMdata$Primary.Key,strata$PrimaryKey)]

#another option is to change Actual.Eco.Site in the code to Ecological.Site.ID if you want to 
#just use ecological sites, this will also need to be added to every CSV except Query
#(already in Query), and can be added to each form the same way discussed in assigning Actual.Eco.Sites easily
#CSVs have to be saved as CSV with the names as follows: plots = "Allyears_plots" , soil horizon = "Allyears_soilhorizons"
#query results = "Allyears_query", plant specis = "PSPPALL" , species richness = "Allyears_species_rich"
#LPI detail = "LPI_all"

#below is the stratum description, general using TerrADat form "Plots"
#replace strata abbreviations with your strata/zone/allotment name or abbreviation 
#(has to be exact abbreviation used in your CSVs)
#which can be done by highlighting the current strata abbreviation,for example,BH
#typing ctrl+F, typing your
#strata/zone/allotment abbreviation into the replace box and clicking All

#only need this if didn't load at the start of the code
Plotdata<-read.csv("~/Allyears_plots_S.csv")
View(Plotdata)
#
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

summary(ROF$AvgPrecip)
summary(ROF$Slope)
summary(ROF$Elevation)
summary(ROF$EcolSite)
summary(ROF$LandscapeType)

#stratum description, soil pits using TerrADat Soil Horizons records
soildata<-read.csv("~/Allyears_soilhorizons.csv")
View(soildata)

BH<-subset(soildata,Actual.Eco.Site=="BH")
LB<-subset(soildata,Actual.Eco.Site=="LB")
LOA<-subset(soildata,Actual.Eco.Site=="LO")
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

summary(FORE$Texture)
summary(FORE$RockFragments)
summary(FORE$Effer)
summary(FORE$ESD_PctClay)

summary(SAN$Texture)
summary(SAN$RockFragments)
summary(SAN$Effer)
summary(SAN$ESD_PctClay)

summary(SAL$Texture)
summary(SAL$RockFragments)
summary(SAL$Effer)
summary(SAL$ESD_PctClay)

summary(OTH$Texture)
summary(OTH$RockFragments)
summary(OTH$Effer)
summary(OTH$ESD_PctClay)

summary(ROF$Texture)
summary(ROF$RockFragments)
summary(ROF$Effer)
summary(ROF$ESD_PctClay)

#surface cover
#only need if didn't assign at the start of the code
AIMdata<-read.csv("~/Allyears_query.csv")
View(AIMdata)
#
AIMdata$shrub.subshrub<-AIMdata$Shrub.Cover.Pct.Any.Hit+AIMdata$Noxious.SubShrub.Cover.Pct.Any.Hit+AIMdata$NonNoxious.SubShrub.Cover.Pct.Any.Hit
AIMdata$succulent<-AIMdata$Noxious.Succulent.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Succulent.Cover.Pct.Any.Hit
AIMdata$tree<-AIMdata$Noxious.Tree.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Tree.Cover.Pct.Any.Hit
AIMdata$noxiousforb<-AIMdata$Noxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$Noxious.Perennial.Forb.Cover.Pct.Any.Hit
AIMdata$nonnoxiousforb<-AIMdata$NonNoxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Perennial.Forb.Cover.Pct.Any.Hit

#first,calculating Population Size, because it will be used throughout the entire script
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
OTHN<-count(OT,Primary.Key)
OTHN2<-sum(OTN$n)
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

BHpop<-paste("BH Surface and Vegetation Cover by Strata   n=",BHN2)
LBpop<-paste("LB Surface and Vegetation Cover by Strata   n=",LBN2)
LOApop<-paste("LO Surface and Vegetation Cover by Strata   n=",LOAN2)
MOpop<-paste("MO Surface and Vegetation Cover by Strata   n=",MON2)
OTHpop<-paste("OT Surface and Vegetation Cover by Strata   n=",OTHN2)
ROFpop<-paste("ROF Surface and Vegetation Cover by Strata   n=",ROFN2)
SALpop<-paste("SAL Surface and Vegetation Cover by Strata   n=",SALN2)
SANpop<-paste("SAN Surface and Vegetation Cover by Strata   n=",SANN2)
FOREpop<-paste("FO Surface and Vegetation Cover by Strata   n=",FOREN2)

SC<-ggplot(AIMdata2,aes(x=factor(covertype),y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle()+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC3<-SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = c("Bare Soil", "Forb","Grass", "Litter","Rock", "Shrub/subshrub","Succulent","Tree", "Foliar"))+theme(axis.text.x=element_text(colour="gray20"))
SC3+facet_wrap(~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))


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


#native vs nonnative
AIMdata1<-AIMdata%>%
  select(Noxious.Cover.Pct.Any.Hit,NonNoxious.Plant.Cover.Pct.Any.Hit,Actual.Eco.Site)
AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)
head(AIMdata2)


Covers<-c("Nonnox.", "Noxious")
NN<-ggplot(AIMdata2,aes(x=covertype,y=covervalue, col=covertype))+geom_point(alpha=0.4)
NN2<-NN+geom_boxplot()
NN3<-NN2+ggtitle("Noxious and Nonnoxious Species Comparison by Strata")+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
NN4<-NN3+scale_x_discrete(labels= Covers)+scale_color_manual(values=c("seagreen2","darkred"),labels = c("Nonnoxious", "Noxious"))+theme(axis.text.x=element_text(colour="gray20"))
NN4+facet_wrap(~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

#perennial and annual grass
AIMdata1<-AIMdata %>%
  select(Perennial.Grass.Cover.Pct.Any.Hit,Annual.Grass.Cover.Pct.Any.Hit,Actual.Eco.Site)
AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)

Covers<-c("Annual Grass","Perennial Grass")
G<-ggplot(AIMdata2, aes(x=covertype,y=covervalue,col=covertype))
G2<-G+geom_boxplot()
G3<-G2+ggtitle(" Percent Cover of Annual and Perennial Grass (n=)")+labs(x="Duration",y="Average Percent Cover",colour="Duration")+scale_x_discrete(labels= Covers)
G3+facet_wrap(.~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))+ scale_color_manual(values=c("paleturquoise3", "rosybrown3"),labels = c("Perennial", "Annual"))

#Dominant Species
##if a blank table is produced, no species meet the criteria of being present on 20% of the plots. To adjust criteria change proportion on filter(dom_ss>=.2)
##using the same reference that led to the process for choosing how to calculate dominant species,
##if any of thespecies produced in the N_category=1 than that species is ecologically meaningful for that strata/allotment/etc
PSPSdata<-read.csv("~/PSPPALL.csv")
head(PSPSdata)
PSPS1<-PSPSdata[!is.na(PSPSdata$AH_SpeciesCover),]

##BH STRATA
##list of dominant species in order of species with highest percent occurence per plot
BH<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(BH$PrimaryKey))-N_category) %>%
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
BH2<-BH %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
BH5<-BH4[!duplicated(BH4),]
formattable(BH5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
GrowthHabit_List<-c(A2,B2,C2,D2)
zeros<-data.frame(Species=Species_list,AH_SpeciesCover=AH_SpeciesCover_list,PrimaryKey=PrimaryKey_list,GrowthHabit=GrowthHabit_List)
BH8<-rbind(as.data.frame(BH7),zeros)
##BOX PLOT TITLES
BHpop<-paste("BH Percent Cover of Dominant Species   n=",BHN2)
LBpop<-paste("LB Percent Cover of Dominant Species   n=",LBN2)
LOApop<-paste("LO Percent Cover of Dominant Species   n=",LOAN2)
MOpop<-paste("MO Percent Cover of Dominant Species   n=",MON2)
OTHpop<-paste("OT Percent Cover of Dominant Species   n=",OTHN2)
ROFpop<-paste("ROF Percent Cover of Dominant Species   n=",ROFN2)
SALpop<-paste("SAL Percent Cover of Dominant Species   n=",SALN2)
SANpop<-paste("SAN Percent Cover of Dominant Species   n=",SANN2)
FOREpop<-paste("FO Percent Cover of Dominant Species   n=",FOREN2)

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
##list of dominant species in order of species with highest percent occurence per plot
LB<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(LB$PrimaryKey))-N_category) %>%
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
LB2<-LB %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
LB5<-LB2[!duplicated(LB2),]
formattable(LB5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
LBA<-LB %>%
  filter(Species %in% domss)
LB6<-LBA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
LB7<-as.data.frame(LB6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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
LOA<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(LOA$PrimaryKey))-N_category) %>%
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
LOA2<-LOA %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
LOA5<-LOA2[!duplicated(LOA2),]
formattable(LOA5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
LOAA<-LOA %>%
  filter(Species %in% domss)
LOA6<-LOAA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
LOA7<-as.data.frame(LOA6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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
MO<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(MO$PrimaryKey))-N_category) %>%
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
MO2<-MO %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
MO5<-MO2[!duplicated(MO2),]
formattable(MO5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
MOA<-MO %>%
  filter(Species %in% domss)
MO6<-MOA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
MO7<-as.data.frame(MO6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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
OTH<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(OTH$PrimaryKey))-N_category) %>%
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
OTH2<-OTH %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
OTH5<-OTH2[!duplicated(OTH2),]
formattable(OTH5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
OTHA<-OTH %>%
  filter(Species %in% domss)
OTH6<-OTHA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
OTH7<-as.data.frame(OTH6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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
ROF<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(ROF$PrimaryKey))-N_category) %>%
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
ROF2<-ROF %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
ROF5<-ROF2[!duplicated(ROF2),]
formattable(ROF5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
ROFA<-ROF %>%
  filter(Species %in% domss)
ROF6<-ROFA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
ROF7<-as.data.frame(ROF6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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
SAL<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(SAL$PrimaryKey))-N_category) %>%
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
SAL2<-SAL %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
SAL5<-SAL2[!duplicated(SAL2),]
formattable(SAL5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
SALA<-SAL %>%
  filter(Species %in% domss)
SAL6<-SALA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
SAL7<-as.data.frame(SAL6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
GrowthHabit_List<-c(A2,B2,C2,D2)
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
SAN<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(SAN$PrimaryKey))-N_category) %>%
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
SAN2<-SAN %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
SAN5<-SAN2[!duplicated(SAN2),]
formattable(SAN5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
SANA<-SAN %>%
  filter(Species %in% domss)
SAN6<-SANA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
SAN7<-as.data.frame(SAN6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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
FORE<- PSPS1 %>%
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
  mutate(SS_avg=sum(AH_SpeciesCover)/length(unique(PrimaryKey)),zero=length(unique(FORE$PrimaryKey))-N_category) %>%
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
FORE2<-FORE %>%
  group_by(N_category)%>%
  select(Species,zero) %>%
  group_by(Species)
FORE5<-FORE2[!duplicated(FORE2),]
formattable(FORE5)

##adding plots with 0 value, replace domss and species name and zero # based on results of last step
domss<-c("BOGR2","HECO26","KRLA2","PIED")
FOREA<-FORE %>%
  filter(Species %in% domss)
FORE6<-FOREA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
FORE7<-as.data.frame(FORE6)
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
AH_SpeciesCover_list<-replicate(76,0)
PrimaryKey_list<-1:76
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


#soil stability
AIMdata<-read.csv("~/Allyears_query.csv")

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
Average<-c(mean(BH$Soil.Stability.All),mean(BH$Soil.Stability.Protected),mean(BH$Soil.Stability.Unprotected))
Minimum<-c(min(BH$Soil.Stability.All),min(BH$Soil.Stability.Protected),min(BH$Soil.Stability.Unprotected))
Maximum<-c(max(BH$Soil.Stability.All),max(BH$Soil.Stability.Protected),max(BH$Soil.Stability.Unprotected))
St.Dev.<-c(sd(BH$Soil.Stability.All),sd(BH$Soil.Stability.Protected),sd(BH$Soil.Stability.Unprotected))
BHdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(BHdataframe, list(
  Type = blank_bold))

#LB
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(LB$Soil.Stability.All),mean(LB$Soil.Stability.Protected),mean(LB$Soil.Stability.Unprotected))
Minimum<-c(min(LB$Soil.Stability.All),min(LB$Soil.Stability.Protected),min(LB$Soil.Stability.Unprotected))
Maximum<-c(max(LB$Soil.Stability.All),max(LB$Soil.Stability.Protected),max(LB$Soil.Stability.Unprotected))
St.Dev.<-c(sd(LB$Soil.Stability.All),sd(LB$Soil.Stability.Protected),sd(LB$Soil.Stability.Unprotected))
LBdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(LBdataframe, list(
  Type = blank_bold))

#LOA
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(LOA$Soil.Stability.All),mean(LOA$Soil.Stability.Protected),mean(LOA$Soil.Stability.Unprotected))
Minimum<-c(min(LOA$Soil.Stability.All),min(LOA$Soil.Stability.Protected),min(LOA$Soil.Stability.Unprotected))
Maximum<-c(max(LOA$Soil.Stability.All),max(LOA$Soil.Stability.Protected),max(LOA$Soil.Stability.Unprotected))
St.Dev.<-c(sd(LOA$Soil.Stability.All),sd(LOA$Soil.Stability.Protected),sd(LOA$Soil.Stability.Unprotected))
LOAdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(LOAdataframe, list(
  Type = blank_bold))

#MO
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(MO$Soil.Stability.All),mean(MO$Soil.Stability.Protected),mean(MO$Soil.Stability.Unprotected))
Minimum<-c(min(MO$Soil.Stability.All),min(MO$Soil.Stability.Protected),min(MO$Soil.Stability.Unprotected))
Maximum<-c(max(MO$Soil.Stability.All),max(MO$Soil.Stability.Protected),max(MO$Soil.Stability.Unprotected))
St.Dev.<-c(sd(MO$Soil.Stability.All),sd(MO$Soil.Stability.Protected),sd(MO$Soil.Stability.Unprotected))
MOdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(MOdataframe, list(
  Type = blank_bold))

#OTH
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(OTH$Soil.Stability.All),mean(OTH$Soil.Stability.Protected),mean(OTH$Soil.Stability.Unprotected))
Minimum<-c(min(OTH$Soil.Stability.All),min(OTH$Soil.Stability.Protected),min(OTH$Soil.Stability.Unprotected))
Maximum<-c(max(OTH$Soil.Stability.All),max(OTH$Soil.Stability.Protected),max(OTH$Soil.Stability.Unprotected))
St.Dev.<-c(sd(OTH$Soil.Stability.All),sd(OTH$Soil.Stability.Protected),sd(OTH$Soil.Stability.Unprotected))
OTHdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(OTHdataframe, list(
  Type = blank_bold))

#ROF
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(ROF$Soil.Stability.All),mean(ROF$Soil.Stability.Protected),mean(ROF$Soil.Stability.Unprotected))
Minimum<-c(min(ROF$Soil.Stability.All),min(ROF$Soil.Stability.Protected),min(ROF$Soil.Stability.Unprotected))
Maximum<-c(max(ROF$Soil.Stability.All),max(ROF$Soil.Stability.Protected),max(ROF$Soil.Stability.Unprotected))
St.Dev.<-c(sd(ROF$Soil.Stability.All),sd(ROF$Soil.Stability.Protected),sd(ROF$Soil.Stability.Unprotected))
ROFdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(ROFdataframe, list(
  Type = blank_bold))

#SAL
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(SAL$Soil.Stability.All),mean(SAL$Soil.Stability.Protected),mean(SAL$Soil.Stability.Unprotected))
Minimum<-c(min(SAL$Soil.Stability.All),min(SAL$Soil.Stability.Protected),min(SAL$Soil.Stability.Unprotected))
Maximum<-c(max(SAL$Soil.Stability.All),max(SAL$Soil.Stability.Protected),max(SAL$Soil.Stability.Unprotected))
St.Dev.<-c(sd(SAL$Soil.Stability.All),sd(SAL$Soil.Stability.Protected),sd(SAL$Soil.Stability.Unprotected))
SALdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(SALdataframe, list(
  Type = blank_bold))

#SAN
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(SAN$Soil.Stability.All),mean(SAN$Soil.Stability.Protected),mean(SAN$Soil.Stability.Unprotected))
Minimum<-c(min(SAN$Soil.Stability.All),min(SAN$Soil.Stability.Protected),min(SAN$Soil.Stability.Unprotected))
Maximum<-c(max(SAN$Soil.Stability.All),max(SAN$Soil.Stability.Protected),max(SAN$Soil.Stability.Unprotected))
St.Dev.<-c(sd(SAN$Soil.Stability.All),sd(SAN$Soil.Stability.Protected),sd(SAN$Soil.Stability.Unprotected))
SANdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(SANdataframe, list(
  Type = blank_bold))

#FORE
Type<-c("All Samples","Under Plant Cover","No Cover")
Average<-c(mean(FORE$Soil.Stability.All),mean(FORE$Soil.Stability.Protected),mean(FORE$Soil.Stability.Unprotected))
Minimum<-c(min(FORE$Soil.Stability.All),min(FORE$Soil.Stability.Protected),min(FORE$Soil.Stability.Unprotected))
Maximum<-c(max(FORE$Soil.Stability.All),max(FORE$Soil.Stability.Protected),max(FORE$Soil.Stability.Unprotected))
St.Dev.<-c(sd(FORE$Soil.Stability.All),sd(FORE$Soil.Stability.Protected),sd(FORE$Soil.Stability.Unprotected))
FOREdataframe<-data.frame(Type,Average,Minimum,Maximum)
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x == Type, "bold", NA)))
formattable(FOREdataframe, list(
  Type = blank_bold))

#canopy gap chart
Gapdata<-AIMdata %>%
  select(Gap.Cover.25.to.50.Pct,Gap.Cover.51.to.100.Pct,Gap.Cover.101.to.200.Pct,Gap.Cover.200.Plus.Pct,Actual.Eco.Site)
Gapdata$CanopyCover<-100-(AIMdata$Gap.Cover.25.to.50.Pct+AIMdata$Gap.Cover.51.to.100.Pct+AIMdata$Gap.Cover.101.to.200.Pct+AIMdata$Gap.Cover.200.Plus.Pct)
Gapdata$Gaps<-Gapdata$Gap.Cover.25.to.50.Pct+Gapdata$Gap.Cover.51.to.100.Pct+Gapdata$Gap.Cover.101.to.200.Pct+Gapdata$Gap.Cover.200.Plus.Pct
Gapdata2<-gather(Gapdata,Gapsize,Percentofplot,-Actual.Eco.Site)
Gapdata2$Gapsize <- factor(Gapdata2$Gapsize, levels = c("CanopyCover","Gap.Cover.25.to.50.PCt", "Gap.Cover.51.to.100.PCt", "Gap.Cover.101.to.200.PCt","Gap.Cover.200.Plus.PCt"))

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
sd(BHG$CanopyCover)

summary(LBG$Gaps)
summary(LBG$CanopyCover)
sd(LBG$CanopyCover)

summary(LOAG$Gaps)
summary(LOAG$CanopyCover)
sd(LOAG$CanopyCover)

summary(MOG$Gaps)
summary(MOG$CanopyCover)
sd(MOG$CanopyCover)

summary(OTHG$Gaps)
summary(OTHG$CanopyCover)
sd(OTHG$CanopyCover)

summary(ROFG$Gaps)
summary(ROFG$CanopyCover)
sd(ROFG$CanopyCover)

summary(SALG$Gaps)
summary(SALG$CanopyCover)
sd(SALG$CanopyCover)

summary(SANG$Gaps)
summary(SANG$CanopyCover)
sd(SANG$CanopyCover)

summary(FOREG$Gaps)
summary(FOREG$CanopyCover)
sd(FOREG$CanopyCover)

#gap barchart
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
LOApop<-paste("LO Proportion Canopy Gap and Cover   n=",LOAN2)
MOpop<-paste("MO Proportion Canopy Gap and Cover   n=",MON2)
OTHpop<-paste("OT Proportion Canopy Gap and Cover   n=",OTHN2)
ROFpop<-paste("ROF Proportion Canopy Gap and Cover   n=",ROFN2)
SALpop<-paste("SAL Proportion Canopy Gap and Cover   n=",SALN2)
SANpop<-paste("SAN Proportion Canopy Gap and Cover   n=",SANN2)
FOREpop<-paste("FO Proportion Canopy Gap and Cover   n=",FOREN2)

GC<-ggplot(BHG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(LBG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(LOAG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(MOG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(OTHG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(ROFG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(SALG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(SANG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

GC<-ggplot(FOREG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)

#surface indicator summary tables, sagebrush height not included, tree and succulent height can be calculated using LPI detail

#SPECIES RICHNESS
SPdata<-read.csv("~/Allyears_species_rich.csv")
View(SPdata)
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

#the below species is calculating tree and shrub heights for the indicator summary table

ST<-read.csv("~/LPI_all.csv")
View(ST)

ST1<-ST %>%
  select(SpeciesWoody,HeightWoody,X)
STN<-na.omit(ST1)

STN$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(STN$X,Plotdata$PrimaryKey)]
  
PS2<-PSPSdata[!duplicated(PSPSdata$Species),]

STN$GrowthHabitsub<-PS2$GrowthHabitSub[match(STN$SpeciesWoody,PS2$Species)]  

BHST<-subset(STN,Actual.Eco.Site=="BH")
LBST<-subset(STN,Actual.Eco.Site=="LB")
LOST<-subset(STN,Actual.Eco.Site=="LO")
MOST<-subset(STN,Actual.Eco.Site=="MO")
OTHST<-subset(STN,Actual.Eco.Site=="OTH")
ROFST<-subset(STN,Actual.Eco.Site=="ROF")
SALST<-subset(STN,Actual.Eco.Site=="SAL")
SANST<-subset(STN,Actual.Eco.Site=="SAN")
FOREST<-subset(STN,Actual.Eco.Site=="FORE")

BHST2<-BHST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
BHT<-BHST2[!duplicated(BHST2$X),]
BHSS2<-BHST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
BHS<-BHSS2[!duplicated(BHSS2$X),]

LBST2<-LBST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
LBT<-LBST2[!duplicated(LBST2$X),]
LBSS2<-LBST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
LBS<-LBSS2[!duplicated(LBSS2$X),]

LOST2<-LOST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
LOT<-LOST2[!duplicated(LOST2$X),]
LOSS2<-LOST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
LOS<-LOSS2[!duplicated(LOSS2$X),]

MOST2<-MOST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
MOT<-MOST2[!duplicated(MOST2$X),]
MOSS2<-MOST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
MOS<-MOSS2[!duplicated(MOSS2$X),]

OTHST2<-OTHST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
OTHT<-OTHST2[!duplicated(OTHST2$X),]
OTHSS2<-OTHST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
OTHS<-OTHSS2[!duplicated(OTHSS2$X),]

ROFST2<-ROFST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
ROFT<-ROFST2[!duplicated(ROFST2$X),]
ROFSS2<-ROFST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
ROFS<-ROFSS2[!duplicated(ROFSS2$X),]

SALST2<-SALST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
SALT<-SALST2[!duplicated(SALST2$X),]
SALSS2<-SALST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
SALS<-SALSS2[!duplicated(SALSS2$X),]

SANST2<-SANST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
SANT<-SANST2[!duplicated(SANST2$X),]
SANSS2<-SANST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
SANS<-SANSS2[!duplicated(SANSS2$X),]

FOREST2<-FOREST %>% 
  filter(GrowthHabitsub=="Tree") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
FORET<-FOREST2[!duplicated(FOREST2$X),]
FORESS2<-FOREST %>%
  filter(GrowthHabitsub=="Shrub") %>%
  group_by(X) %>%
  mutate(AvgHeight=mean(HeightWoody))
FORES<-FORESS2[!duplicated(FORESS2$X),]

#for the below code replace "insert dom ss..." with dominant species names for the correct functional group. if none = "N/A" . If more than one
#for a functional group the name has to be added to Indicator as "species name", 
#then added to average using round(mean(BHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),
#added to min using round(min(BHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),  
#added to max and St.Dev using same code replacing the "min" with "max" and "sd"

#BH strata
Indicator<-c("Surface Indicator (%)","Rock Fragment", "Litter","Bare Soil",
             "Foliar Cover (%)","Shrub/Sub-shrub","Grass","Succulent",
             "Tree","Noxious Forb/Herb","Nonnoxious Forb/Herb","Vegetation Height (cm)",
             "Grass","Forb/Herb","Tree", "Shrub",
             "Dominant Grass Cover (%)","Insert Dom Grass SS names,if none = N/A",
             "Dominant Herb. Cover (%)", "Insert Dom Herb SS names,if none = N/A",
             "Dominant Woody Cover (%)","Insert Dom Woody SS names,if none = N/A",
             "Canopy Gaps (%)", "Gaps 25-50 cm", "Gaps 51-100 cm",
             "Gaps 101-200 cm","Gaps >200 cm", "Canopy Cover (%)",
             "Total Canopy Cover","Species Richness","No. Species per Plot")

Average<-c("",round(mean(BH$Rock.Cover.Pct.First.Hit), digits=2),round(mean(BH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(BH$Bare.Soil.Pct), digits=2),
           "",round(mean(BH$shrub.subshrub), digits=2), round(mean(BH$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(BH$succulent),digits=2),
           round(mean(BH$tree),digits=2),round(mean(BH$noxiousforb),digits=2),round(mean(BH$nonnoxiousforb),digits=2),"",
           round(mean(BH$Average.Grass.Height.cm),digits=2),round(mean(BH$Average.Forb.Height.cm), digits=2),round(mean(BHT$HeightWoody), digits=2),round(mean(BHS$HeightWoody), digits=2),"",
           round(mean(BHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(BHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(BHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(BHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(BHG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(BHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(BHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(BHG$CanopyCover), digits=2),"",round(mean(BHSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(BH$Rock.Cover.Pct.First.Hit), digits=2),round(min(BH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(BH$Bare.Soil.Pct), digits=2),
           "",round(min(BH$shrub.subshrub), digits=2), round(min(BH$Grass.Cover.Pct.Any.Hit),digits=2),round(min(BH$succulent),digits=2),
           round(min(BH$tree),digits=2),round(min(BH$noxiousforb),digits=2),round(min(BH$nonnoxiousforb),digits=2),"",
           round(min(BH$Average.Grass.Height.cm),digits=2),round(min(BH$Average.Forb.Height.cm), digits=2),round(min(BHT$HeightWoody), digits=2),round(mean(BHS$HeightWoody), digits=2),"",
           round(min(BHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(BHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(BHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(BHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(BHG$Gap.Cover.51.to.100.Pct), digits=2),round(min(BHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(BHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(BHG$CanopyCover), digits=2),"",round(min(BHSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(BH$Rock.Cover.Pct.First.Hit), digits=2),round(max(BH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(BH$Bare.Soil.Pct), digits=2),
           "",round(max(BH$shrub.subshrub), digits=2), round(max(BH$Grass.Cover.Pct.Any.Hit),digits=2),round(max(BH$succulent),digits=2),
           round(max(BH$tree),digits=2),round(max(BH$noxiousforb),digits=2),round(max(BH$nonnoxiousforb),digits=2),"",
           round(max(BH$Average.Grass.Height.cm),digits=2),round(max(BH$Average.Forb.Height.cm), digits=2),round(max(BHT$HeightWoody), digits=2),round(mean(BHS$HeightWoody), digits=2),"",
           round(max(BHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(BHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(BHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(BHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(BHG$Gap.Cover.51.to.100.Pct), digits=2),round(max(BHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(BHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(BHG$CanopyCover), digits=2),"",round(max(BHSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(BH$Rock.Cover.Pct.First.Hit), digits=2),round(sd(BH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(BH$Bare.Soil.Pct), digits=2),
           "",round(sd(BH$shrub.subshrub), digits=2), round(sd(BH$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(BH$succulent),digits=2),
           round(sd(BH$tree),digits=2),round(sd(BH$noxiousforb),digits=2),round(sd(BH$nonnoxiousforb),digits=2),"",
           round(sd(BH$Average.Grass.Height.cm),digits=2),round(sd(BH$Average.Forb.Height.cm), digits=2),round(sd(BHT$HeightWoody), digits=2),round(mean(BHS$HeightWoody), digits=2),"",
           round(sd(BHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(BHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(BHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(BHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(BHG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(BHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(BHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(BHG$CanopyCover), digits=2),"",round(sd(BHSR$SpeciesCount), digits=2))


BHdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(BHdataframe, list(
  Indicator = blank_bold))

#LB strata
Average<-c("",round(mean(LB$Rock.Cover.Pct.First.Hit), digits=2),round(mean(LB$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(LB$Bare.Soil.Pct), digits=2),
           "",round(mean(LB$shrub.subshrub), digits=2), round(mean(LB$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(LB$succulent),digits=2),
           round(mean(LB$tree),digits=2),round(mean(LB$noxiousforb),digits=2),round(mean(LB$nonnoxiousforb),digits=2),"",
           round(mean(LB$Average.Grass.Height.cm),digits=2),round(mean(LB$Average.Forb.Height.cm), digits=2),round(mean(LBT$HeightWoody), digits=2),round(mean(LBS$HeightWoody), digits=2),"",
           round(mean(LBA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(LBA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(LBA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(LBG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(LBG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(LBG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(LBG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(LBG$CanopyCover), digits=2),"",round(mean(LBSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(LB$Rock.Cover.Pct.First.Hit), digits=2),round(min(LB$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(LB$Bare.Soil.Pct), digits=2),
           "",round(min(LB$shrub.subshrub), digits=2), round(min(LB$Grass.Cover.Pct.Any.Hit),digits=2),round(min(LB$succulent),digits=2),
           round(min(LB$tree),digits=2),round(min(LB$noxiousforb),digits=2),round(min(LB$nonnoxiousforb),digits=2),"",
           round(min(LB$Average.Grass.Height.cm),digits=2),round(min(LB$Average.Forb.Height.cm), digits=2),round(min(LBT$HeightWoody), digits=2),round(mean(LBS$HeightWoody), digits=2),"",
           round(min(LBA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(LBA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(LBA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(LBG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(LBG$Gap.Cover.51.to.100.Pct), digits=2),round(min(LBG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(LBG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(LBG$CanopyCover), digits=2),"",round(min(LBSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(LB$Rock.Cover.Pct.First.Hit), digits=2),round(max(LB$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(LB$Bare.Soil.Pct), digits=2),
           "",round(max(LB$shrub.subshrub), digits=2), round(max(LB$Grass.Cover.Pct.Any.Hit),digits=2),round(max(LB$succulent),digits=2),
           round(max(LB$tree),digits=2),round(max(LB$noxiousforb),digits=2),round(max(LB$nonnoxiousforb),digits=2),"",
           round(max(LB$Average.Grass.Height.cm),digits=2),round(max(LB$Average.Forb.Height.cm), digits=2),round(max(LBT$HeightWoody), digits=2),round(mean(LBS$HeightWoody), digits=2),"",
           round(max(LBA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(LBA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(LBA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(LBG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(LBG$Gap.Cover.51.to.100.Pct), digits=2),round(max(LBG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(LBG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(LBG$CanopyCover), digits=2),"",round(max(LBSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(LB$Rock.Cover.Pct.First.Hit), digits=2),round(sd(LB$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(LB$Bare.Soil.Pct), digits=2),
           "",round(sd(LB$shrub.subshrub), digits=2), round(sd(LB$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(LB$succulent),digits=2),
           round(sd(LB$tree),digits=2),round(sd(LB$noxiousforb),digits=2),round(sd(LB$nonnoxiousforb),digits=2),"",
           round(sd(LB$Average.Grass.Height.cm),digits=2),round(sd(LB$Average.Forb.Height.cm), digits=2),round(sd(LBT$HeightWoody), digits=2),round(mean(LBS$HeightWoody), digits=2),"",
           round(sd(LBA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(LBA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(LBA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(LBG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(LBG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(LBG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(LBG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(LBG$CanopyCover), digits=2),"",round(sd(LBSR$SpeciesCount), digits=2))


LBdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(LBdataframe, list(
  Indicator = blank_bold))

#LOA strata
Average<-c("",round(mean(LOA$Rock.Cover.Pct.First.Hit), digits=2),round(mean(LOA$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(LOA$Bare.Soil.Pct), digits=2),
           "",round(mean(LOA$shrub.subshrub), digits=2), round(mean(LOA$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(LOA$succulent),digits=2),
           round(mean(LOA$tree),digits=2),round(mean(LOA$noxiousforb),digits=2),round(mean(LOA$nonnoxiousforb),digits=2),"",
           round(mean(LOA$Average.Grass.Height.cm),digits=2),round(mean(LOA$Average.Forb.Height.cm), digits=2),round(mean(LOAT$HeightWoody), digits=2),round(mean(LOAS$HeightWoody), digits=2),"",
           round(mean(LOAA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(LOAA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(LOAA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(LOAG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(LOAG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(LOAG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(LOAG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(LOAG$CanopyCover), digits=2),"",round(mean(LOASR$SpeciesCount), digits=2))

Minimum<-c("",round(min(LOA$Rock.Cover.Pct.First.Hit), digits=2),round(min(LOA$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(LOA$Bare.Soil.Pct), digits=2),
           "",round(min(LOA$shrub.subshrub), digits=2), round(min(LOA$Grass.Cover.Pct.Any.Hit),digits=2),round(min(LOA$succulent),digits=2),
           round(min(LOA$tree),digits=2),round(min(LOA$noxiousforb),digits=2),round(min(LOA$nonnoxiousforb),digits=2),"",
           round(min(LOA$Average.Grass.Height.cm),digits=2),round(min(LOA$Average.Forb.Height.cm), digits=2),round(min(LOAT$HeightWoody), digits=2),round(mean(LOAS$HeightWoody), digits=2),"",
           round(min(LOAA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(LOAA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(LOAA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(LOAG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(LOAG$Gap.Cover.51.to.100.Pct), digits=2),round(min(LOAG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(LOAG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(LOAG$CanopyCover), digits=2),"",round(min(LOASR$SpeciesCount), digits=2))

Maximum<-c("",round(max(LOA$Rock.Cover.Pct.First.Hit), digits=2),round(max(LOA$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(LOA$Bare.Soil.Pct), digits=2),
           "",round(max(LOA$shrub.subshrub), digits=2), round(max(LOA$Grass.Cover.Pct.Any.Hit),digits=2),round(max(LOA$succulent),digits=2),
           round(max(LOA$tree),digits=2),round(max(LOA$noxiousforb),digits=2),round(max(LOA$nonnoxiousforb),digits=2),"",
           round(max(LOA$Average.Grass.Height.cm),digits=2),round(max(LOA$Average.Forb.Height.cm), digits=2),round(max(LOAT$HeightWoody), digits=2),round(mean(LOAS$HeightWoody), digits=2),"",
           round(max(LOAA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(LOAA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(LOAA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(LOAG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(LOAG$Gap.Cover.51.to.100.Pct), digits=2),round(max(LOAG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(LOAG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(LOAG$CanopyCover), digits=2),"",round(max(LOASR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(LOA$Rock.Cover.Pct.First.Hit), digits=2),round(sd(LOA$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(LOA$Bare.Soil.Pct), digits=2),
           "",round(sd(LOA$shrub.subshrub), digits=2), round(sd(LOA$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(LOA$succulent),digits=2),
           round(sd(LOA$tree),digits=2),round(sd(LOA$noxiousforb),digits=2),round(sd(LOA$nonnoxiousforb),digits=2),"",
           round(sd(LOA$Average.Grass.Height.cm),digits=2),round(sd(LOA$Average.Forb.Height.cm), digits=2),round(sd(LOAT$HeightWoody), digits=2),round(mean(LOAS$HeightWoody), digits=2),"",
           round(sd(LOAA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(LOAA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(LOAA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(LOAG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(LOAG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(LOAG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(LOAG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(LOAG$CanopyCover), digits=2),"",round(sd(LOASR$SpeciesCount), digits=2))


LOAdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(LOAdataframe, list(
  Indicator = blank_bold))

#MO strata
Average<-c("",round(mean(MO$Rock.Cover.Pct.First.Hit), digits=2),round(mean(MO$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(MO$Bare.Soil.Pct), digits=2),
           "",round(mean(MO$shrub.subshrub), digits=2), round(mean(MO$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(MO$succulent),digits=2),
           round(mean(MO$tree),digits=2),round(mean(MO$noxiousforb),digits=2),round(mean(MO$nonnoxiousforb),digits=2),"",
           round(mean(MO$Average.Grass.Height.cm),digits=2),round(mean(MO$Average.Forb.Height.cm), digits=2),round(mean(MOT$HeightWoody), digits=2),round(mean(MOS$HeightWoody), digits=2),"",
           round(mean(MOA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(MOA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(MOA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(MOG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(MOG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(MOG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(MOG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(MOG$CanopyCover), digits=2),"",round(mean(MOSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(MO$Rock.Cover.Pct.First.Hit), digits=2),round(min(MO$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(MO$Bare.Soil.Pct), digits=2),
           "",round(min(MO$shrub.subshrub), digits=2), round(min(MO$Grass.Cover.Pct.Any.Hit),digits=2),round(min(MO$succulent),digits=2),
           round(min(MO$tree),digits=2),round(min(MO$noxiousforb),digits=2),round(min(MO$nonnoxiousforb),digits=2),"",
           round(min(MO$Average.Grass.Height.cm),digits=2),round(min(MO$Average.Forb.Height.cm), digits=2),round(min(MOT$HeightWoody), digits=2),round(mean(MOS$HeightWoody), digits=2),"",
           round(min(MOA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(MOA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(MOA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(MOG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(MOG$Gap.Cover.51.to.100.Pct), digits=2),round(min(MOG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(MOG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(MOG$CanopyCover), digits=2),"",round(min(MOSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(MO$Rock.Cover.Pct.First.Hit), digits=2),round(max(MO$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(MO$Bare.Soil.Pct), digits=2),
           "",round(max(MO$shrub.subshrub), digits=2), round(max(MO$Grass.Cover.Pct.Any.Hit),digits=2),round(max(MO$succulent),digits=2),
           round(max(MO$tree),digits=2),round(max(MO$noxiousforb),digits=2),round(max(MO$nonnoxiousforb),digits=2),"",
           round(max(MO$Average.Grass.Height.cm),digits=2),round(max(MO$Average.Forb.Height.cm), digits=2),round(max(MOT$HeightWoody), digits=2),round(mean(MOS$HeightWoody), digits=2),"",
           round(max(MOA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(MOA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(MOA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(MOG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(MOG$Gap.Cover.51.to.100.Pct), digits=2),round(max(MOG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(MOG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(MOG$CanopyCover), digits=2),"",round(max(MOSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(MO$Rock.Cover.Pct.First.Hit), digits=2),round(sd(MO$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(MO$Bare.Soil.Pct), digits=2),
           "",round(sd(MO$shrub.subshrub), digits=2), round(sd(MO$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(MO$succulent),digits=2),
           round(sd(MO$tree),digits=2),round(sd(MO$noxiousforb),digits=2),round(sd(MO$nonnoxiousforb),digits=2),"",
           round(sd(MO$Average.Grass.Height.cm),digits=2),round(sd(MO$Average.Forb.Height.cm), digits=2),round(sd(MOT$HeightWoody), digits=2),round(mean(MOS$HeightWoody), digits=2),"",
           round(sd(MOA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(MOA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(MOA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(MOG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(MOG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(MOG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(MOG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(MOG$CanopyCover), digits=2),"",round(sd(MOSR$SpeciesCount), digits=2))


MOdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(MOdataframe, list(
  Indicator = blank_bold))

#OTH STRATA
Average<-c("",round(mean(OTH$Rock.Cover.Pct.First.Hit), digits=2),round(mean(OTH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(OTH$Bare.Soil.Pct), digits=2),
           "",round(mean(OTH$shrub.subshrub), digits=2), round(mean(OTH$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(OTH$succulent),digits=2),
           round(mean(OTH$tree),digits=2),round(mean(OTH$noxiousforb),digits=2),round(mean(OTH$nonnoxiousforb),digits=2),"",
           round(mean(OTH$Average.Grass.Height.cm),digits=2),round(mean(OTH$Average.Forb.Height.cm), digits=2),round(mean(OTHT$HeightWoody), digits=2),round(mean(OTHS$HeightWoody), digits=2),"",
           round(mean(OTHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(OTHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(OTHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(OTHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(OTHG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(OTHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(OTHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(OTHG$CanopyCover), digits=2),"",round(mean(OTHSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(OTH$Rock.Cover.Pct.First.Hit), digits=2),round(min(OTH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(OTH$Bare.Soil.Pct), digits=2),
           "",round(min(OTH$shrub.subshrub), digits=2), round(min(OTH$Grass.Cover.Pct.Any.Hit),digits=2),round(min(OTH$succulent),digits=2),
           round(min(OTH$tree),digits=2),round(min(OTH$noxiousforb),digits=2),round(min(OTH$nonnoxiousforb),digits=2),"",
           round(min(OTH$Average.Grass.Height.cm),digits=2),round(min(OTH$Average.Forb.Height.cm), digits=2),round(min(OTHT$HeightWoody), digits=2),round(mean(OTHS$HeightWoody), digits=2),"",
           round(min(OTHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(OTHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(OTHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(OTHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(OTHG$Gap.Cover.51.to.100.Pct), digits=2),round(min(OTHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(OTHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(OTHG$CanopyCover), digits=2),"",round(min(OTHSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(OTH$Rock.Cover.Pct.First.Hit), digits=2),round(max(OTH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(OTH$Bare.Soil.Pct), digits=2),
           "",round(max(OTH$shrub.subshrub), digits=2), round(max(OTH$Grass.Cover.Pct.Any.Hit),digits=2),round(max(OTH$succulent),digits=2),
           round(max(OTH$tree),digits=2),round(max(OTH$noxiousforb),digits=2),round(max(OTH$nonnoxiousforb),digits=2),"",
           round(max(OTH$Average.Grass.Height.cm),digits=2),round(max(OTH$Average.Forb.Height.cm), digits=2),round(max(OTHT$HeightWoody), digits=2),round(mean(OTHS$HeightWoody), digits=2),"",
           round(max(OTHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(OTHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(OTHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(OTHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(OTHG$Gap.Cover.51.to.100.Pct), digits=2),round(max(OTHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(OTHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(OTHG$CanopyCover), digits=2),"",round(max(OTHSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(OTH$Rock.Cover.Pct.First.Hit), digits=2),round(sd(OTH$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(OTH$Bare.Soil.Pct), digits=2),
           "",round(sd(OTH$shrub.subshrub), digits=2), round(sd(OTH$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(OTH$succulent),digits=2),
           round(sd(OTH$tree),digits=2),round(sd(OTH$noxiousforb),digits=2),round(sd(OTH$nonnoxiousforb),digits=2),"",
           round(sd(OTH$Average.Grass.Height.cm),digits=2),round(sd(OTH$Average.Forb.Height.cm), digits=2),round(sd(OTHT$HeightWoody), digits=2),round(mean(OTHS$HeightWoody), digits=2),"",
           round(sd(OTHA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(OTHA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(OTHA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(OTHG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(OTHG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(OTHG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(OTHG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(OTHG$CanopyCover), digits=2),"",round(sd(OTHSR$SpeciesCount), digits=2))


OTHdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(OTHdataframe, list(
  Indicator = blank_bold))

#ROF STRATA
Average<-c("",round(mean(ROF$Rock.Cover.Pct.First.Hit), digits=2),round(mean(ROF$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(ROF$Bare.Soil.Pct), digits=2),
           "",round(mean(ROF$shrub.subshrub), digits=2), round(mean(ROF$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(ROF$succulent),digits=2),
           round(mean(ROF$tree),digits=2),round(mean(ROF$noxiousforb),digits=2),round(mean(ROF$nonnoxiousforb),digits=2),"",
           round(mean(ROF$Average.Grass.Height.cm),digits=2),round(mean(ROF$Average.Forb.Height.cm), digits=2),round(mean(ROFT$HeightWoody), digits=2),round(mean(ROFS$HeightWoody), digits=2),"",
           round(mean(ROFA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(ROFA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(ROFA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(ROFG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(ROFG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(ROFG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(ROFG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(ROFG$CanopyCover), digits=2),"",round(mean(ROFSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(ROF$Rock.Cover.Pct.First.Hit), digits=2),round(min(ROF$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(ROF$Bare.Soil.Pct), digits=2),
           "",round(min(ROF$shrub.subshrub), digits=2), round(min(ROF$Grass.Cover.Pct.Any.Hit),digits=2),round(min(ROF$succulent),digits=2),
           round(min(ROF$tree),digits=2),round(min(ROF$noxiousforb),digits=2),round(min(ROF$nonnoxiousforb),digits=2),"",
           round(min(ROF$Average.Grass.Height.cm),digits=2),round(min(ROF$Average.Forb.Height.cm), digits=2),round(min(ROFT$HeightWoody), digits=2),round(mean(ROFS$HeightWoody), digits=2),"",
           round(min(ROFA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(ROFA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(ROFA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(ROFG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(ROFG$Gap.Cover.51.to.100.Pct), digits=2),round(min(ROFG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(ROFG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(ROFG$CanopyCover), digits=2),"",round(min(ROFSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(ROF$Rock.Cover.Pct.First.Hit), digits=2),round(max(ROF$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(ROF$Bare.Soil.Pct), digits=2),
           "",round(max(ROF$shrub.subshrub), digits=2), round(max(ROF$Grass.Cover.Pct.Any.Hit),digits=2),round(max(ROF$succulent),digits=2),
           round(max(ROF$tree),digits=2),round(max(ROF$noxiousforb),digits=2),round(max(ROF$nonnoxiousforb),digits=2),"",
           round(max(ROF$Average.Grass.Height.cm),digits=2),round(max(ROF$Average.Forb.Height.cm), digits=2),round(max(ROFT$HeightWoody), digits=2),round(mean(ROFS$HeightWoody), digits=2),"",
           round(max(ROFA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(ROFA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(ROFA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(ROFG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(ROFG$Gap.Cover.51.to.100.Pct), digits=2),round(max(ROFG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(ROFG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(ROFG$CanopyCover), digits=2),"",round(max(ROFSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(ROF$Rock.Cover.Pct.First.Hit), digits=2),round(sd(ROF$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(ROF$Bare.Soil.Pct), digits=2),
           "",round(sd(ROF$shrub.subshrub), digits=2), round(sd(ROF$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(ROF$succulent),digits=2),
           round(sd(ROF$tree),digits=2),round(sd(ROF$noxiousforb),digits=2),round(sd(ROF$nonnoxiousforb),digits=2),"",
           round(sd(ROF$Average.Grass.Height.cm),digits=2),round(sd(ROF$Average.Forb.Height.cm), digits=2),round(sd(ROFT$HeightWoody), digits=2),round(mean(ROFS$HeightWoody), digits=2),"",
           round(sd(ROFA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(ROFA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(ROFA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(ROFG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(ROFG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(ROFG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(ROFG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(ROFG$CanopyCover), digits=2),"",round(sd(ROFSR$SpeciesCount), digits=2))


ROFdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(ROFdataframe, list(
  Indicator = blank_bold))

#SAL STRATA
Average<-c("",round(mean(SAL$Rock.Cover.Pct.First.Hit), digits=2),round(mean(SAL$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(SAL$Bare.Soil.Pct), digits=2),
           "",round(mean(SAL$shrub.subshrub), digits=2), round(mean(SAL$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(SAL$succulent),digits=2),
           round(mean(SAL$tree),digits=2),round(mean(SAL$noxiousforb),digits=2),round(mean(SAL$nonnoxiousforb),digits=2),"",
           round(mean(SAL$Average.Grass.Height.cm),digits=2),round(mean(SAL$Average.Forb.Height.cm), digits=2),round(mean(SALT$HeightWoody), digits=2),round(mean(SALS$HeightWoody), digits=2),"",
           round(mean(SALA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(SALA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(SALA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(SALG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(SALG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(SALG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(SALG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(SALG$CanopyCover), digits=2),"",round(mean(SALSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(SAL$Rock.Cover.Pct.First.Hit), digits=2),round(min(SAL$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(SAL$Bare.Soil.Pct), digits=2),
           "",round(min(SAL$shrub.subshrub), digits=2), round(min(SAL$Grass.Cover.Pct.Any.Hit),digits=2),round(min(SAL$succulent),digits=2),
           round(min(SAL$tree),digits=2),round(min(SAL$noxiousforb),digits=2),round(min(SAL$nonnoxiousforb),digits=2),"",
           round(min(SAL$Average.Grass.Height.cm),digits=2),round(min(SAL$Average.Forb.Height.cm), digits=2),round(min(SALT$HeightWoody), digits=2),round(mean(SALS$HeightWoody), digits=2),"",
           round(min(SALA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(SALA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(SALA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(SALG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(SALG$Gap.Cover.51.to.100.Pct), digits=2),round(min(SALG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(SALG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(SALG$CanopyCover), digits=2),"",round(min(SALSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(SAL$Rock.Cover.Pct.First.Hit), digits=2),round(max(SAL$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(SAL$Bare.Soil.Pct), digits=2),
           "",round(max(SAL$shrub.subshrub), digits=2), round(max(SAL$Grass.Cover.Pct.Any.Hit),digits=2),round(max(SAL$succulent),digits=2),
           round(max(SAL$tree),digits=2),round(max(SAL$noxiousforb),digits=2),round(max(SAL$nonnoxiousforb),digits=2),"",
           round(max(SAL$Average.Grass.Height.cm),digits=2),round(max(SAL$Average.Forb.Height.cm), digits=2),round(max(SALT$HeightWoody), digits=2),round(mean(SALS$HeightWoody), digits=2),"",
           round(max(SALA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(SALA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(SALA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(SALG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(SALG$Gap.Cover.51.to.100.Pct), digits=2),round(max(SALG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(SALG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(SALG$CanopyCover), digits=2),"",round(max(SALSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(SAL$Rock.Cover.Pct.First.Hit), digits=2),round(sd(SAL$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(SAL$Bare.Soil.Pct), digits=2),
           "",round(sd(SAL$shrub.subshrub), digits=2), round(sd(SAL$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(SAL$succulent),digits=2),
           round(sd(SAL$tree),digits=2),round(sd(SAL$noxiousforb),digits=2),round(sd(SAL$nonnoxiousforb),digits=2),"",
           round(sd(SAL$Average.Grass.Height.cm),digits=2),round(sd(SAL$Average.Forb.Height.cm), digits=2),round(sd(SALT$HeightWoody), digits=2),round(mean(SALS$HeightWoody), digits=2),"",
           round(sd(SALA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(SALA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(SALA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(SALG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(SALG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(SALG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(SALG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(SALG$CanopyCover), digits=2),"",round(sd(SALSR$SpeciesCount), digits=2))


SALdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(SALdataframe, list(
  Indicator = blank_bold))

#SAN STRATA
Average<-c("",round(mean(SAN$Rock.Cover.Pct.First.Hit), digits=2),round(mean(SAN$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(SAN$Bare.Soil.Pct), digits=2),
           "",round(mean(SAN$shrub.subshrub), digits=2), round(mean(SAN$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(SAN$succulent),digits=2),
           round(mean(SAN$tree),digits=2),round(mean(SAN$noxiousforb),digits=2),round(mean(SAN$nonnoxiousforb),digits=2),"",
           round(mean(SAN$Average.Grass.Height.cm),digits=2),round(mean(SAN$Average.Forb.Height.cm), digits=2),round(mean(SANT$HeightWoody), digits=2),round(mean(SANS$HeightWoody), digits=2),"",
           round(mean(SANA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(SANA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(mean(SANA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(SANG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(SANG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(SANG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(SANG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(SANG$CanopyCover), digits=2),"",round(mean(SANSR$SpeciesCount), digits=2))

Minimum<-c("",round(min(SAN$Rock.Cover.Pct.First.Hit), digits=2),round(min(SAN$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(SAN$Bare.Soil.Pct), digits=2),
           "",round(min(SAN$shrub.subshrub), digits=2), round(min(SAN$Grass.Cover.Pct.Any.Hit),digits=2),round(min(SAN$succulent),digits=2),
           round(min(SAN$tree),digits=2),round(min(SAN$noxiousforb),digits=2),round(min(SAN$nonnoxiousforb),digits=2),"",
           round(min(SAN$Average.Grass.Height.cm),digits=2),round(min(SAN$Average.Forb.Height.cm), digits=2),round(min(SANT$HeightWoody), digits=2),round(mean(SANS$HeightWoody), digits=2),"",
           round(min(SANA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(SANA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(min(SANA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(SANG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(SANG$Gap.Cover.51.to.100.Pct), digits=2),round(min(SANG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(SANG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(SANG$CanopyCover), digits=2),"",round(min(SANSR$SpeciesCount), digits=2))

Maximum<-c("",round(max(SAN$Rock.Cover.Pct.First.Hit), digits=2),round(max(SAN$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(SAN$Bare.Soil.Pct), digits=2),
           "",round(max(SAN$shrub.subshrub), digits=2), round(max(SAN$Grass.Cover.Pct.Any.Hit),digits=2),round(max(SAN$succulent),digits=2),
           round(max(SAN$tree),digits=2),round(max(SAN$noxiousforb),digits=2),round(max(SAN$nonnoxiousforb),digits=2),"",
           round(max(SAN$Average.Grass.Height.cm),digits=2),round(max(SAN$Average.Forb.Height.cm), digits=2),round(max(SANT$HeightWoody), digits=2),round(mean(SANS$HeightWoody), digits=2),"",
           round(max(SANA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(SANA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(max(SANA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(SANG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(SANG$Gap.Cover.51.to.100.Pct), digits=2),round(max(SANG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(SANG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(SANG$CanopyCover), digits=2),"",round(max(SANSR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(SAN$Rock.Cover.Pct.First.Hit), digits=2),round(sd(SAN$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(SAN$Bare.Soil.Pct), digits=2),
           "",round(sd(SAN$shrub.subshrub), digits=2), round(sd(SAN$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(SAN$succulent),digits=2),
           round(sd(SAN$tree),digits=2),round(sd(SAN$noxiousforb),digits=2),round(sd(SAN$nonnoxiousforb),digits=2),"",
           round(sd(SAN$Average.Grass.Height.cm),digits=2),round(sd(SAN$Average.Forb.Height.cm), digits=2),round(sd(SANT$HeightWoody), digits=2),round(mean(SANS$HeightWoody), digits=2),"",
           round(sd(SANA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(SANA$Species=="insert dominant herb. name, if none=N/A"), digits=2),"",
           round(sd(SANA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(SANG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(SANG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(SANG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(SANG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(SANG$CanopyCover), digits=2),"",round(sd(SANSR$SpeciesCount), digits=2))


SANdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(SANdataframe, list(
  Indicator = blank_bold))

#FORE STRATA
Average<-c("",round(mean(FORE$Rock.Cover.Pct.First.Hit), digits=2),round(mean(FORE$Total.Litter.Cover.Pct.First.Hit), digits=2),round(mean(FORE$Bare.Soil.Pct), digits=2),
           "",round(mean(FORE$shrub.subshrub), digits=2), round(mean(FORE$Grass.Cover.Pct.Any.Hit),digits=2),round(mean(FORE$succulent),digits=2),
           round(mean(FORE$tree),digits=2),round(mean(FORE$noxiousforb),digits=2),round(mean(FORE$nonnoxiousforb),digits=2),"",
           round(mean(FORE$Average.Grass.Height.cm),digits=2),round(mean(FORE$Average.Forb.Height.cm), digits=2),round(mean(FORET$HeightWoody), digits=2),round(mean(FORES$HeightWoody), digits=2),"",
           round(mean(FOREA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(mean(FOREA$Species=="insert strata dominant woody species here, if none = N/A"), digits=2),"",
           round(mean(FOREA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(mean(FOREG$Gap.Cover.25.to.50.Pct), digits=2),
           round(mean(FOREG$Gap.Cover.51.to.100.Pct), digits=2),round(mean(FOREG$Gap.Cover.101.to.200.Pct), digits=2),
           round(mean(FOREG$Gap.Cover.200.Plus.Pct), digits=2),"",round(mean(FOREG$CanopyCover), digits=2),"",round(mean(FORESR$SpeciesCount), digits=2))

Minimum<-c("",round(min(FORE$Rock.Cover.Pct.First.Hit), digits=2),round(min(FORE$Total.Litter.Cover.Pct.First.Hit), digits=2),round(min(FORE$Bare.Soil.Pct), digits=2),
           "",round(min(FORE$shrub.subshrub), digits=2), round(min(FORE$Grass.Cover.Pct.Any.Hit),digits=2),round(min(FORE$succulent),digits=2),
           round(min(FORE$tree),digits=2),round(min(FORE$noxiousforb),digits=2),round(min(FORE$nonnoxiousforb),digits=2),"",
           round(min(FORE$Average.Grass.Height.cm),digits=2),round(min(FORE$Average.Forb.Height.cm), digits=2),round(min(FORET$HeightWoody), digits=2),round(mean(FORES$HeightWoody), digits=2),"",
           round(min(FOREA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(min(FOREA$Species=="insert strata dominant woody species here, if none = N/A"), digits=2),"",
           round(min(FOREA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(min(FOREG$Gap.Cover.25.to.50.Pct), digits=2),
           round(min(FOREG$Gap.Cover.51.to.100.Pct), digits=2),round(min(FOREG$Gap.Cover.101.to.200.Pct), digits=2),
           round(min(FOREG$Gap.Cover.200.Plus.Pct), digits=2),"",round(min(FOREG$CanopyCover), digits=2),"",round(min(FORESR$SpeciesCount), digits=2))

Maximum<-c("",round(max(FORE$Rock.Cover.Pct.First.Hit), digits=2),round(max(FORE$Total.Litter.Cover.Pct.First.Hit), digits=2),round(max(FORE$Bare.Soil.Pct), digits=2),
           "",round(max(FORE$shrub.subshrub), digits=2), round(max(FORE$Grass.Cover.Pct.Any.Hit),digits=2),round(max(FORE$succulent),digits=2),
           round(max(FORE$tree),digits=2),round(max(FORE$noxiousforb),digits=2),round(max(FORE$nonnoxiousforb),digits=2),"",
           round(max(FORE$Average.Grass.Height.cm),digits=2),round(max(FORE$Average.Forb.Height.cm), digits=2),round(max(FORET$HeightWoody), digits=2),round(mean(FORES$HeightWoody), digits=2),"",
           round(max(FOREA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(max(FOREA$Species=="insert strata dominant woody species here, if none = N/A"), digits=2),"",
           round(max(FOREA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(max(FOREG$Gap.Cover.25.to.50.Pct), digits=2),
           round(max(FOREG$Gap.Cover.51.to.100.Pct), digits=2),round(max(FOREG$Gap.Cover.101.to.200.Pct), digits=2),
           round(max(FOREG$Gap.Cover.200.Plus.Pct), digits=2),"",round(max(FOREG$CanopyCover), digits=2),"",round(max(FORESR$SpeciesCount), digits=2))

St.Dev.<-c("",round(sd(FORE$Rock.Cover.Pct.First.Hit), digits=2),round(sd(FORE$Total.Litter.Cover.Pct.First.Hit), digits=2),round(sd(FORE$Bare.Soil.Pct), digits=2),
           "",round(sd(FORE$shrub.subshrub), digits=2), round(sd(FORE$Grass.Cover.Pct.Any.Hit),digits=2),round(sd(FORE$succulent),digits=2),
           round(sd(FORE$tree),digits=2),round(sd(FORE$noxiousforb),digits=2),round(sd(FORE$nonnoxiousforb),digits=2),"",
           round(sd(FORE$Average.Grass.Height.cm),digits=2),round(sd(FORE$Average.Forb.Height.cm), digits=2),round(sd(FORET$HeightWoody), digits=2),round(mean(FORES$HeightWoody), digits=2),"",
           round(sd(FOREA$Species=="insert stratas dominant grass species, if none = N/A"), digits=2),"",round(sd(FOREA$Species=="insert strata dominant woody species here, if none = N/A"), digits=2),"",
           round(sd(FOREA$Species=="insert stratas dominant woody species, if none = N/A"), digits=2),"",round(sd(FOREG$Gap.Cover.25.to.50.Pct), digits=2),
           round(sd(FOREG$Gap.Cover.51.to.100.Pct), digits=2),round(sd(FOREG$Gap.Cover.101.to.200.Pct), digits=2),
           round(sd(FOREG$Gap.Cover.200.Plus.Pct), digits=2),"",round(sd(FOREG$CanopyCover), digits=2),"",round(sd(FORESR$SpeciesCount), digits=2))


FOREdataframe<-data.frame(Indicator,Average,Minimum,Maximum,St.Dev.)
target<-c("Surface Indicator (%)","Foliar Cover (%)","Vegetation Height (cm)","Dominant Grass Cover (%)",
          "Dominant Herb. Cover (%)","Dominant Woody Cover (%)","Canopy Gaps (%)","Canopy Cover (%)",
          "Species Richness")
blank_bold<-formatter("span", 
                      style = x ~ style("font-weight" = ifelse(x %in% target, "bold", NA)))
formattable(FOREdataframe, list(
  Indicator = blank_bold))


