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
domss<-c("BOGR2","HECO26","PIMI","MUMO")
OTHA<-OTH %>%
  filter(Species %in% domss)
OTH6<-OTHA %>%
  select(Species,AH_SpeciesCover,PrimaryKey,GrowthHabit)
OTH7<-as.data.frame(OTH6)
#(species name, zero #)
A<-rep("BOGR2",10)
B<-rep("HECO26",15)
C<-rep("PIMI",15)
D<-rep("MUMO",11)
#(growthhabit, zero #)
A2<-rep("NonWoody",10)
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
