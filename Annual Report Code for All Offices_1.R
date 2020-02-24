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
#below AIMdata(Query results) is the csv which I assigned strata abbreviations to


mydata<-read.csv("~/Plotdata.csv")
AIMdata<-read.csv("~/Allyears_query.csv")
Plotdata<-read.csv("~/Allyears_plots.csv")
Plotdata$Actual.Eco.Site<-mydata$Actual.Eco.Site[match(Plotdata$PrimaryKey,mydata$PrimaryKey)]
AIMdata$Actual.Eco.Site<-mydata$Actual.Eco.Site[match(AIMdata$Primary.Key,mydata$PrimaryKey)]

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
View(Plotdata)
BH<-subset(Plotdata,Actual.Eco.Site=="BH")

summary(BH$AvgPrecip)
summary(BH$Slope)
summary(BH$Elevation)
summary(BH$EcolSite)
summary(BH$LandscapeType)



#stratum description, soil pits using TerrADat Soil Horizons records
soildata<-read.csv("~/Allyears_soilhorizons.csv")

BH<-subset(Plotdata,Actual.Eco.Site=="BH")


summary(BH$Texture)
summary(BH$RockFragments)
summary(BH$Effer)
summary(BH$ESD_PctClay)



#surface cover
View(AIMdata)

AIMdata$shrub.subshrub<-AIMdata$Shrub.Cover.Pct.Any.Hit+AIMdata$Noxious.SubShrub.Cover.Pct.Any.Hit+AIMdata$NonNoxious.SubShrub.Cover.Pct.Any.Hit
AIMdata$succulent<-AIMdata$Noxious.Succulent.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Succulent.Cover.Pct.Any.Hit
AIMdata$tree<-AIMdata$Noxious.Tree.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Tree.Cover.Pct.Any.Hit
AIMdata$noxiousforb<-AIMdata$Noxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$Noxious.Perennial.Forb.Cover.Pct.Any.Hit
AIMdata$nonnoxiousforb<-AIMdata$NonNoxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Perennial.Forb.Cover.Pct.Any.Hit

#first,calculating Population Size, because it will be used throughout the entire script
BH<-subset(AIMdata,Actual.Eco.Site=="BH")

BHN<-count(BH,Primary.Key)
BHN2<-sum(BHN$n)


#continuing with calculating surface cover

AIMdata1<-AIMdata %>%
  select(Bare.Soil.Pct,Forb.Cover.Pct.Any.Hit,Grass.Cover.Pct.Any.Hit,Total.Litter.Cover.Pct.First.Hit,Rock.Cover.Pct.First.Hit,shrub.subshrub,succulent,tree,Foliar.Cover.Pct,Actual.Eco.Site)

AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)

Covers<-c("Bare Soil", "Forb","Grass", "Litter", "Rock","Shrub","Succulent","Tree","Foliar")
AIMdata2$covertype <- factor(AIMdata2$covertype, levels = c("Bare.Soil.Pct","Forb.Cover.Pct.Any.Hit","Grass.Cover.Pct.Any.Hit","Total.Litter.Cover.Pct.First.Hit","Rock.Cover.Pct.First.Hit","shrub.subshrub","succulent","tree","Foliar.Cover.Pct","Actual.Eco.Site"))

BH<-subset(AIMdata2,Actual.Eco.Site=="BH")

BHpop<-paste("BH Surface and Vegetation Cover by Strata   n=",BHN2)

SC<-ggplot(AIMdata2,aes(x=factor(covertype),y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle()+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC3<-SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = c("Bare Soil", "Forb","Grass", "Litter","Rock", "Shrub/subshrub","Succulent","Tree", "Foliar"))+theme(axis.text.x=element_text(colour="gray20"))
SC3+facet_wrap(~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))


SC<-ggplot(BH,aes(x=covertype,y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle(paste0(BHpop))+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
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
PSPSdata<-read.csv("~/PSPPALL_SLVFO.csv")
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

##dom ss boxplot
myColors<-c("black","darkgreen","sienna4")
names(myColors)<-levels(BH8$GrowthHabit)
colScale<-scale_colour_manual(name = "GrowthHabit",values = myColors)
P<-ggplot(BH8, aes(x=Species, y = AH_SpeciesCover,col = GrowthHabit))
MC<-P+geom_boxplot()
MC2<-MC+ggtitle(paste0(BHpop))+labs(x="Plant Species Code",y="Average Percent Cover",colour="Growth Form")
MC3<-MC2+facet_grid(.~GrowthHabit, scale = "free", drop= TRUE)+theme(axis.text.x=element_text(colour="gray20"))
MC3 + colScale


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

AIMdata1<-AIMdata %>%
  select(Soil.Stability.All,Soil.Stability.Protected,Soil.Stability.Unprotected,Actual.Eco.Site)


BH<-subset(AIMdata1,Actual.Eco.Site=="BH")



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

#canopy gap chart
Gapdata<-AIMdata %>%
  select(Gap.Cover.25.to.50.Pct,Gap.Cover.51.to.100.Pct,Gap.Cover.101.to.200.Pct,Gap.Cover.200.Plus.Pct,Actual.Eco.Site)
Gapdata$CanopyCover<-100-(AIMdata$Gap.Cover.25.to.50.Pct+AIMdata$Gap.Cover.51.to.100.Pct+AIMdata$Gap.Cover.101.to.200.Pct+AIMdata$Gap.Cover.200.Plus.Pct)
Gapdata$Gaps<-Gapdata$Gap.Cover.25.to.50.Pct+Gapdata$Gap.Cover.51.to.100.Pct+Gapdata$Gap.Cover.101.to.200.Pct+Gapdata$Gap.Cover.200.Plus.Pct
Gapdata2<-gather(Gapdata,Gapsize,Percentofplot,-Actual.Eco.Site)
Gapdata2$Gapsize <- factor(Gapdata2$Gapsize, levels = c("CanopyCover","Gap.Cover.25.to.50.PCt", "Gap.Cover.51.to.100.PCt", "Gap.Cover.101.to.200.PCt","Gap.Cover.200.Plus.PCt"))

BHG<-subset(Gapdata,Actual.Eco.Site=="BH")

summary(BHG$Gaps)
summary(BHG$CanopyCover)
sd(BHG$CanopyCover)


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

GC<-ggplot(BHG2,aes(x="",y=Percentofplot,fill=Gapsize))
GC2<-GC+geom_bar(width=0.5,position="fill", stat = "identity")+ggtitle(paste0(BHpop))+coord_flip()
GC3<-GC2+scale_fill_manual(values=c("cadetblue3", "khaki1","sienna1","firebrick3","firebrick4"),labels=c("Canopy Cover (No Gap)","25-50 (cm)","51-100 (cm)", "101-200 (cm)", "200+ (cm)")) 
GC3+theme(axis.title.y = element_blank())+labs(y="Percent of Plot")+scale_y_continuous(labels=scales::percent)


#surface indicator summary tables, sagebrush height not included, tree and succulent height can be calculated using LPI detail

#SPECIES RICHNESS
SPdata<-read.csv("~/Allyears_species_rich.csv")
View(SPdata)
BHSR<-subset(SPdata,Actual.Eco.Site=="BH")

BH<-subset(AIMdata,Actual.Eco.Site=="BH")

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



