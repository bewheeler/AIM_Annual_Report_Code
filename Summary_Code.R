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
#native vs nonnative
target<-c("BH","LB","LOA","MO","OTH","ROF","SAL","SAN","FORE")

#soil stability
AIMdata1<-AIMdata %>%
  select(Soil.Stability.All,Soil.Stability.Protected,Soil.Stability.Unprotected,Actual.Eco.Site) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_soil_stability=mean(Soil.Stability.All)) %>%
  arrange(desc(avg_soil_stability))
AIMdata1<-AIMdata1[!duplicated(AIMdata1$avg_soil_stability),]
formattable(AIMdata1)

#surface cover
View(AIMdata)

AIMdata$shrub.subshrub<-AIMdata$Shrub.Cover.Pct.Any.Hit+AIMdata$Noxious.SubShrub.Cover.Pct.Any.Hit+AIMdata$NonNoxious.SubShrub.Cover.Pct.Any.Hit
AIMdata$succulent<-AIMdata$Noxious.Succulent.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Succulent.Cover.Pct.Any.Hit
AIMdata$tree<-AIMdata$Noxious.Tree.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Tree.Cover.Pct.Any.Hit
AIMdata$noxiousforb<-AIMdata$Noxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$Noxious.Perennial.Forb.Cover.Pct.Any.Hit
AIMdata$nonnoxiousforb<-AIMdata$NonNoxious.Annual.Forb.Cover.Pct.Any.Hit+AIMdata$NonNoxious.Perennial.Forb.Cover.Pct.Any.Hit

AIMdata1<-AIMdata %>%
  select(Bare.Soil.Pct,Forb.Cover.Pct.Any.Hit,Grass.Cover.Pct.Any.Hit,Total.Litter.Cover.Pct.First.Hit,Rock.Cover.Pct.First.Hit,shrub.subshrub,succulent,tree,Foliar.Cover.Pct,Actual.Eco.Site)

AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site) %>%
  filter(Actual.Eco.Site %in% target)

Covers<-c("Bare Soil", "Forb","Grass", "Litter", "Rock","Shrub","Succulent","Tree","Foliar")
AIMdata2$covertype <- factor(AIMdata2$covertype, levels = c("Bare.Soil.Pct","Forb.Cover.Pct.Any.Hit","Grass.Cover.Pct.Any.Hit","Total.Litter.Cover.Pct.First.Hit","Rock.Cover.Pct.First.Hit","shrub.subshrub","succulent","tree","Foliar.Cover.Pct","Actual.Eco.Site"))

SC<-ggplot(AIMdata2,aes(x=factor(covertype),y=covervalue,col=covertype))+geom_point(alpha=0.4)
SC2<-SC+geom_boxplot()+ggtitle("Surface and Vegetation Cover by Strata")+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
SC3<-SC2+scale_x_discrete(labels= Covers)+scale_color_hue(labels = c("Bare Soil", "Forb","Grass", "Litter","Rock", "Shrub/subshrub","Succulent","Tree", "Foliar"))+theme(axis.text.x=element_text(colour="gray20"))
SC3+facet_wrap(~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

#bare soil cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,Bare.Soil.Pct) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_baresoil=mean(Bare.Soil.Pct)) %>%
  select(Actual.Eco.Site,avg_baresoil) %>%
  arrange(desc(avg_baresoil))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_baresoil),]
formattable(AIMdata3)

#forb cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,Forb.Cover.Pct.Any.Hit) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_forb=mean(Forb.Cover.Pct.Any.Hit)) %>%
  select(Actual.Eco.Site,avg_forb) %>%
  arrange(desc(avg_forb))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_forb),]
formattable(AIMdata3)

#grass cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,Grass.Cover.Pct.Any.Hit) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_grass=mean(Grass.Cover.Pct.Any.Hit)) %>%
  select(Actual.Eco.Site,avg_grass) %>%
  arrange(desc(avg_grass))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_grass),]
formattable(AIMdata3)

#litter cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,Total.Litter.Cover.Pct.First.Hit) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_litter=mean(Total.Litter.Cover.Pct.First.Hit)) %>%
  select(Actual.Eco.Site,avg_litter) %>%
  arrange(desc(avg_litter))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_litter),]
formattable(AIMdata3)

#rock cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,Rock.Cover.Pct.First.Hit) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_rock=mean(Rock.Cover.Pct.First.Hit)) %>%
  select(Actual.Eco.Site,avg_rock) %>%
  arrange(desc(avg_rock))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_rock),]
formattable(AIMdata3)

#shrub cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,shrub.subshrub) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_shrub=mean(shrub.subshrub)) %>%
  select(Actual.Eco.Site,avg_shrub) %>%
  arrange(desc(avg_shrub))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_shrub),]
formattable(AIMdata3)

#succulent cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,succulent) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_succulent=mean(succulent)) %>%
  select(Actual.Eco.Site,avg_succulent) %>%
  arrange(desc(avg_succulent))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_succulent),]
formattable(AIMdata3)

#tree cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,tree) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_tree=mean(tree)) %>%
  select(Actual.Eco.Site,avg_tree) %>%
  arrange(desc(avg_tree))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_tree),]
formattable(AIMdata3)

#foliar cover
AIMdata3<- AIMdata1 %>%
  select(Actual.Eco.Site,Foliar.Cover.Pct) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_foliar=mean(Foliar.Cover.Pct)) %>%
  select(Actual.Eco.Site,avg_foliar) %>%
  arrange(desc(avg_foliar))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_foliar),]
formattable(AIMdata3)

#native vs nonnative
target<-c("BH","FORE","LB","LOA","MO","OTH","ROF","SAL","SAN")
AIMdata1<-AIMdata%>%
  select(Noxious.Cover.Pct.Any.Hit,NonNoxious.Plant.Cover.Pct.Any.Hit,Actual.Eco.Site) %>%
  filter(Actual.Eco.Site %in% target)

AIMdata2<-gather(AIMdata1,covertype,covervalue,-Actual.Eco.Site)
head(AIMdata2)

Covers<-c("Nonnox.", "Noxious")
NN<-ggplot(AIMdata2,aes(x=covertype,y=covervalue, col=covertype))+geom_point(alpha=0.4)
NN2<-NN+geom_boxplot()
NN3<-NN2+ggtitle("Noxious and Nonnoxious Species Comparison by Strata")+labs(x="Cover Type",y="Average Percent Cover",colour="Cover Type")
NN4<-NN3+scale_x_discrete(labels= Covers)+scale_color_manual(values=c("seagreen2","darkred"),labels = c("Nonnoxious", "Noxious"))+theme(axis.text.x=element_text(colour="gray20"))
NN4+facet_wrap(~Actual.Eco.Site, ncol=3)+theme(axis.text.x=element_text(colour="gray20",angle = 45, hjust=1))

AIMdata1<- AIMdata1 %>%
  select(Actual.Eco.Site,Noxious.Cover.Pct.Any.Hit, NonNoxious.Plant.Cover.Pct.Any.Hit) %>%
  group_by(Actual.Eco.Site) %>%
  mutate(avg_nox=mean(Noxious.Cover.Pct.Any.Hit)) %>%
  mutate(avg_nonox=mean(NonNoxious.Plant.Cover.Pct.Any.Hit))

AIMdata3<-AIMdata1 %>%
  select(Actual.Eco.Site,avg_nox) %>%
  arrange(desc(avg_nox))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_nox),]
formattable(AIMdata3)

AIMdata3<- AIMdata1 %>%
select(Actual.Eco.Site,avg_nonox) %>%
  arrange(desc(avg_nonox))
AIMdata3<-AIMdata3[!duplicated(AIMdata3$avg_nonox),]
formattable(AIMdata3)

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

#canopy gap chart
Gapdata<-AIMdata %>%
  select(Gap.Cover.25.to.50.Pct,Gap.Cover.51.to.100.Pct,Gap.Cover.101.to.200.Pct,Gap.Cover.200.Plus.Pct,Actual.Eco.Site)
Gapdata$CanopyCover<-100-(AIMdata$Gap.Cover.25.to.50.Pct+AIMdata$Gap.Cover.51.to.100.Pct+AIMdata$Gap.Cover.101.to.200.Pct+AIMdata$Gap.Cover.200.Plus.Pct)
Gapdata$Gaps<-Gapdata$Gap.Cover.25.to.50.Pct+Gapdata$Gap.Cover.51.to.100.Pct+Gapdata$Gap.Cover.101.to.200.Pct+Gapdata$Gap.Cover.200.Plus.Pct

Gapdatag<- Gapdata %>%
  group_by(Actual.Eco.Site) %>%
  select(Actual.Eco.Site,Gaps) %>%
  mutate(avg_gaps=mean(Gaps)) %>%
  select(Actual.Eco.Site,avg_gaps) %>%
  arrange(desc(avg_gaps))
Gapdatag<-Gapdatag[!duplicated(Gapdatag$avg_gaps),]
formattable(Gapdatag)

Gapdatac<- Gapdata %>%
  group_by(Actual.Eco.Site) %>%
  select(Actual.Eco.Site, CanopyCover) %>%
  mutate(avg_canopy=mean(CanopyCover)) %>%
  select(Actual.Eco.Site,avg_canopy) %>%
  arrange(desc(avg_canopy))
Gapdatac<-Gapdatac[!duplicated(Gapdatac$avg_canopy),]
formattable(Gapdatac)
