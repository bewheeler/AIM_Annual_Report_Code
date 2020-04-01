library(tidyverse)
library(dplyr)
library(ggrepel)
library(formattable)

benchtool<-read.csv("~/Allyears_query.csv")
plots<-read.csv("~/Allyears_plots.csv")
benchtool$Actual.Eco.Site<-plots$Actual.Eco.Site[match(benchtool$Primary.Key,plots$PrimaryKey)]
benchtool$plotID<-plots$PlotID[match(benchtool$Primary.Key,plots$PrimaryKey)]
benchtool$Allotment<-plots$Allotment.Name[match(benchtool$Primary.Key,plots$PrimaryKey)]
View(benchtool)


bench<-benchtool[-c(2:148)]
bench$baresoil<-benchtool$Bare.Soil.Pct
bench$soilstability<-benchtool$Soil.Stability.All
bench$litter<-benchtool$Total.Litter.Cover.Pct.First.Hit
bench$native<-benchtool$NonNoxious.Plant.Cover.Pct.Any.Hit
bench$grasscover<-benchtool$Grass.Cover.Pct.Any.Hit
bench$forbcover<-benchtool$Forb.Cover.Pct.Any.Hit
bench$shrubcover<-benchtool$Shrub.Cover.Pct.Any.Hit
bench$treecover<-benchtool$Noxious.Tree.Cover.Pct.Any.Hit+benchtool$NonNoxious.Tree.Cover.Pct.Any.Hit
bench$sagecover<-benchtool$Sagebrush.Cover.Pct.Any.Hit
bench$nonsagecover<-benchtool$NonSagebrush.Shrub.Cover.Pct.Any.Hit
bench$grassheight<-benchtool$Average.Grass.Height.cm
bench$sageheight<-benchtool$Average.Sagebrush.Height.cm
bench$forb.height<-benchtool$Average.Forb.Height.cm
bench$nonnative<-benchtool$Noxious.Cover.Pct.Any.Hit
bench$forbnumber<-benchtool$Number.Preferred.Forb.Species
bench$Actual.Eco.Site<-benchtool$Actual.Eco.Site
bench$rock<-benchtool$Rock.Cover.Pct.First.Hit
bench$succulent<-benchtool$NonNoxious.Succulent.Cover.Pct.Any.Hit+benchtool$Noxious.Succulent.Cover.Pct.Any.Hit
bench$gaps<-benchtool$Gap.Cover.25.Plus.Pct
bench$plotID<-benchtool$plotID
bench$allotment<-benchtool$Allotment

#PV
PV<-subset(bench,Actual.Eco.Site=="PV")
PVmean<-mean(na.omit(PV$baresoil))
PVsd<-sd(na.omit(PV$baresoil))
breaks<-c(-Inf,unique(c(PVmean-(2*PVsd),PVmean+(2*PVsd))),Inf)
tags<-c("0","1","0")
PV$BSgroup_tags<-cut(PV$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$BSgroup_tags)
PV$BSgroup_tags<-as.numeric(as.character(PV$BSgroup_tags))

PV<-PV %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

PVSSmean<-mean(na.omit(PV$soilstability))
PVSSsd<-sd(na.omit(PV$soilstability))
breaks<-c(-Inf,unique(c(PVSSmean-(2*PVSSsd),PVSSmean+(2*PVSSsd))),Inf)
breaks
tags<-c("0","1","0")
PV$SSgroup_tags<-cut(PV$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$SSgroup_tags)
PV$SSgroup_tags<-as.numeric(as.character(PV$SSgroup_tags))

PV<-PV %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

PVNmean<-mean(na.omit(PV$native))
PVNsd<-sd(na.omit(PV$native))
breaks<-c(-Inf,unique(c(PVNmean-(2*PVNsd),PVNmean+(2*PVNsd))),Inf)
tags<-c("0","1","0")
PV$Ngroup_tags<-cut(PV$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Ngroup_tags)
PV$Ngroup_tags<-as.numeric(as.character(PV$Ngroup_tags))

PV<-PV %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


PVGmean<-mean(na.omit(PV$grasscover))
PVGsd<-sd(na.omit(PV$grasscover))
breaks<-c(-Inf,unique(c(PVGmean-(2*PVGsd),PVGmean+(2*PVGsd))),Inf)
tags<-c("0","1","0")
PV$Ggroup_tags<-cut(PV$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Ggroup_tags)
PV$Ggroup_tags<-as.numeric(as.character(PV$Ggroup_tags))

PV<-PV %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


PVFmean<-mean(na.omit(PV$forbcover))
PVFsd<-sd(na.omit(PV$forbcover))
breaks<-c(-Inf,unique(c(PVFmean-(2*PVFsd),PVFmean+(2*PVFsd))),Inf)
tags<-c("0","1","0")
PV$Fgroup_tags<-cut(PV$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Fgroup_tags)
PV$Fgroup_tags<-as.numeric(as.character(PV$Fgroup_tags))

PV<-PV %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

PVSmean<-mean(na.omit(PV$shrubcover))
PVSsd<-sd(na.omit(PV$shrubcover))
breaks<-c(-Inf,unique(c(PVSmean-(2*PVSsd),PVSmean+(2*PVSsd))),Inf)
tags<-c("0","1","0")
PV$Sgroup_tags<-cut(PV$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Sgroup_tags)
PV$Sgroup_tags<-as.numeric(as.character(PV$Sgroup_tags))

PV<-PV %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

PVTmean<-mean(na.omit(PV$treecover))
PVTsd<-sd(na.omit(PV$treecover))
breaks<-c(-Inf,unique(c(PVTmean-(2*PVTsd),PVTmean+(2*PVTsd))),Inf)
tags<-c("0","1","0")
PV$Tgroup_tags<-cut(PV$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Tgroup_tags)
PV$Tgroup_tags<-as.numeric(as.character(PV$Tgroup_tags))

PV<-PV %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

PVGHmean<-mean(na.omit(PV$grassheight))
PVGHsd<-sd(na.omit(PV$grassheight))
breaks<-c(-Inf,PVGHmean-PVGHsd,PVGHmean+PVGHsd,Inf)
tags<-c("0","1","0")
PV$GHgroup_tags<-cut(PV$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$GHgroup_tags)
PV$GHgroup_tags<-as.numeric(as.character(PV$GHgroup_tags))

PV<-PV %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

PVFHmean<-mean(na.omit(PV$forb.height))
PVFHsd<-sd(na.omit(PV$forb.height))
breaks<-c(-Inf,unique(c(PVFHmean-(2*PVFHsd),PVFHmean+(2*PVFHsd))),Inf)
tags<-c("0","1","0")
PV$FHgroup_tags<-cut(PV$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$FHgroup_tags)
PV$FHgroup_tags<-as.numeric(as.character(PV$FHgroup_tags))

PV<-PV %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
PVNNmean<-mean(na.omit(PV$nonnative))
PVNNsd<-sd(na.omit(PV$nonnative))
breaks<-c(-Inf,unique(PVNNmean-PVNNsd,PVNNmean+PVNNsd),Inf)
breaks
tags<-c("0","1")
PV$NNgroup_tags<-cut(PV$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$NNgroup_tags)
PV$NNgroup_tags<-as.numeric(as.character(PV$NNgroup_tags))

PV<-PV %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

PVFNmean<-mean(na.omit(PV$forbnumber))
PVFNsd<-sd(na.omit(PV$forbnumber))
breaks<-c(-Inf,unique(c(PVFNmean-(2*PVFNsd),PVFNmean+(2*PVFNsd))),Inf)
tags<-c("0","1","0")
PV$FNgroup_tags<-cut(PV$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$FNgroup_tags)
PV$FNgroup_tags<-as.numeric(as.character(PV$FNgroup_tags))
PV<-PV %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

PVRmean<-mean(na.omit(PV$rock))
PVRsd<-sd(na.omit(PV$rock))
breaks<-c(-Inf,unique(c(PVRmean-(2*PVRsd),PVRmean+(2*PVRsd))),Inf)
tags<-c("0","1","0")
PV$Rgroup_tags<-cut(PV$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Rgroup_tags)
PV$Rgroup_tags<-as.numeric(as.character(PV$Rgroup_tags))

PV<-PV %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

PVLmean<-mean(na.omit(PV$litter))
PVLsd<-sd(na.omit(PV$litter))
breaks<-c(-Inf,unique(c(PVLmean-(2*PVLsd),PVLmean+(2*PVLsd))),Inf)
tags<-c("0","1","0")
PV$Lgroup_tags<-cut(PV$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$Lgroup_tags)
PV$Lgroup_tags<-as.numeric(as.character(PV$Lgroup_tags))

PV<-PV %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


PVSUmean<-mean(na.omit(PV$succulent))
PVSUsd<-sd(na.omit(PV$succulent))
breaks<-c(-Inf,unique(c(PVSUmean-(2*PVSUsd),PVSUmean+(2*PVSUsd))),Inf)
tags<-c("0","1","0")
PV$SUgroup_tags<-cut(PV$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$SUgroup_tags)
PV$SUgroup_tags<-as.numeric(as.character(PV$SUgroup_tags))

PV<-PV %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

PVGAmean<-mean(na.omit(PV$gaps))
PVGAsd<-sd(na.omit(PV$gaps))
breaks<-c(-Inf,unique(c(PVGAmean-(2*PVGAsd),PVGAmean+(2*PVGAsd))),Inf)
tags<-c("0","1","0")
PV$GAgroup_tags<-cut(PV$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$GAgroup_tags)
PV$GAgroup_tags<-as.numeric(as.character(PV$GAgroup_tags))

PV<-PV %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
PVL<-PV %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
PVL<-gather(PVL,indicator,value,-plotID,na.rm=TRUE)
PVL<- PVL %>%
  group_by(plotID) %>%
  mutate(sum=n())
PVGraph<-PV
PVGraph$sum<-PVL$sum[match(PVGraph$plotID,PVL$plotID)]
PVGraph[is.na(PVGraph)] <- 0
PVGraph<-PVGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
PVGraph$list<-(1:length(PVGraph$graph))

PVB<-ggplot(PVGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-PVB+ggtitle("Plots within expected range PV")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
PV2<-PV
PV2$graph<-as.numeric(PVGraph$graph)
PV2$graph<-round(PV2$graph,digits=2)
PV3<-PV2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
PV3<-PV3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(PV3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(PV3)
write.csv(PV3,"PVIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
PVA1<-PV
PV2$graph<-as.numeric(PVGraph$graph)
PVA1$graph<-round(PV2$graph,digits=2)
PVA<-PVA1 %>%
  select(plotID,allotment,graph)
PVA<-PVA%>%
  arrange(allotment)
names(PVA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(PVA)

write.csv(PVA,"PVallotmentspercentmeeting.csv")

#REGUSGWVE DUPLICATES
PV$baresoil.bin<-PV$baresoil.bin*100
PV$soilstabilitybin<-PV$soilstabilitybin*100
PV$treecoverbin<-PV$treecoverbin*100
PV$shrubcoverbin<-PV$shrubcoverbin*100
PV$grasscoverbin<-PV$grasscoverbin*100
PV$grassheightbin<-PV$grassheightbin*100
PV$forb.heightbin<-PV$forb.heightbin*100
PV$forbcoverbin<-PV$forbcoverbin*100
PV$gapsbin<-PV$gapsbin*100
PV$nonnativebin<-PV$nonnativebin*100
PV$nativebin<-PV$nativebin*100
PV$succulentbin<-PV$succulentbin*100
PV$litterbin<-PV$litterbin*100
PV$rockbin<-PV$rockbin*100
PV$forbnumberbin<-PV$forbnumberbin*100
PV$graph<-PVGraph$graph[match(PV$plotID,PVGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
PV$SUMgroup_tags<-cut(PV$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(PV$SUMgroup_tags)
PV$SUMgroup_tags<-as.numeric(as.character(PV$SUMgroup_tags))

PV<-PV %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
PVR<-PV
PV<-PV[!duplicated(PV$nativebin),]
stratablanks<-rep("",15)
Strata<-c("PV",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
              "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
              "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(PV$baresoil.bin,digits=2),round(PV$soilstabilitybin,digits=2),round(PV$litterbin,digits=2),round(PV$nativebin,digits=2),round(PV$grasscoverbin,digits=2),round(PV$forbcoverbin,digits=2),
                   round(PV$shrubcoverbin,digits=2),round(PV$nonnativebin,digits=2),round(PV$forbnumberbin,digits=2),round(PV$rockbin,digits=2),
                   round(PV$treecoverbin,digits=2),round(PV$succulentbin,digits=2),round(PV$gapsbin,digits=2),round(PV$forb.heightbin,digits=2),round(PV$grassheightbin,digits=2),round(PV$SUMbin,digits=2))
PVdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(PVdataframe)
#if you want a csv
write.csv(PVdataframe, "PVPercentofIndicatorsMeeting.csv")

#GUSGO
GUSGO<-subset(bench,Actual.Eco.Site=="GUSGO")
GUSGOmean<-mean(na.omit(GUSGO$baresoil))
GUSGOsd<-sd(na.omit(GUSGO$baresoil))
breaks<-c(-Inf,unique(c(GUSGOmean-(2*GUSGOsd),GUSGOmean+(2*GUSGOsd))),Inf)
tags<-c("0","1","0")
GUSGO$BSgroup_tags<-cut(GUSGO$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$BSgroup_tags)
GUSGO$BSgroup_tags<-as.numeric(as.character(GUSGO$BSgroup_tags))

GUSGO<-GUSGO %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

GUSGOSSmean<-mean(na.omit(GUSGO$soilstability))
GUSGOSSsd<-sd(na.omit(GUSGO$soilstability))
breaks<-c(-Inf,unique(c(GUSGOSSmean-(2*GUSGOSSsd),GUSGOSSmean+(2*GUSGOSSsd))),Inf)
breaks
tags<-c("0","1","0")
GUSGO$SSgroup_tags<-cut(GUSGO$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$SSgroup_tags)
GUSGO$SSgroup_tags<-as.numeric(as.character(GUSGO$SSgroup_tags))

GUSGO<-GUSGO %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

GUSGONmean<-mean(na.omit(GUSGO$native))
GUSGONsd<-sd(na.omit(GUSGO$native))
breaks<-c(-Inf,unique(c(GUSGONmean-(2*GUSGONsd),GUSGONmean+(2*GUSGONsd))),Inf)
tags<-c("0","1","0")
GUSGO$Ngroup_tags<-cut(GUSGO$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Ngroup_tags)
GUSGO$Ngroup_tags<-as.numeric(as.character(GUSGO$Ngroup_tags))

GUSGO<-GUSGO %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


GUSGOGmean<-mean(na.omit(GUSGO$grasscover))
GUSGOGsd<-sd(na.omit(GUSGO$grasscover))
breaks<-c(-Inf,unique(c(GUSGOGmean-(2*GUSGOGsd),GUSGOGmean+(2*GUSGOGsd))),Inf)
tags<-c("0","1","0")
GUSGO$Ggroup_tags<-cut(GUSGO$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Ggroup_tags)
GUSGO$Ggroup_tags<-as.numeric(as.character(GUSGO$Ggroup_tags))

GUSGO<-GUSGO %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


GUSGOFmean<-mean(na.omit(GUSGO$forbcover))
GUSGOFsd<-sd(na.omit(GUSGO$forbcover))
breaks<-c(-Inf,unique(c(GUSGOFmean-(2*GUSGOFsd),GUSGOFmean+(2*GUSGOFsd))),Inf)
tags<-c("0","1","0")
GUSGO$Fgroup_tags<-cut(GUSGO$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Fgroup_tags)
GUSGO$Fgroup_tags<-as.numeric(as.character(GUSGO$Fgroup_tags))

GUSGO<-GUSGO %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

GUSGOSmean<-mean(na.omit(GUSGO$shrubcover))
GUSGOSsd<-sd(na.omit(GUSGO$shrubcover))
breaks<-c(-Inf,unique(c(GUSGOSmean-(2*GUSGOSsd),GUSGOSmean+(2*GUSGOSsd))),Inf)
tags<-c("0","1","0")
GUSGO$Sgroup_tags<-cut(GUSGO$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Sgroup_tags)
GUSGO$Sgroup_tags<-as.numeric(as.character(GUSGO$Sgroup_tags))

GUSGO<-GUSGO %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

GUSGOTmean<-mean(na.omit(GUSGO$treecover))
GUSGOTsd<-sd(na.omit(GUSGO$treecover))
breaks<-c(-Inf,unique(c(GUSGOTmean-(2*GUSGOTsd),GUSGOTmean+(2*GUSGOTsd))),Inf)
tags<-c("0","1")
GUSGO$Tgroup_tags<-cut(GUSGO$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Tgroup_tags)
GUSGO$Tgroup_tags<-as.numeric(as.character(GUSGO$Tgroup_tags))

GUSGO<-GUSGO %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

GUSGOGHmean<-mean(na.omit(GUSGO$grassheight))
GUSGOGHsd<-sd(na.omit(GUSGO$grassheight))
breaks<-c(-Inf,GUSGOGHmean-GUSGOGHsd,GUSGOGHmean+GUSGOGHsd,Inf)
tags<-c("0","1","0")
GUSGO$GHgroup_tags<-cut(GUSGO$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$GHgroup_tags)
GUSGO$GHgroup_tags<-as.numeric(as.character(GUSGO$GHgroup_tags))

GUSGO<-GUSGO %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

GUSGOFHmean<-mean(na.omit(GUSGO$forb.height))
GUSGOFHsd<-sd(na.omit(GUSGO$forb.height))
breaks<-c(-Inf,unique(c(GUSGOFHmean-(2*GUSGOFHsd),GUSGOFHmean+(2*GUSGOFHsd))),Inf)
tags<-c("0","1","0")
GUSGO$FHgroup_tags<-cut(GUSGO$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$FHgroup_tags)
GUSGO$FHgroup_tags<-as.numeric(as.character(GUSGO$FHgroup_tags))

GUSGO<-GUSGO %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
GUSGONNmean<-mean(na.omit(GUSGO$nonnative))
GUSGONNsd<-sd(na.omit(GUSGO$nonnative))
breaks<-c(-Inf,unique(GUSGONNmean-GUSGONNsd,GUSGONNmean+GUSGONNsd),Inf)
breaks
tags<-c("0","1")
GUSGO$NNgroup_tags<-cut(GUSGO$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$NNgroup_tags)
GUSGO$NNgroup_tags<-as.numeric(as.character(GUSGO$NNgroup_tags))

GUSGO<-GUSGO %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

GUSGOFNmean<-mean(na.omit(GUSGO$forbnumber))
GUSGOFNsd<-sd(na.omit(GUSGO$forbnumber))
breaks<-c(-Inf,unique(c(GUSGOFNmean-(2*GUSGOFNsd),GUSGOFNmean+(2*GUSGOFNsd))),Inf)
tags<-c("0","1","0")
GUSGO$FNgroup_tags<-cut(GUSGO$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$FNgroup_tags)
GUSGO$FNgroup_tags<-as.numeric(as.character(GUSGO$FNgroup_tags))
GUSGO<-GUSGO %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

GUSGORmean<-mean(na.omit(GUSGO$rock))
GUSGORsd<-sd(na.omit(GUSGO$rock))
breaks<-c(-Inf,unique(c(GUSGORmean-(2*GUSGORsd),GUSGORmean+(2*GUSGORsd))),Inf)
tags<-c("0","1","0")
GUSGO$Rgroup_tags<-cut(GUSGO$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Rgroup_tags)
GUSGO$Rgroup_tags<-as.numeric(as.character(GUSGO$Rgroup_tags))

GUSGO<-GUSGO %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

GUSGOLmean<-mean(na.omit(GUSGO$litter))
GUSGOLsd<-sd(na.omit(GUSGO$litter))
breaks<-c(-Inf,unique(c(GUSGOLmean-(2*GUSGOLsd),GUSGOLmean+(2*GUSGOLsd))),Inf)
tags<-c("0","1","0")
GUSGO$Lgroup_tags<-cut(GUSGO$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$Lgroup_tags)
GUSGO$Lgroup_tags<-as.numeric(as.character(GUSGO$Lgroup_tags))

GUSGO<-GUSGO %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


GUSGOSUmean<-mean(na.omit(GUSGO$succulent))
GUSGOSUsd<-sd(na.omit(GUSGO$succulent))
breaks<-c(-Inf,unique(c(GUSGOSUmean-(2*GUSGOSUsd),GUSGOSUmean+(2*GUSGOSUsd))),Inf)
tags<-c("0","1")
GUSGO$SUgroup_tags<-cut(GUSGO$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$SUgroup_tags)
GUSGO$SUgroup_tags<-as.numeric(as.character(GUSGO$SUgroup_tags))

GUSGO<-GUSGO %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

GUSGOGAmean<-mean(na.omit(GUSGO$gaps))
GUSGOGAsd<-sd(na.omit(GUSGO$gaps))
breaks<-c(-Inf,unique(c(GUSGOGAmean-(2*GUSGOGAsd),GUSGOGAmean+(2*GUSGOGAsd))),Inf)
tags<-c("0","1","0")
GUSGO$GAgroup_tags<-cut(GUSGO$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$GAgroup_tags)
GUSGO$GAgroup_tags<-as.numeric(as.character(GUSGO$GAgroup_tags))

GUSGO<-GUSGO %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
GUSGOL<-GUSGO %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
GUSGOL<-gather(GUSGOL,indicator,value,-plotID,na.rm=TRUE)
GUSGOL<- GUSGOL %>%
  group_by(plotID) %>%
  mutate(sum=n())
GUSGOGraph<-GUSGO
GUSGOGraph$sum<-GUSGOL$sum[match(GUSGOGraph$plotID,GUSGOL$plotID)]
GUSGOGraph[is.na(GUSGOGraph)] <- 0
GUSGOGraph<-GUSGOGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
GUSGOGraph$list<-(1:length(GUSGOGraph$graph))

GUSGOB<-ggplot(GUSGOGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-GUSGOB+ggtitle("Plots within expected range GUSGO")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
GUSGO2<-GUSGO
GUSGO2$graph<-GUSGOGraph$graph[match(GUSGO2$plotID,GUSGOGraph$plotID)]
GUSGO2$graph<-as.numeric(GUSGO2$graph)
GUSGO2$graph<-round(GUSGO2$graph,digits=2)
GUSGO3<-GUSGO2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
GUSGO3<-GUSGO3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(GUSGO3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(GUSGO3)
write.csv(GUSGO3,"GUSGOIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
GUSGOA1<-GUSGO
GUSGO2$graph<-as.numeric(GUSGOGraph$graph)
GUSGOA1$graph<-round(GUSGO2$graph,digits=2)
GUSGOA<-GUSGOA1 %>%
  select(plotID,allotment,graph)
GUSGOA<-GUSGOA%>%
  arrange(allotment)
names(GUSGOA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(GUSGOA)

write.csv(GUSGOA,"GUSGOallotmentspercentmeeting.csv")
#number of plots within allotment meeting benchmark

#REGUSGWVE DUPLICATES

GUSGO$baresoil.bin<-GUSGO$baresoil.bin*100
GUSGO$soilstabilitybin<-GUSGO$soilstabilitybin*100
GUSGO$treecoverbin<-GUSGO$treecoverbin*100
GUSGO$shrubcoverbin<-GUSGO$shrubcoverbin*100
GUSGO$grasscoverbin<-GUSGO$grasscoverbin*100
GUSGO$grassheightbin<-GUSGO$grassheightbin*100
GUSGO$forb.heightbin<-GUSGO$forb.heightbin*100
GUSGO$forbcoverbin<-GUSGO$forbcoverbin*100
GUSGO$gapsbin<-GUSGO$gapsbin*100
GUSGO$nonnativebin<-GUSGO$nonnativebin*100
GUSGO$nativebin<-GUSGO$nativebin*100
GUSGO$succulentbin<-GUSGO$succulentbin*100
GUSGO$litterbin<-GUSGO$litterbin*100
GUSGO$rockbin<-GUSGO$rockbin*100
GUSGO$forbnumberbin<-GUSGO$forbnumberbin*100
GUSGO$graph<-GUSGOGraph$graph[match(GUSGO$plotID,GUSGOGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
GUSGO$SUMgroup_tags<-cut(GUSGO$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGO$SUMgroup_tags)
GUSGO$SUMgroup_tags<-as.numeric(as.character(GUSGO$SUMgroup_tags))

GUSGO<-GUSGO %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
GUSGOR<-GUSGO
GUSGO<-GUSGO[!duplicated(GUSGO$nativebin),]
stratablanks<-rep("",15)
Strata<-c("GUSGO",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
              "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
              "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(GUSGO$baresoil.bin,digits=2),round(GUSGO$soilstabilitybin,digits=2),round(GUSGO$litterbin,digits=2),round(GUSGO$nativebin,digits=2),round(GUSGO$grasscoverbin,digits=2),round(GUSGO$forbcoverbin,digits=2),
                   round(GUSGO$shrubcoverbin,digits=2),round(GUSGO$nonnativebin,digits=2),round(GUSGO$forbnumberbin,digits=2),round(GUSGO$rockbin,digits=2),
                   round(GUSGO$treecoverbin,digits=2),round(GUSGO$succulentbin,digits=2),round(GUSGO$gapsbin,digits=2),round(GUSGO$forb.heightbin,digits=2),round(GUSGO$grassheightbin,digits=2),round(GUSGO$SUMbin,digits=2))
GUSGOdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(GUSGOdataframe)
#if you want a csv
write.csv(GUSGOdataframe, "GUSGOPercentofIndicatorsMeeting.csv")

#GUSGP
GUSGP<-subset(bench,Actual.Eco.Site=="GUSGP")
GUSGPmean<-mean(na.omit(GUSGP$baresoil))
GUSGPsd<-sd(na.omit(GUSGP$baresoil))
breaks<-c(-Inf,unique(c(GUSGPmean-(2*GUSGPsd),GUSGPmean+(2*GUSGPsd))),Inf)
tags<-c("0","1","0")
GUSGP$BSgroup_tags<-cut(GUSGP$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$BSgroup_tags)
GUSGP$BSgroup_tags<-as.numeric(as.character(GUSGP$BSgroup_tags))

GUSGP<-GUSGP %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

GUSGPSSmean<-mean(na.omit(GUSGP$soilstability))
GUSGPSSsd<-sd(na.omit(GUSGP$soilstability))
breaks<-c(-Inf,unique(c(GUSGPSSmean-(2*GUSGPSSsd),GUSGPSSmean+(2*GUSGPSSsd))),Inf)
breaks
tags<-c("0","1","0")
GUSGP$SSgroup_tags<-cut(GUSGP$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$SSgroup_tags)
GUSGP$SSgroup_tags<-as.numeric(as.character(GUSGP$SSgroup_tags))

GUSGP<-GUSGP %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

GUSGPNmean<-mean(na.omit(GUSGP$native))
GUSGPNsd<-sd(na.omit(GUSGP$native))
breaks<-c(-Inf,unique(c(GUSGPNmean-(2*GUSGPNsd),GUSGPNmean+(2*GUSGPNsd))),Inf)
tags<-c("0","1","0")
GUSGP$Ngroup_tags<-cut(GUSGP$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Ngroup_tags)
GUSGP$Ngroup_tags<-as.numeric(as.character(GUSGP$Ngroup_tags))

GUSGP<-GUSGP %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


GUSGPGmean<-mean(na.omit(GUSGP$grasscover))
GUSGPGsd<-sd(na.omit(GUSGP$grasscover))
breaks<-c(-Inf,unique(c(GUSGPGmean-(2*GUSGPGsd),GUSGPGmean+(2*GUSGPGsd))),Inf)
tags<-c("0","1","0")
GUSGP$Ggroup_tags<-cut(GUSGP$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Ggroup_tags)
GUSGP$Ggroup_tags<-as.numeric(as.character(GUSGP$Ggroup_tags))

GUSGP<-GUSGP %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


GUSGPFmean<-mean(na.omit(GUSGP$forbcover))
GUSGPFsd<-sd(na.omit(GUSGP$forbcover))
breaks<-c(-Inf,unique(c(GUSGPFmean-(2*GUSGPFsd),GUSGPFmean+(2*GUSGPFsd))),Inf)
tags<-c("0","1","0")
GUSGP$Fgroup_tags<-cut(GUSGP$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Fgroup_tags)
GUSGP$Fgroup_tags<-as.numeric(as.character(GUSGP$Fgroup_tags))

GUSGP<-GUSGP %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

GUSGPSmean<-mean(na.omit(GUSGP$shrubcover))
GUSGPSsd<-sd(na.omit(GUSGP$shrubcover))
breaks<-c(-Inf,unique(c(GUSGPSmean-(2*GUSGPSsd),GUSGPSmean+(2*GUSGPSsd))),Inf)
tags<-c("0","1","0")
GUSGP$Sgroup_tags<-cut(GUSGP$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Sgroup_tags)
GUSGP$Sgroup_tags<-as.numeric(as.character(GUSGP$Sgroup_tags))

GUSGP<-GUSGP %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

GUSGPTmean<-mean(na.omit(GUSGP$treecover))
GUSGPTsd<-sd(na.omit(GUSGP$treecover))
breaks<-c(-Inf,unique(c(GUSGPTmean-(2*GUSGPTsd),GUSGPTmean+(2*GUSGPTsd))),Inf)
tags<-c("0","1")
GUSGP$Tgroup_tags<-cut(GUSGP$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Tgroup_tags)
GUSGP$Tgroup_tags<-as.numeric(as.character(GUSGP$Tgroup_tags))

GUSGP<-GUSGP %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

GUSGPGHmean<-mean(na.omit(GUSGP$grassheight))
GUSGPGHsd<-sd(na.omit(GUSGP$grassheight))
breaks<-c(-Inf,GUSGPGHmean-GUSGPGHsd,GUSGPGHmean+GUSGPGHsd,Inf)
tags<-c("0","1","0")
GUSGP$GHgroup_tags<-cut(GUSGP$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$GHgroup_tags)
GUSGP$GHgroup_tags<-as.numeric(as.character(GUSGP$GHgroup_tags))

GUSGP<-GUSGP %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

GUSGPFHmean<-mean(na.omit(GUSGP$forb.height))
GUSGPFHsd<-sd(na.omit(GUSGP$forb.height))
breaks<-c(-Inf,unique(c(GUSGPFHmean-(2*GUSGPFHsd),GUSGPFHmean+(2*GUSGPFHsd))),Inf)
tags<-c("0","1","0")
GUSGP$FHgroup_tags<-cut(GUSGP$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$FHgroup_tags)
GUSGP$FHgroup_tags<-as.numeric(as.character(GUSGP$FHgroup_tags))

GUSGP<-GUSGP %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
GUSGPNNmean<-mean(na.omit(GUSGP$nonnative))
GUSGPNNsd<-sd(na.omit(GUSGP$nonnative))
breaks<-c(-Inf,unique(GUSGPNNmean-GUSGPNNsd,GUSGPNNmean+GUSGPNNsd),Inf)
breaks
tags<-c("0","1")
GUSGP$NNgroup_tags<-cut(GUSGP$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$NNgroup_tags)
GUSGP$NNgroup_tags<-as.numeric(as.character(GUSGP$NNgroup_tags))

GUSGP<-GUSGP %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

GUSGPFNmean<-mean(na.omit(GUSGP$forbnumber))
GUSGPFNsd<-sd(na.omit(GUSGP$forbnumber))
breaks<-c(-Inf,unique(c(GUSGPFNmean-(2*GUSGPFNsd),GUSGPFNmean+(2*GUSGPFNsd))),Inf)
tags<-c("0","1","0")
GUSGP$FNgroup_tags<-cut(GUSGP$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$FNgroup_tags)
GUSGP$FNgroup_tags<-as.numeric(as.character(GUSGP$FNgroup_tags))
GUSGP<-GUSGP %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

GUSGPRmean<-mean(na.omit(GUSGP$rock))
GUSGPRsd<-sd(na.omit(GUSGP$rock))
breaks<-c(-Inf,unique(c(GUSGPRmean-(2*GUSGPRsd),GUSGPRmean+(2*GUSGPRsd))),Inf)
tags<-c("0","1","0")
GUSGP$Rgroup_tags<-cut(GUSGP$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Rgroup_tags)
GUSGP$Rgroup_tags<-as.numeric(as.character(GUSGP$Rgroup_tags))

GUSGP<-GUSGP %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

GUSGPLmean<-mean(na.omit(GUSGP$litter))
GUSGPLsd<-sd(na.omit(GUSGP$litter))
breaks<-c(-Inf,unique(c(GUSGPLmean-(2*GUSGPLsd),GUSGPLmean+(2*GUSGPLsd))),Inf)
tags<-c("0","1","0")
GUSGP$Lgroup_tags<-cut(GUSGP$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$Lgroup_tags)
GUSGP$Lgroup_tags<-as.numeric(as.character(GUSGP$Lgroup_tags))

GUSGP<-GUSGP %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


GUSGPSUmean<-mean(na.omit(GUSGP$succulent))
GUSGPSUsd<-sd(na.omit(GUSGP$succulent))
breaks<-c(-Inf,unique(c(GUSGPSUmean-(2*GUSGPSUsd),GUSGPSUmean+(2*GUSGPSUsd))),Inf)
tags<-c("0","1","0")
GUSGP$SUgroup_tags<-cut(GUSGP$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$SUgroup_tags)
GUSGP$SUgroup_tags<-as.numeric(as.character(GUSGP$SUgroup_tags))

GUSGP<-GUSGP %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

GUSGPGAmean<-mean(na.omit(GUSGP$gaps))
GUSGPGAsd<-sd(na.omit(GUSGP$gaps))
breaks<-c(-Inf,unique(c(GUSGPGAmean-(2*GUSGPGAsd),GUSGPGAmean+(2*GUSGPGAsd))),Inf)
tags<-c("0","1","0")
GUSGP$GAgroup_tags<-cut(GUSGP$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$GAgroup_tags)
GUSGP$GAgroup_tags<-as.numeric(as.character(GUSGP$GAgroup_tags))

GUSGP<-GUSGP %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
GUSGPL<-GUSGP %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
GUSGPL<-gather(GUSGPL,indicator,value,-plotID,na.rm=TRUE)
GUSGPL<- GUSGPL %>%
  group_by(plotID) %>%
  mutate(sum=n())
GUSGPGraph<-GUSGP
GUSGPGraph$sum<-GUSGPL$sum[match(GUSGPGraph$plotID,GUSGPL$plotID)]
GUSGPGraph[is.na(GUSGPGraph)] <- 0
GUSGPGraph<-GUSGPGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
GUSGPGraph$list<-(1:length(GUSGPGraph$graph))

GUSGPB<-ggplot(GUSGPGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-GUSGPB+ggtitle("Plots within expected range GUSGP")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
GUSGP2<-GUSGP
GUSGP2$graph<-GUSGPGraph$graph[match(GUSGP2$plotID,GUSGPGraph$plotID)]
GUSGP2$graph<-as.numeric(GUSGP$graph)
GUSGP2$graph<-round(GUSGP2$graph,digits=2)
GUSGP3<-GUSGP2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
GUSGP3<-GUSGP3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(GUSGP3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(GUSGP3)
write.csv(GUSGP3,"GUSGPIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
GUSGPA1<-GUSGP
GUSGPA1$graph<-GUSGPGraph$graph[match(GUSGPA1$plotID,GUSGPGraph$plotID)]
GUSGP2$graph<-as.numeric(GUSGPGraph$graph)
GUSGPA1$graph<-round(GUSGP2$graph,digits=2)
GUSGPA<-GUSGPA1 %>%
  select(plotID,allotment,graph)
GUSGPA<-GUSGPA%>%
  arrange(allotment)
names(GUSGPA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(GUSGPA)

write.csv(GUSGPA,"GUSGPallotmentspercentmeeting.csv")

#REGUSGWVE DUPLICATES
GUSGP$baresoil.bin<-GUSGP$baresoil.bin*100
GUSGP$soilstabilitybin<-GUSGP$soilstabilitybin*100
GUSGP$treecoverbin<-GUSGP$treecoverbin*100
GUSGP$shrubcoverbin<-GUSGP$shrubcoverbin*100
GUSGP$grasscoverbin<-GUSGP$grasscoverbin*100
GUSGP$grassheightbin<-GUSGP$grassheightbin*100
GUSGP$forb.heightbin<-GUSGP$forb.heightbin*100
GUSGP$forbcoverbin<-GUSGP$forbcoverbin*100
GUSGP$gapsbin<-GUSGP$gapsbin*100
GUSGP$nonnativebin<-GUSGP$nonnativebin*100
GUSGP$nativebin<-GUSGP$nativebin*100
GUSGP$succulentbin<-GUSGP$succulentbin*100
GUSGP$litterbin<-GUSGP$litterbin*100
GUSGP$rockbin<-GUSGP$rockbin*100
GUSGP$forbnumberbin<-GUSGP$forbnumberbin*100
GUSGP$graph<-GUSGPGraph$graph[match(GUSGP$plotID,GUSGPGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
GUSGP$SUMgroup_tags<-cut(GUSGP$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGP$SUMgroup_tags)
GUSGP$SUMgroup_tags<-as.numeric(as.character(GUSGP$SUMgroup_tags))

GUSGP<-GUSGP %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
GUSGPR<-GUSGP
GUSGP<-GUSGP[!duplicated(GUSGP$nativebin),]
stratablanks<-rep("",15)
Strata<-c("GUSGP",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
              "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
              "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(GUSGP$baresoil.bin,digits=2),round(GUSGP$soilstabilitybin,digits=2),round(GUSGP$litterbin,digits=2),round(GUSGP$nativebin,digits=2),round(GUSGP$grasscoverbin,digits=2),round(GUSGP$forbcoverbin,digits=2),
                   round(GUSGP$shrubcoverbin,digits=2),round(GUSGP$nonnativebin,digits=2),round(GUSGP$forbnumberbin,digits=2),round(GUSGP$rockbin,digits=2),
                   round(GUSGP$treecoverbin,digits=2),round(GUSGP$succulentbin,digits=2),round(GUSGP$gapsbin,digits=2),round(GUSGP$forb.heightbin,digits=2),round(GUSGP$grassheightbin,digits=2),round(GUSGP$SUMbin,digits=2))
GUSGPdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(GUSGPdataframe)
#to write csv
write.csv(GUSGPdataframe,"GUSGPPercentofIndicatorsMeeting.csv")
#GUSGW
GUSGW<-subset(bench,Actual.Eco.Site=="GUSGW")
GUSGWmean<-mean(na.omit(GUSGW$baresoil))
GUSGWsd<-sd(na.omit(GUSGW$baresoil))
breaks<-c(-Inf,unique(c(GUSGWmean-(2*GUSGWsd),GUSGWmean+(2*GUSGWsd))),Inf)
tags<-c("0","1","0")
GUSGW$BSgroup_tags<-cut(GUSGW$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$BSgroup_tags)
GUSGW$BSgroup_tags<-as.numeric(as.character(GUSGW$BSgroup_tags))

GUSGW<-GUSGW %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

GUSGWSSmean<-mean(na.omit(GUSGW$soilstability))
GUSGWSSsd<-sd(na.omit(GUSGW$soilstability))
breaks<-c(-Inf,unique(c(GUSGWSSmean-(2*GUSGWSSsd),GUSGWSSmean+(2*GUSGWSSsd))),Inf)
breaks
tags<-c("0","1","0")
GUSGW$SSgroup_tags<-cut(GUSGW$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$SSgroup_tags)
GUSGW$SSgroup_tags<-as.numeric(as.character(GUSGW$SSgroup_tags))

GUSGW<-GUSGW %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

GUSGWNmean<-mean(na.omit(GUSGW$native))
GUSGWNsd<-sd(na.omit(GUSGW$native))
breaks<-c(-Inf,unique(c(GUSGWNmean-(2*GUSGWNsd),GUSGWNmean+(2*GUSGWNsd))),Inf)
tags<-c("0","1","0")
GUSGW$Ngroup_tags<-cut(GUSGW$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Ngroup_tags)
GUSGW$Ngroup_tags<-as.numeric(as.character(GUSGW$Ngroup_tags))

GUSGW<-GUSGW %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


GUSGWGmean<-mean(na.omit(GUSGW$grasscover))
GUSGWGsd<-sd(na.omit(GUSGW$grasscover))
breaks<-c(-Inf,unique(c(GUSGWGmean-(2*GUSGWGsd),GUSGWGmean+(2*GUSGWGsd))),Inf)
tags<-c("0","1","0")
GUSGW$Ggroup_tags<-cut(GUSGW$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Ggroup_tags)
GUSGW$Ggroup_tags<-as.numeric(as.character(GUSGW$Ggroup_tags))

GUSGW<-GUSGW %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


GUSGWFmean<-mean(na.omit(GUSGW$forbcover))
GUSGWFsd<-sd(na.omit(GUSGW$forbcover))
breaks<-c(-Inf,unique(c(GUSGWFmean-(2*GUSGWFsd),GUSGWFmean+(2*GUSGWFsd))),Inf)
tags<-c("0","1","0")
GUSGW$Fgroup_tags<-cut(GUSGW$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Fgroup_tags)
GUSGW$Fgroup_tags<-as.numeric(as.character(GUSGW$Fgroup_tags))

GUSGW<-GUSGW %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

GUSGWSmean<-mean(na.omit(GUSGW$shrubcover))
GUSGWSsd<-sd(na.omit(GUSGW$shrubcover))
breaks<-c(-Inf,unique(c(GUSGWSmean-(2*GUSGWSsd),GUSGWSmean+(2*GUSGWSsd))),Inf)
tags<-c("0","1","0")
GUSGW$Sgroup_tags<-cut(GUSGW$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Sgroup_tags)
GUSGW$Sgroup_tags<-as.numeric(as.character(GUSGW$Sgroup_tags))

GUSGW<-GUSGW %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

GUSGWTmean<-mean(na.omit(GUSGW$treecover))
GUSGWTsd<-sd(na.omit(GUSGW$treecover))
breaks<-c(-Inf,unique(c(GUSGWTmean-(2*GUSGWTsd),GUSGWTmean+(2*GUSGWTsd))),Inf)
tags<-c("0","1")
GUSGW$Tgroup_tags<-cut(GUSGW$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Tgroup_tags)
GUSGW$Tgroup_tags<-as.numeric(as.character(GUSGW$Tgroup_tags))

GUSGW<-GUSGW %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

GUSGWGHmean<-mean(na.omit(GUSGW$grassheight))
GUSGWGHsd<-sd(na.omit(GUSGW$grassheight))
breaks<-c(-Inf,GUSGWGHmean-GUSGWGHsd,GUSGWGHmean+GUSGWGHsd,Inf)
tags<-c("0","1","0")
GUSGW$GHgroup_tags<-cut(GUSGW$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$GHgroup_tags)
GUSGW$GHgroup_tags<-as.numeric(as.character(GUSGW$GHgroup_tags))

GUSGW<-GUSGW %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

GUSGWFHmean<-mean(na.omit(GUSGW$forb.height))
GUSGWFHsd<-sd(na.omit(GUSGW$forb.height))
breaks<-c(-Inf,unique(c(GUSGWFHmean-(2*GUSGWFHsd),GUSGWFHmean+(2*GUSGWFHsd))),Inf)
tags<-c("0","1","0")
GUSGW$FHgroup_tags<-cut(GUSGW$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$FHgroup_tags)
GUSGW$FHgroup_tags<-as.numeric(as.character(GUSGW$FHgroup_tags))

GUSGW<-GUSGW %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
GUSGWNNmean<-mean(na.omit(GUSGW$nonnative))
GUSGWNNsd<-sd(na.omit(GUSGW$nonnative))
breaks<-c(-Inf,unique(GUSGWNNmean-GUSGWNNsd,GUSGWNNmean+GUSGWNNsd),Inf)
breaks
tags<-c("0","1")
GUSGW$NNgroup_tags<-cut(GUSGW$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$NNgroup_tags)
GUSGW$NNgroup_tags<-as.numeric(as.character(GUSGW$NNgroup_tags))

GUSGW<-GUSGW %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

GUSGWFNmean<-mean(na.omit(GUSGW$forbnumber))
GUSGWFNsd<-sd(na.omit(GUSGW$forbnumber))
breaks<-c(-Inf,unique(c(GUSGWFNmean-(2*GUSGWFNsd),GUSGWFNmean+(2*GUSGWFNsd))),Inf)
tags<-c("0","1","0")
GUSGW$FNgroup_tags<-cut(GUSGW$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$FNgroup_tags)
GUSGW$FNgroup_tags<-as.numeric(as.character(GUSGW$FNgroup_tags))
GUSGW<-GUSGW %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

GUSGWRmean<-mean(na.omit(GUSGW$rock))
GUSGWRsd<-sd(na.omit(GUSGW$rock))
breaks<-c(-Inf,unique(c(GUSGWRmean-(2*GUSGWRsd),GUSGWRmean+(2*GUSGWRsd))),Inf)
tags<-c("0","1","0")
GUSGW$Rgroup_tags<-cut(GUSGW$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Rgroup_tags)
GUSGW$Rgroup_tags<-as.numeric(as.character(GUSGW$Rgroup_tags))

GUSGW<-GUSGW %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

GUSGWLmean<-mean(na.omit(GUSGW$litter))
GUSGWLsd<-sd(na.omit(GUSGW$litter))
breaks<-c(-Inf,unique(c(GUSGWLmean-(2*GUSGWLsd),GUSGWLmean+(2*GUSGWLsd))),Inf)
tags<-c("0","1","0")
GUSGW$Lgroup_tags<-cut(GUSGW$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$Lgroup_tags)
GUSGW$Lgroup_tags<-as.numeric(as.character(GUSGW$Lgroup_tags))

GUSGW<-GUSGW %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


GUSGWSUmean<-mean(na.omit(GUSGW$succulent))
GUSGWSUsd<-sd(na.omit(GUSGW$succulent))
breaks<-c(-Inf,unique(c(GUSGWSUmean-(2*GUSGWSUsd),GUSGWSUmean+(2*GUSGWSUsd))),Inf)
tags<-c("0","1","0")
GUSGW$SUgroup_tags<-cut(GUSGW$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$SUgroup_tags)
GUSGW$SUgroup_tags<-as.numeric(as.character(GUSGW$SUgroup_tags))

GUSGW<-GUSGW %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

GUSGWGAmean<-mean(na.omit(GUSGW$gaps))
GUSGWGAsd<-sd(na.omit(GUSGW$gaps))
breaks<-c(-Inf,unique(c(GUSGWGAmean-(2*GUSGWGAsd),GUSGWGAmean+(2*GUSGWGAsd))),Inf)
tags<-c("0","1","0")
GUSGW$GAgroup_tags<-cut(GUSGW$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$GAgroup_tags)
GUSGW$GAgroup_tags<-as.numeric(as.character(GUSGW$GAgroup_tags))

GUSGW<-GUSGW %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
GUSGWL<-GUSGW %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
GUSGWL<-gather(GUSGWL,indicator,value,-plotID,na.rm=TRUE)
GUSGWL<- GUSGWL %>%
  group_by(plotID) %>%
  mutate(sum=n())
GUSGWGraph<-GUSGW
GUSGWGraph$sum<-GUSGWL$sum[match(GUSGWGraph$plotID,GUSGWL$plotID)]
GUSGWGraph[is.na(GUSGWGraph)] <- 0
GUSGWGraph<-GUSGWGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
GUSGWGraph$list<-(1:length(GUSGWGraph$graph))

GUSGWB<-ggplot(GUSGWGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-GUSGWB+ggtitle("Plots within expected range GUSGW")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
GUSGW2<-GUSGW
GUSGW2$graph<-GUSGWGraph$graph[match(GUSGW2$plotID,GUSGWGraph$plotID)]
GUSGW2$graph<-as.numeric(GUSGW2$graph)
GUSGW2$graph<-round(GUSGW2$graph,digits=2)
GUSGW3<-GUSGW2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
GUSGW3<-GUSGW3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(GUSGW3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(GUSGW3)
write.csv(GUSGW3,"GUSGWIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
GUSGWA1<-GUSGW
GUSGW2$graph<-as.numeric(GUSGWGraph$graph)
GUSGWA1$graph<-round(GUSGW2$graph,digits=2)
GUSGWA<-GUSGWA1 %>%
  select(plotID,allotment,graph)
GUSGWA<-GUSGWA%>%
  arrange(allotment)
names(GUSGWA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(GUSGWA)

write.csv(GUSGWA,"GUSGWallotmentspercentmeeting.csv")

#REGUSGWVE DUPLICATES
GUSGW$baresoil.bin<-GUSGW$baresoil.bin*100
GUSGW$soilstabilitybin<-GUSGW$soilstabilitybin*100
GUSGW$treecoverbin<-GUSGW$treecoverbin*100
GUSGW$shrubcoverbin<-GUSGW$shrubcoverbin*100
GUSGW$grasscoverbin<-GUSGW$grasscoverbin*100
GUSGW$grassheightbin<-GUSGW$grassheightbin*100
GUSGW$forb.heightbin<-GUSGW$forb.heightbin*100
GUSGW$forbcoverbin<-GUSGW$forbcoverbin*100
GUSGW$gapsbin<-GUSGW$gapsbin*100
GUSGW$nonnativebin<-GUSGW$nonnativebin*100
GUSGW$nativebin<-GUSGW$nativebin*100
GUSGW$succulentbin<-GUSGW$succulentbin*100
GUSGW$litterbin<-GUSGW$litterbin*100
GUSGW$rockbin<-GUSGW$rockbin*100
GUSGW$forbnumberbin<-GUSGW$forbnumberbin*100
GUSGW$graph<-GUSGWGraph$graph[match(GUSGW$plotID,GUSGWGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
GUSGW$SUMgroup_tags<-cut(GUSGW$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(GUSGW$SUMgroup_tags)
GUSGW$SUMgroup_tags<-as.numeric(as.character(GUSGW$SUMgroup_tags))

GUSGW<-GUSGW %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
GUSGWR<-GUSGW
GUSGW<-GUSGW[!duplicated(GUSGW$nativebin),]
stratablanks<-rep("",15)
Strata<-c("GUSGW",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
              "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
              "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting")
Percent.Meeting<-c(round(GUSGW$baresoil.bin,digits=2),round(GUSGW$soilstabilitybin,digits=2),round(GUSGW$litterbin,digits=2),round(GUSGW$nativebin,digits=2),round(GUSGW$grasscoverbin,digits=2),round(GUSGW$forbcoverbin,digits=2),
                   round(GUSGW$shrubcoverbin,digits=2),round(GUSGW$nonnativebin,digits=2),round(GUSGW$forbnumberbin,digits=2),round(GUSGW$rockbin,digits=2),
                   round(GUSGW$treecoverbin,digits=2),round(GUSGW$succulentbin,digits=2),round(GUSGW$gapsbin,digits=2),round(GUSGW$forb.heightbin,digits=2),round(GUSGW$grassheightbin,digits=2),round(GUSGW$SUMbin,digits=2))
GUSGWdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(GUSGWdataframe)
#to write csv
write.csv(GUSGWdataframe,"GUSGWPercentofIndicatorsMeeting.csv")

#ran summary of allotments in the master sample code since it's all one field office
