#this code creates benchmarks for all indicators (indicators= baresoil, rock, tree, shrub, soil stability, forb number, forb cover,
#grass cover, forb height, grass height, succulent, litter, noxious species, nonnoxious species and succulent cover) then produces 
#tables for indicators met by each plot, percent of plots meeting each indicator by stratum and percent of plots being met by allotment

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

#BH
BH<-subset(bench,Actual.Eco.Site=="BH")
BHmean<-mean(na.omit(BH$baresoil))
BHsd<-sd(na.omit(BH$baresoil))
breaks<-c(-Inf,unique(c(BHmean-(2*BHsd),BHmean+(2*BHsd))),Inf)
tags<-c("0","1","0")
BH$BSgroup_tags<-cut(BH$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$BSgroup_tags)
BH$BSgroup_tags<-as.numeric(as.character(BH$BSgroup_tags))

BH<-BH %>%
   mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

BHSSmean<-mean(na.omit(BH$soilstability))
BHSSsd<-sd(na.omit(BH$soilstability))
breaks<-c(-Inf,unique(c(BHSSmean-(2*BHSSsd),BHSSmean+(2*BHSSsd))),Inf)
breaks
tags<-c("0","1","0")
BH$SSgroup_tags<-cut(BH$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$SSgroup_tags)
BH$SSgroup_tags<-as.numeric(as.character(BH$SSgroup_tags))

BH<-BH %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

BHNmean<-mean(na.omit(BH$native))
BHNsd<-sd(na.omit(BH$native))
breaks<-c(-Inf,unique(c(BHNmean-(2*BHNsd),BHNmean+(2*BHNsd))),Inf)
tags<-c("0","1","0")
BH$Ngroup_tags<-cut(BH$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Ngroup_tags)
BH$Ngroup_tags<-as.numeric(as.character(BH$Ngroup_tags))

BH<-BH %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


BHGmean<-mean(na.omit(BH$grasscover))
BHGsd<-sd(na.omit(BH$grasscover))
breaks<-c(-Inf,unique(c(BHGmean-(2*BHGsd),BHGmean+(2*BHGsd))),Inf)
tags<-c("0","1","0")
BH$Ggroup_tags<-cut(BH$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Ggroup_tags)
BH$Ggroup_tags<-as.numeric(as.character(BH$Ggroup_tags))

BH<-BH %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


BHFmean<-mean(na.omit(BH$forbcover))
BHFsd<-sd(na.omit(BH$forbcover))
breaks<-c(-Inf,unique(c(BHFmean-(2*BHFsd),BHFmean+(2*BHFsd))),Inf)
tags<-c("0","1","0")
BH$Fgroup_tags<-cut(BH$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Fgroup_tags)
BH$Fgroup_tags<-as.numeric(as.character(BH$Fgroup_tags))

BH<-BH %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

BHSmean<-mean(na.omit(BH$shrubcover))
BHSsd<-sd(na.omit(BH$shrubcover))
breaks<-c(-Inf,unique(c(BHSmean-(2*BHSsd),BHSmean+(2*BHSsd))),Inf)
tags<-c("0","1","0")
BH$Sgroup_tags<-cut(BH$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Sgroup_tags)
BH$Sgroup_tags<-as.numeric(as.character(BH$Sgroup_tags))

BH<-BH %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

BHTmean<-mean(na.omit(BH$treecover))
BHTsd<-sd(na.omit(BH$treecover))
breaks<-c(-Inf,unique(c(BHTmean-(2*BHTsd),BHTmean+(2*BHTsd))),Inf)
tags<-c("0","1","0")
BH$Tgroup_tags<-cut(BH$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Tgroup_tags)
BH$Tgroup_tags<-as.numeric(as.character(BH$Tgroup_tags))

BH<-BH %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

BHGHmean<-mean(na.omit(BH$grassheight))
BHGHsd<-sd(na.omit(BH$grassheight))
breaks<-c(-Inf,BHGHmean-BHGHsd,BHGHmean+BHGHsd,Inf)
tags<-c("0","1","0")
BH$GHgroup_tags<-cut(BH$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$GHgroup_tags)
BH$GHgroup_tags<-as.numeric(as.character(BH$GHgroup_tags))

BH<-BH %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

BHFHmean<-mean(na.omit(BH$forb.height))
BHFHsd<-sd(na.omit(BH$forb.height))
breaks<-c(-Inf,unique(c(BHFHmean-(2*BHFHsd),BHFHmean+(2*BHFHsd))),Inf)
tags<-c("0","1","0")
BH$FHgroup_tags<-cut(BH$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$FHgroup_tags)
BH$FHgroup_tags<-as.numeric(as.character(BH$FHgroup_tags))

BH<-BH %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
BHNNmean<-mean(na.omit(BH$nonnative))
BHNNsd<-sd(na.omit(BH$nonnative))
breaks<-c(-Inf,unique(BHNNmean-BHNNsd,BHNNmean+BHNNsd),Inf)
breaks
tags<-c("0","1")
BH$NNgroup_tags<-cut(BH$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$NNgroup_tags)
BH$NNgroup_tags<-as.numeric(as.character(BH$NNgroup_tags))

BH<-BH %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

BHFNmean<-mean(na.omit(BH$forbnumber))
BHFNsd<-sd(na.omit(BH$forbnumber))
breaks<-c(-Inf,unique(c(BHFNmean-(2*BHFNsd),BHFNmean+(2*BHFNsd))),Inf)
tags<-c("0","1","0")
BH$FNgroup_tags<-cut(BH$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$FNgroup_tags)
BH$FNgroup_tags<-as.numeric(as.character(BH$FNgroup_tags))
BH<-BH %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

BHRmean<-mean(na.omit(BH$rock))
BHRsd<-sd(na.omit(BH$rock))
breaks<-c(-Inf,unique(c(BHRmean-(2*BHRsd),BHRmean+(2*BHRsd))),Inf)
tags<-c("0","1","0")
BH$Rgroup_tags<-cut(BH$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Rgroup_tags)
BH$Rgroup_tags<-as.numeric(as.character(BH$Rgroup_tags))

BH<-BH %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

BHLmean<-mean(na.omit(BH$litter))
BHLsd<-sd(na.omit(BH$litter))
breaks<-c(-Inf,unique(c(BHLmean-(2*BHLsd),BHLmean+(2*BHLsd))),Inf)
tags<-c("0","1","0")
BH$Lgroup_tags<-cut(BH$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$Lgroup_tags)
BH$Lgroup_tags<-as.numeric(as.character(BH$Lgroup_tags))

BH<-BH %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


BHSUmean<-mean(na.omit(BH$succulent))
BHSUsd<-sd(na.omit(BH$succulent))
breaks<-c(-Inf,unique(c(BHSUmean-(2*BHSUsd),BHSUmean+(2*BHSUsd))),Inf)
tags<-c("0","1","0")
BH$SUgroup_tags<-cut(BH$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$SUgroup_tags)
BH$SUgroup_tags<-as.numeric(as.character(BH$SUgroup_tags))

BH<-BH %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

BHGAmean<-mean(na.omit(BH$gaps))
BHGAsd<-sd(na.omit(BH$gaps))
breaks<-c(-Inf,unique(c(BHGAmean-(2*BHGAsd),BHGAmean+(2*BHGAsd))),Inf)
tags<-c("0","1","0")
BH$GAgroup_tags<-cut(BH$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$GAgroup_tags)
BH$GAgroup_tags<-as.numeric(as.character(BH$GAgroup_tags))

BH<-BH %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
BHL<-BH %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
           NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
           Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
BHL<-gather(BHL,indicator,value,-plotID,na.rm=TRUE)
BHL<- BHL %>%
  group_by(plotID) %>%
  mutate(sum=n())
BHGraph<-BH
BHGraph$sum<-BHL$sum[match(BHGraph$plotID,BHL$plotID)]
BHGraph[is.na(BHGraph)] <- 0
BHGraph<-BHGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                  NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                  Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
BHGraph$list<-(1:length(BHGraph$graph))

BHB<-ggplot(BHGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-BHB+ggtitle("Plots within expected range BH")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
BH2<-BH
BH2$graph<-as.numeric(BHGraph$graph)
BH2$graph<-round(BH2$graph,digits=2)
BH3<-BH2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
BH3<-BH3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(BH3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(BH3)
write.csv(BH3,"BHIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
BHA1<-BH
BH2$graph<-as.numeric(BHGraph$graph)
BHA1$graph<-round(BH2$graph,digits=2)
BHA<-BHA1 %>%
  select(plotID,allotment,graph)
BHA<-BHA%>%
  arrange(allotment)
names(BHA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(BHA)

write.csv(BHA,"BHallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
BH$baresoil.bin<-BH$baresoil.bin*100
BH$soilstabilitybin<-BH$soilstabilitybin*100
BH$treecoverbin<-BH$treecoverbin*100
BH$shrubcoverbin<-BH$shrubcoverbin*100
BH$grasscoverbin<-BH$grasscoverbin*100
BH$grassheightbin<-BH$grassheightbin*100
BH$forb.heightbin<-BH$forb.heightbin*100
BH$forbcoverbin<-BH$forbcoverbin*100
BH$gapsbin<-BH$gapsbin*100
BH$nonnativebin<-BH$nonnativebin*100
BH$nativebin<-BH$nativebin*100
BH$succulentbin<-BH$succulentbin*100
BH$litterbin<-BH$litterbin*100
BH$rockbin<-BH$rockbin*100
BH$forbnumberbin<-BH$forbnumberbin*100
BH$graph<-BHGraph$graph[match(BH$plotID,BHGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
BH$SUMgroup_tags<-cut(BH$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(BH$SUMgroup_tags)
BH$SUMgroup_tags<-as.numeric(as.character(BH$SUMgroup_tags))

BH<-BH %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
BHR<-BH
BH<-BH[!duplicated(BH$nativebin),]
stratablanks<-rep("",15)
Strata<-c("BH",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(BH$baresoil.bin,digits=2),round(BH$soilstabilitybin,digits=2),round(BH$litterbin,digits=2),round(BH$nativebin,digits=2),round(BH$grasscoverbin,digits=2),round(BH$forbcoverbin,digits=2),
                   round(BH$shrubcoverbin,digits=2),round(BH$nonnativebin,digits=2),round(BH$forbnumberbin,digits=2),round(BH$rockbin,digits=2),
                   round(BH$treecoverbin,digits=2),round(BH$succulentbin,digits=2),round(BH$gapsbin,digits=2),round(BH$forb.heightbin,digits=2),round(BH$grassheightbin,digits=2),round(BH$SUMbin,digits=2))
BHdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(BHdataframe)
#if you want a csv
write.csv(BHdataframe, "BHPercentofIndicatorsMeeting.csv")

#LB
LB<-subset(bench,Actual.Eco.Site=="LB")
LBmean<-mean(na.omit(LB$baresoil))
LBsd<-sd(na.omit(LB$baresoil))
breaks<-c(-Inf,unique(c(LBmean-(2*LBsd),LBmean+(2*LBsd))),Inf)
tags<-c("0","1","0")
LB$BSgroup_tags<-cut(LB$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$BSgroup_tags)
LB$BSgroup_tags<-as.numeric(as.character(LB$BSgroup_tags))

LB<-LB %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

LBSSmean<-mean(na.omit(LB$soilstability))
LBSSsd<-sd(na.omit(LB$soilstability))
breaks<-c(-Inf,unique(c(LBSSmean-(2*LBSSsd),LBSSmean+(2*LBSSsd))),Inf)
breaks
tags<-c("0","1","0")
LB$SSgroup_tags<-cut(LB$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$SSgroup_tags)
LB$SSgroup_tags<-as.numeric(as.character(LB$SSgroup_tags))

LB<-LB %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

LBNmean<-mean(na.omit(LB$native))
LBNsd<-sd(na.omit(LB$native))
breaks<-c(-Inf,unique(c(LBNmean-(2*LBNsd),LBNmean+(2*LBNsd))),Inf)
tags<-c("0","1","0")
LB$Ngroup_tags<-cut(LB$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Ngroup_tags)
LB$Ngroup_tags<-as.numeric(as.character(LB$Ngroup_tags))

LB<-LB %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


LBGmean<-mean(na.omit(LB$grasscover))
LBGsd<-sd(na.omit(LB$grasscover))
breaks<-c(-Inf,unique(c(LBGmean-(2*LBGsd),LBGmean+(2*LBGsd))),Inf)
tags<-c("0","1","0")
LB$Ggroup_tags<-cut(LB$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Ggroup_tags)
LB$Ggroup_tags<-as.numeric(as.character(LB$Ggroup_tags))

LB<-LB %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


LBFmean<-mean(na.omit(LB$forbcover))
LBFsd<-sd(na.omit(LB$forbcover))
breaks<-c(-Inf,unique(c(LBFmean-(2*LBFsd),LBFmean+(2*LBFsd))),Inf)
tags<-c("0","1","0")
LB$Fgroup_tags<-cut(LB$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Fgroup_tags)
LB$Fgroup_tags<-as.numeric(as.character(LB$Fgroup_tags))

LB<-LB %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

LBSmean<-mean(na.omit(LB$shrubcover))
LBSsd<-sd(na.omit(LB$shrubcover))
breaks<-c(-Inf,unique(c(LBSmean-(2*LBSsd),LBSmean+(2*LBSsd))),Inf)
tags<-c("0","1","0")
LB$Sgroup_tags<-cut(LB$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Sgroup_tags)
LB$Sgroup_tags<-as.numeric(as.character(LB$Sgroup_tags))

LB<-LB %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

LBTmean<-mean(na.omit(LB$treecover))
LBTsd<-sd(na.omit(LB$treecover))
breaks<-c(-Inf,unique(c(LBTmean-(2*LBTsd),LBTmean+(2*LBTsd))),Inf)
tags<-c("0","1","0")
LB$Tgroup_tags<-cut(LB$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Tgroup_tags)
LB$Tgroup_tags<-as.numeric(as.character(LB$Tgroup_tags))

LB<-LB %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

LBGHmean<-mean(na.omit(LB$grassheight))
LBGHsd<-sd(na.omit(LB$grassheight))
breaks<-c(-Inf,LBGHmean-LBGHsd,LBGHmean+LBGHsd,Inf)
tags<-c("0","1","0")
LB$GHgroup_tags<-cut(LB$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$GHgroup_tags)
LB$GHgroup_tags<-as.numeric(as.character(LB$GHgroup_tags))

LB<-LB %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

LBFHmean<-mean(na.omit(LB$forb.height))
LBFHsd<-sd(na.omit(LB$forb.height))
breaks<-c(-Inf,unique(c(LBFHmean-(2*LBFHsd),LBFHmean+(2*LBFHsd))),Inf)
tags<-c("0","1","0")
LB$FHgroup_tags<-cut(LB$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$FHgroup_tags)
LB$FHgroup_tags<-as.numeric(as.character(LB$FHgroup_tags))

LB<-LB %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
LBNNmean<-mean(na.omit(LB$nonnative))
LBNNsd<-sd(na.omit(LB$nonnative))
breaks<-c(-Inf,unique(LBNNmean-LBNNsd,LBNNmean+LBNNsd),Inf)
breaks
tags<-c("0","1")
LB$NNgroup_tags<-cut(LB$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$NNgroup_tags)
LB$NNgroup_tags<-as.numeric(as.character(LB$NNgroup_tags))

LB<-LB %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

LBFNmean<-mean(na.omit(LB$forbnumber))
LBFNsd<-sd(na.omit(LB$forbnumber))
breaks<-c(-Inf,unique(c(LBFNmean-(2*LBFNsd),LBFNmean+(2*LBFNsd))),Inf)
tags<-c("0","1","0")
LB$FNgroup_tags<-cut(LB$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$FNgroup_tags)
LB$FNgroup_tags<-as.numeric(as.character(LB$FNgroup_tags))
LB<-LB %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

LBRmean<-mean(na.omit(LB$rock))
LBRsd<-sd(na.omit(LB$rock))
breaks<-c(-Inf,unique(c(LBRmean-(2*LBRsd),LBRmean+(2*LBRsd))),Inf)
tags<-c("0","1","0")
LB$Rgroup_tags<-cut(LB$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Rgroup_tags)
LB$Rgroup_tags<-as.numeric(as.character(LB$Rgroup_tags))

LB<-LB %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

LBLmean<-mean(na.omit(LB$litter))
LBLsd<-sd(na.omit(LB$litter))
breaks<-c(-Inf,unique(c(LBLmean-(2*LBLsd),LBLmean+(2*LBLsd))),Inf)
tags<-c("0","1","0")
LB$Lgroup_tags<-cut(LB$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$Lgroup_tags)
LB$Lgroup_tags<-as.numeric(as.character(LB$Lgroup_tags))

LB<-LB %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


LBSUmean<-mean(na.omit(LB$succulent))
LBSUsd<-sd(na.omit(LB$succulent))
breaks<-c(-Inf,unique(c(LBSUmean-(2*LBSUsd),LBSUmean+(2*LBSUsd))),Inf)
tags<-c("0","1","0")
LB$SUgroup_tags<-cut(LB$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$SUgroup_tags)
LB$SUgroup_tags<-as.numeric(as.character(LB$SUgroup_tags))

LB<-LB %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

LBGAmean<-mean(na.omit(LB$gaps))
LBGAsd<-sd(na.omit(LB$gaps))
breaks<-c(-Inf,unique(c(LBGAmean-(2*LBGAsd),LBGAmean+(2*LBGAsd))),Inf)
tags<-c("0","1","0")
LB$GAgroup_tags<-cut(LB$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$GAgroup_tags)
LB$GAgroup_tags<-as.numeric(as.character(LB$GAgroup_tags))

LB<-LB %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
LBL<-LB %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
LBL<-gather(LBL,indicator,value,-plotID,na.rm=TRUE)
LBL<- LBL %>%
  group_by(plotID) %>%
  mutate(sum=n())
LBGraph<-LB
LBGraph$sum<-LBL$sum[match(LBGraph$plotID,LBL$plotID)]
LBGraph[is.na(LBGraph)] <- 0
LBGraph<-LBGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
LBGraph$list<-(1:length(LBGraph$graph))

LBB<-ggplot(LBGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-LBB+ggtitle("Plots within expected range LB")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
LB2<-LB
LB2$graph<-LBGraph$graph[match(LB2$plotID,LBGraph$plotID)]
LB2$graph<-as.numeric(LB$graph)
LB2$graph<-round(LB2$graph,digits=2)
LB3<-LB2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
LB3<-LB3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(LB3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(LB3)
write.csv(LB3,"LBIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
LBA1<-LB
LB2$graph<-as.numeric(LBGraph$graph)
LBA1$graph<-round(LB2$graph,digits=2)
LBA<-LBA1 %>%
  select(plotID,allotment,graph)
LBA<-LBA%>%
  arrange(allotment)
names(LBA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(LBA)

write.csv(LBA,"LBallotmentspercentmeeting.csv")
#number of plots within allotment meeting benchmark

#REMOVE DUPLICATES

LB$baresoil.bin<-LB$baresoil.bin*100
LB$soilstabilitybin<-LB$soilstabilitybin*100
LB$treecoverbin<-LB$treecoverbin*100
LB$shrubcoverbin<-LB$shrubcoverbin*100
LB$grasscoverbin<-LB$grasscoverbin*100
LB$grassheightbin<-LB$grassheightbin*100
LB$forb.heightbin<-LB$forb.heightbin*100
LB$forbcoverbin<-LB$forbcoverbin*100
LB$gapsbin<-LB$gapsbin*100
LB$nonnativebin<-LB$nonnativebin*100
LB$nativebin<-LB$nativebin*100
LB$succulentbin<-LB$succulentbin*100
LB$litterbin<-LB$litterbin*100
LB$rockbin<-LB$rockbin*100
LB$forbnumberbin<-LB$forbnumberbin*100
LB$graph<-LBGraph$graph[match(LB$plotID,LBGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
LB$SUMgroup_tags<-cut(LB$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LB$SUMgroup_tags)
LB$SUMgroup_tags<-as.numeric(as.character(LB$SUMgroup_tags))

LB<-LB %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
LBR<-LB
LB<-LB[!duplicated(LB$nativebin),]
stratablanks<-rep("",15)
Strata<-c("LB",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(LB$baresoil.bin,digits=2),round(LB$soilstabilitybin,digits=2),round(LB$litterbin,digits=2),round(LB$nativebin,digits=2),round(LB$grasscoverbin,digits=2),round(LB$forbcoverbin,digits=2),
                   round(LB$shrubcoverbin,digits=2),round(LB$nonnativebin,digits=2),round(LB$forbnumberbin,digits=2),round(LB$rockbin,digits=2),
                   round(LB$treecoverbin,digits=2),round(LB$succulentbin,digits=2),round(LB$gapsbin,digits=2),round(LB$forb.heightbin,digits=2),round(LB$grassheightbin,digits=2),round(LB$SUMbin,digits=2))
LBdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(LBdataframe)
#if you want a csv
write.csv(LBdataframe, "LBPercentofIndicatorsMeeting.csv")

#LOA
LOA<-subset(bench,Actual.Eco.Site=="LOA")
LOAmean<-mean(na.omit(LOA$baresoil))
LOAsd<-sd(na.omit(LOA$baresoil))
breaks<-c(-Inf,unique(c(LOAmean-(2*LOAsd),LOAmean+(2*LOAsd))),Inf)
tags<-c("0","1","0")
LOA$BSgroup_tags<-cut(LOA$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$BSgroup_tags)
LOA$BSgroup_tags<-as.numeric(as.character(LOA$BSgroup_tags))

LOA<-LOA %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

LOASSmean<-mean(na.omit(LOA$soilstability))
LOASSsd<-sd(na.omit(LOA$soilstability))
breaks<-c(-Inf,unique(c(LOASSmean-(2*LOASSsd),LOASSmean+(2*LOASSsd))),Inf)
breaks
tags<-c("0","1","0")
LOA$SSgroup_tags<-cut(LOA$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$SSgroup_tags)
LOA$SSgroup_tags<-as.numeric(as.character(LOA$SSgroup_tags))

LOA<-LOA %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

LOANmean<-mean(na.omit(LOA$native))
LOANsd<-sd(na.omit(LOA$native))
breaks<-c(-Inf,unique(c(LOANmean-(2*LOANsd),LOANmean+(2*LOANsd))),Inf)
tags<-c("0","1","0")
LOA$Ngroup_tags<-cut(LOA$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Ngroup_tags)
LOA$Ngroup_tags<-as.numeric(as.character(LOA$Ngroup_tags))

LOA<-LOA %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


LOAGmean<-mean(na.omit(LOA$grasscover))
LOAGsd<-sd(na.omit(LOA$grasscover))
breaks<-c(-Inf,unique(c(LOAGmean-(2*LOAGsd),LOAGmean+(2*LOAGsd))),Inf)
tags<-c("0","1","0")
LOA$Ggroup_tags<-cut(LOA$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Ggroup_tags)
LOA$Ggroup_tags<-as.numeric(as.character(LOA$Ggroup_tags))

LOA<-LOA %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


LOAFmean<-mean(na.omit(LOA$forbcover))
LOAFsd<-sd(na.omit(LOA$forbcover))
breaks<-c(-Inf,unique(c(LOAFmean-(2*LOAFsd),LOAFmean+(2*LOAFsd))),Inf)
tags<-c("0","1","0")
LOA$Fgroup_tags<-cut(LOA$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Fgroup_tags)
LOA$Fgroup_tags<-as.numeric(as.character(LOA$Fgroup_tags))

LOA<-LOA %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

LOASmean<-mean(na.omit(LOA$shrubcover))
LOASsd<-sd(na.omit(LOA$shrubcover))
breaks<-c(-Inf,unique(c(LOASmean-(2*LOASsd),LOASmean+(2*LOASsd))),Inf)
tags<-c("0","1","0")
LOA$Sgroup_tags<-cut(LOA$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Sgroup_tags)
LOA$Sgroup_tags<-as.numeric(as.character(LOA$Sgroup_tags))

LOA<-LOA %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

LOATmean<-mean(na.omit(LOA$treecover))
LOATsd<-sd(na.omit(LOA$treecover))
breaks<-c(-Inf,unique(c(LOATmean-(2*LOATsd),LOATmean+(2*LOATsd))),Inf)
tags<-c("0","1","0")
LOA$Tgroup_tags<-cut(LOA$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Tgroup_tags)
LOA$Tgroup_tags<-as.numeric(as.character(LOA$Tgroup_tags))

LOA<-LOA %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

LOAGHmean<-mean(na.omit(LOA$grassheight))
LOAGHsd<-sd(na.omit(LOA$grassheight))
breaks<-c(-Inf,LOAGHmean-LOAGHsd,LOAGHmean+LOAGHsd,Inf)
tags<-c("0","1","0")
LOA$GHgroup_tags<-cut(LOA$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$GHgroup_tags)
LOA$GHgroup_tags<-as.numeric(as.character(LOA$GHgroup_tags))

LOA<-LOA %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

LOAFHmean<-mean(na.omit(LOA$forb.height))
LOAFHsd<-sd(na.omit(LOA$forb.height))
breaks<-c(-Inf,unique(c(LOAFHmean-(2*LOAFHsd),LOAFHmean+(2*LOAFHsd))),Inf)
tags<-c("0","1","0")
LOA$FHgroup_tags<-cut(LOA$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$FHgroup_tags)
LOA$FHgroup_tags<-as.numeric(as.character(LOA$FHgroup_tags))

LOA<-LOA %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
LOANNmean<-mean(na.omit(LOA$nonnative))
LOANNsd<-sd(na.omit(LOA$nonnative))
breaks<-c(-Inf,unique(LOANNmean-LOANNsd,LOANNmean+LOANNsd),Inf)
breaks
tags<-c("0","1")
LOA$NNgroup_tags<-cut(LOA$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$NNgroup_tags)
LOA$NNgroup_tags<-as.numeric(as.character(LOA$NNgroup_tags))

LOA<-LOA %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

LOAFNmean<-mean(na.omit(LOA$forbnumber))
LOAFNsd<-sd(na.omit(LOA$forbnumber))
breaks<-c(-Inf,unique(c(LOAFNmean-(2*LOAFNsd),LOAFNmean+(2*LOAFNsd))),Inf)
tags<-c("0","1","0")
LOA$FNgroup_tags<-cut(LOA$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$FNgroup_tags)
LOA$FNgroup_tags<-as.numeric(as.character(LOA$FNgroup_tags))
LOA<-LOA %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

LOARmean<-mean(na.omit(LOA$rock))
LOARsd<-sd(na.omit(LOA$rock))
breaks<-c(-Inf,unique(c(LOARmean-(2*LOARsd),LOARmean+(2*LOARsd))),Inf)
tags<-c("0","1","0")
LOA$Rgroup_tags<-cut(LOA$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Rgroup_tags)
LOA$Rgroup_tags<-as.numeric(as.character(LOA$Rgroup_tags))

LOA<-LOA %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

LOALmean<-mean(na.omit(LOA$litter))
LOALsd<-sd(na.omit(LOA$litter))
breaks<-c(-Inf,unique(c(LOALmean-(2*LOALsd),LOALmean+(2*LOALsd))),Inf)
tags<-c("0","1","0")
LOA$Lgroup_tags<-cut(LOA$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$Lgroup_tags)
LOA$Lgroup_tags<-as.numeric(as.character(LOA$Lgroup_tags))

LOA<-LOA %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


LOASUmean<-mean(na.omit(LOA$succulent))
LOASUsd<-sd(na.omit(LOA$succulent))
breaks<-c(-Inf,unique(c(LOASUmean-(2*LOASUsd),LOASUmean+(2*LOASUsd))),Inf)
tags<-c("0","1","0")
LOA$SUgroup_tags<-cut(LOA$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$SUgroup_tags)
LOA$SUgroup_tags<-as.numeric(as.character(LOA$SUgroup_tags))

LOA<-LOA %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

LOAGAmean<-mean(na.omit(LOA$gaps))
LOAGAsd<-sd(na.omit(LOA$gaps))
breaks<-c(-Inf,unique(c(LOAGAmean-(2*LOAGAsd),LOAGAmean+(2*LOAGAsd))),Inf)
tags<-c("0","1","0")
LOA$GAgroup_tags<-cut(LOA$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$GAgroup_tags)
LOA$GAgroup_tags<-as.numeric(as.character(LOA$GAgroup_tags))

LOA<-LOA %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
LOAL<-LOA %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
LOAL<-gather(LOAL,indicator,value,-plotID,na.rm=TRUE)
LOAL<- LOAL %>%
  group_by(plotID) %>%
  mutate(sum=n())
LOAGraph<-LOA
LOAGraph$sum<-LOAL$sum[match(LOAGraph$plotID,LOAL$plotID)]
LOAGraph[is.na(LOAGraph)] <- 0
LOAGraph<-LOAGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
LOAGraph$list<-(1:length(LOAGraph$graph))

LOAB<-ggplot(LOAGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-LOAB+ggtitle("Plots within expected range LOA")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
LOA2<-LOA
LOA2$graph<-LOAGraph$graph[match(LOA2$plotID,LOAGraph$plotID)]
LOA2$graph<-as.numeric(LOA2$graph)
LOA2$graph<-round(LOA2$graph,digits=2)
LOA3<-LOA2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
LOA3<-LOA3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(LOA3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(LOA3)
write.csv(LOA3,"LOAIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
LOAA1<-LOA
LOA2$graph<-as.numeric(LOAGraph$graph)
LOAA1$graph<-round(LOA2$graph,digits=2)
LOAA<-LOAA1 %>%
  select(plotID,allotment,graph)
LOAA<-LOAA%>%
  arrange(allotment)
names(LOAA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(LOAA)

write.csv(LOAA,"LOAallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
LOA$baresoil.bin<-LOA$baresoil.bin*100
LOA$soilstabilitybin<-LOA$soilstabilitybin*100
LOA$treecoverbin<-LOA$treecoverbin*100
LOA$shrubcoverbin<-LOA$shrubcoverbin*100
LOA$grasscoverbin<-LOA$grasscoverbin*100
LOA$grassheightbin<-LOA$grassheightbin*100
LOA$forb.heightbin<-LOA$forb.heightbin*100
LOA$forbcoverbin<-LOA$forbcoverbin*100
LOA$gapsbin<-LOA$gapsbin*100
LOA$nonnativebin<-LOA$nonnativebin*100
LOA$nativebin<-LOA$nativebin*100
LOA$succulentbin<-LOA$succulentbin*100
LOA$litterbin<-LOA$litterbin*100
LOA$rockbin<-LOA$rockbin*100
LOA$forbnumberbin<-LOA$forbnumberbin*100
LOA$graph<-LOAGraph$graph[match(LOA$plotID,LOAGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
LOA$SUMgroup_tags<-cut(LOA$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(LOA$SUMgroup_tags)
LOA$SUMgroup_tags<-as.numeric(as.character(LOA$SUMgroup_tags))

LOA<-LOA %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
LOAR<-LOA
LOA<-LOA[!duplicated(LOA$nativebin),]
stratablanks<-rep("",15)
Strata<-c("LOA",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(LOA$baresoil.bin,digits=2),round(LOA$soilstabilitybin,digits=2),round(LOA$litterbin,digits=2),round(LOA$nativebin,digits=2),round(LOA$grasscoverbin,digits=2),round(LOA$forbcoverbin,digits=2),
                   round(LOA$shrubcoverbin,digits=2),round(LOA$nonnativebin,digits=2),round(LOA$forbnumberbin,digits=2),round(LOA$rockbin,digits=2),
                   round(LOA$treecoverbin,digits=2),round(LOA$succulentbin,digits=2),round(LOA$gapsbin,digits=2),round(LOA$forb.heightbin,digits=2),round(LOA$grassheightbin,digits=2),round(LOA$SUMbin,digits=2))
LOAdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(LOAdataframe)
#to write csv
write.csv(LOAdataframe,"LOAPercentofIndicatorsMeeting.csv")
#MO
MO<-subset(bench,Actual.Eco.Site=="MO")
MOmean<-mean(na.omit(MO$baresoil))
MOsd<-sd(na.omit(MO$baresoil))
breaks<-c(-Inf,unique(c(MOmean-(2*MOsd),MOmean+(2*MOsd))),Inf)
tags<-c("0","1","0")
MO$BSgroup_tags<-cut(MO$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$BSgroup_tags)
MO$BSgroup_tags<-as.numeric(as.character(MO$BSgroup_tags))

MO<-MO %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

MOSSmean<-mean(na.omit(MO$soilstability))
MOSSsd<-sd(na.omit(MO$soilstability))
breaks<-c(-Inf,unique(c(MOSSmean-(2*MOSSsd),MOSSmean+(2*MOSSsd))),Inf)
breaks
tags<-c("0","1","0")
MO$SSgroup_tags<-cut(MO$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$SSgroup_tags)
MO$SSgroup_tags<-as.numeric(as.character(MO$SSgroup_tags))

MO<-MO %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

MONmean<-mean(na.omit(MO$native))
MONsd<-sd(na.omit(MO$native))
breaks<-c(-Inf,unique(c(MONmean-(2*MONsd),MONmean+(2*MONsd))),Inf)
tags<-c("0","1","0")
MO$Ngroup_tags<-cut(MO$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Ngroup_tags)
MO$Ngroup_tags<-as.numeric(as.character(MO$Ngroup_tags))

MO<-MO %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


MOGmean<-mean(na.omit(MO$grasscover))
MOGsd<-sd(na.omit(MO$grasscover))
breaks<-c(-Inf,unique(c(MOGmean-(2*MOGsd),MOGmean+(2*MOGsd))),Inf)
tags<-c("0","1","0")
MO$Ggroup_tags<-cut(MO$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Ggroup_tags)
MO$Ggroup_tags<-as.numeric(as.character(MO$Ggroup_tags))

MO<-MO %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


MOFmean<-mean(na.omit(MO$forbcover))
MOFsd<-sd(na.omit(MO$forbcover))
breaks<-c(-Inf,unique(c(MOFmean-(2*MOFsd),MOFmean+(2*MOFsd))),Inf)
tags<-c("0","1","0")
MO$Fgroup_tags<-cut(MO$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Fgroup_tags)
MO$Fgroup_tags<-as.numeric(as.character(MO$Fgroup_tags))

MO<-MO %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

MOSmean<-mean(na.omit(MO$shrubcover))
MOSsd<-sd(na.omit(MO$shrubcover))
breaks<-c(-Inf,unique(c(MOSmean-(2*MOSsd),MOSmean+(2*MOSsd))),Inf)
tags<-c("0","1","0")
MO$Sgroup_tags<-cut(MO$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Sgroup_tags)
MO$Sgroup_tags<-as.numeric(as.character(MO$Sgroup_tags))

MO<-MO %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

MOTmean<-mean(na.omit(MO$treecover))
MOTsd<-sd(na.omit(MO$treecover))
breaks<-c(-Inf,unique(c(MOTmean-(2*MOTsd),MOTmean+(2*MOTsd))),Inf)
tags<-c("0","1","0")
MO$Tgroup_tags<-cut(MO$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Tgroup_tags)
MO$Tgroup_tags<-as.numeric(as.character(MO$Tgroup_tags))

MO<-MO %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

MOGHmean<-mean(na.omit(MO$grassheight))
MOGHsd<-sd(na.omit(MO$grassheight))
breaks<-c(-Inf,MOGHmean-MOGHsd,MOGHmean+MOGHsd,Inf)
tags<-c("0","1","0")
MO$GHgroup_tags<-cut(MO$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$GHgroup_tags)
MO$GHgroup_tags<-as.numeric(as.character(MO$GHgroup_tags))

MO<-MO %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

MOFHmean<-mean(na.omit(MO$forb.height))
MOFHsd<-sd(na.omit(MO$forb.height))
breaks<-c(-Inf,unique(c(MOFHmean-(2*MOFHsd),MOFHmean+(2*MOFHsd))),Inf)
tags<-c("0","1","0")
MO$FHgroup_tags<-cut(MO$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$FHgroup_tags)
MO$FHgroup_tags<-as.numeric(as.character(MO$FHgroup_tags))

MO<-MO %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
MONNmean<-mean(na.omit(MO$nonnative))
MONNsd<-sd(na.omit(MO$nonnative))
breaks<-c(-Inf,unique(MONNmean-MONNsd,MONNmean+MONNsd),Inf)
breaks
tags<-c("0","1")
MO$NNgroup_tags<-cut(MO$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$NNgroup_tags)
MO$NNgroup_tags<-as.numeric(as.character(MO$NNgroup_tags))

MO<-MO %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

MOFNmean<-mean(na.omit(MO$forbnumber))
MOFNsd<-sd(na.omit(MO$forbnumber))
breaks<-c(-Inf,unique(c(MOFNmean-(2*MOFNsd),MOFNmean+(2*MOFNsd))),Inf)
tags<-c("0","1","0")
MO$FNgroup_tags<-cut(MO$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$FNgroup_tags)
MO$FNgroup_tags<-as.numeric(as.character(MO$FNgroup_tags))
MO<-MO %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

MORmean<-mean(na.omit(MO$rock))
MORsd<-sd(na.omit(MO$rock))
breaks<-c(-Inf,unique(c(MORmean-(2*MORsd),MORmean+(2*MORsd))),Inf)
tags<-c("0","1","0")
MO$Rgroup_tags<-cut(MO$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Rgroup_tags)
MO$Rgroup_tags<-as.numeric(as.character(MO$Rgroup_tags))

MO<-MO %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

MOLmean<-mean(na.omit(MO$litter))
MOLsd<-sd(na.omit(MO$litter))
breaks<-c(-Inf,unique(c(MOLmean-(2*MOLsd),MOLmean+(2*MOLsd))),Inf)
tags<-c("0","1","0")
MO$Lgroup_tags<-cut(MO$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$Lgroup_tags)
MO$Lgroup_tags<-as.numeric(as.character(MO$Lgroup_tags))

MO<-MO %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


MOSUmean<-mean(na.omit(MO$succulent))
MOSUsd<-sd(na.omit(MO$succulent))
breaks<-c(-Inf,unique(c(MOSUmean-(2*MOSUsd),MOSUmean+(2*MOSUsd))),Inf)
tags<-c("0","1","0")
MO$SUgroup_tags<-cut(MO$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$SUgroup_tags)
MO$SUgroup_tags<-as.numeric(as.character(MO$SUgroup_tags))

MO<-MO %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

MOGAmean<-mean(na.omit(MO$gaps))
MOGAsd<-sd(na.omit(MO$gaps))
breaks<-c(-Inf,unique(c(MOGAmean-(2*MOGAsd),MOGAmean+(2*MOGAsd))),Inf)
tags<-c("0","1","0")
MO$GAgroup_tags<-cut(MO$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$GAgroup_tags)
MO$GAgroup_tags<-as.numeric(as.character(MO$GAgroup_tags))

MO<-MO %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
MOL<-MO %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
MOL<-gather(MOL,indicator,value,-plotID,na.rm=TRUE)
MOL<- MOL %>%
  group_by(plotID) %>%
  mutate(sum=n())
MOGraph<-MO
MOGraph$sum<-MOL$sum[match(MOGraph$plotID,MOL$plotID)]
MOGraph[is.na(MOGraph)] <- 0
MOGraph<-MOGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
MOGraph$list<-(1:length(MOGraph$graph))

MOB<-ggplot(MOGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-MOB+ggtitle("Plots within expected range MO")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
MO2<-MO
MO2$graph<-MOGraph$graph[match(MO2$plotID,MOGraph$plotID)]
MO2$graph<-as.numeric(MO2$graph)
MO2$graph<-round(MO2$graph,digits=2)
MO3<-MO2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
MO3<-MO3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(MO3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(MO3)
write.csv(MO3,"MOIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
MOA1<-MO
MO2$graph<-as.numeric(MO2$graph)
MOA1$graph<-round(MO2$graph,digits=2)
MOA<-MOA1 %>%
  select(plotID,allotment,graph)
MOA<-MOA%>%
  arrange(allotment)
names(MOA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(MOA)

write.csv(MOA,"MOallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
MO$baresoil.bin<-MO$baresoil.bin*100
MO$soilstabilitybin<-MO$soilstabilitybin*100
MO$treecoverbin<-MO$treecoverbin*100
MO$shrubcoverbin<-MO$shrubcoverbin*100
MO$grasscoverbin<-MO$grasscoverbin*100
MO$grassheightbin<-MO$grassheightbin*100
MO$forb.heightbin<-MO$forb.heightbin*100
MO$forbcoverbin<-MO$forbcoverbin*100
MO$gapsbin<-MO$gapsbin*100
MO$nonnativebin<-MO$nonnativebin*100
MO$nativebin<-MO$nativebin*100
MO$succulentbin<-MO$succulentbin*100
MO$litterbin<-MO$litterbin*100
MO$rockbin<-MO$rockbin*100
MO$forbnumberbin<-MO$forbnumberbin*100
MO$graph<-MOGraph$graph[match(MO$plotID,MOGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
MO$SUMgroup_tags<-cut(MO$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(MO$SUMgroup_tags)
MO$SUMgroup_tags<-as.numeric(as.character(MO$SUMgroup_tags))

MO<-MO %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
MOR<-MO
MO<-MO[!duplicated(MO$nativebin),]
stratablanks<-rep("",15)
Strata<-c("MO",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting")
Percent.Meeting<-c(round(MO$baresoil.bin,digits=2),round(MO$soilstabilitybin,digits=2),round(MO$litterbin,digits=2),round(MO$nativebin,digits=2),round(MO$grasscoverbin,digits=2),round(MO$forbcoverbin,digits=2),
                   round(MO$shrubcoverbin,digits=2),round(MO$nonnativebin,digits=2),round(MO$forbnumberbin,digits=2),round(MO$rockbin,digits=2),
                   round(MO$treecoverbin,digits=2),round(MO$succulentbin,digits=2),round(MO$gapsbin,digits=2),round(MO$forb.heightbin,digits=2),round(MO$grassheightbin,digits=2),round(MO$SUMbin,digits=2))
MOdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(MOdataframe)
#to write csv
write.csv(MOdataframe,"MOPercentofIndicatorsMeeting.csv")


#OTH
OTH<-subset(bench,Actual.Eco.Site=="OTH")
OTHmean<-mean(na.omit(OTH$baresoil))
OTHsd<-sd(na.omit(OTH$baresoil))
breaks<-c(-Inf,unique(c(OTHmean-(2*OTHsd),OTHmean+(2*OTHsd))),Inf)
tags<-c("0","1","0")
OTH$BSgroup_tags<-cut(OTH$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$BSgroup_tags)
OTH$BSgroup_tags<-as.numeric(as.character(OTH$BSgroup_tags))

OTH<-OTH %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

OTHSSmean<-mean(na.omit(OTH$soilstability))
OTHSSsd<-sd(na.omit(OTH$soilstability))
breaks<-c(-Inf,unique(c(OTHSSmean-(2*OTHSSsd),OTHSSmean+(2*OTHSSsd))),Inf)
breaks
tags<-c("0","1","0")
OTH$SSgroup_tags<-cut(OTH$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$SSgroup_tags)
OTH$SSgroup_tags<-as.numeric(as.character(OTH$SSgroup_tags))

OTH<-OTH %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

OTHNmean<-mean(na.omit(OTH$native))
OTHNsd<-sd(na.omit(OTH$native))
breaks<-c(-Inf,unique(c(OTHNmean-(2*OTHNsd),OTHNmean+(2*OTHNsd))),Inf)
tags<-c("0","1","0")
OTH$Ngroup_tags<-cut(OTH$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Ngroup_tags)
OTH$Ngroup_tags<-as.numeric(as.character(OTH$Ngroup_tags))

OTH<-OTH %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


OTHGmean<-mean(na.omit(OTH$grasscover))
OTHGsd<-sd(na.omit(OTH$grasscover))
breaks<-c(-Inf,unique(c(OTHGmean-(2*OTHGsd),OTHGmean+(2*OTHGsd))),Inf)
tags<-c("0","1","0")
OTH$Ggroup_tags<-cut(OTH$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Ggroup_tags)
OTH$Ggroup_tags<-as.numeric(as.character(OTH$Ggroup_tags))

OTH<-OTH %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


OTHFmean<-mean(na.omit(OTH$forbcover))
OTHFsd<-sd(na.omit(OTH$forbcover))
breaks<-c(-Inf,unique(c(OTHFmean-(2*OTHFsd),OTHFmean+(2*OTHFsd))),Inf)
tags<-c("0","1","0")
OTH$Fgroup_tags<-cut(OTH$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Fgroup_tags)
OTH$Fgroup_tags<-as.numeric(as.character(OTH$Fgroup_tags))

OTH<-OTH %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

OTHSmean<-mean(na.omit(OTH$shrubcover))
OTHSsd<-sd(na.omit(OTH$shrubcover))
breaks<-c(-Inf,unique(c(OTHSmean-(2*OTHSsd),OTHSmean+(2*OTHSsd))),Inf)
tags<-c("0","1","0")
OTH$Sgroup_tags<-cut(OTH$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Sgroup_tags)
OTH$Sgroup_tags<-as.numeric(as.character(OTH$Sgroup_tags))

OTH<-OTH %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

OTHTmean<-mean(na.omit(OTH$treecover))
OTHTsd<-sd(na.omit(OTH$treecover))
breaks<-c(-Inf,unique(c(OTHTmean-(2*OTHTsd),OTHTmean+(2*OTHTsd))),Inf)
tags<-c("0","1","0")
OTH$Tgroup_tags<-cut(OTH$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Tgroup_tags)
OTH$Tgroup_tags<-as.numeric(as.character(OTH$Tgroup_tags))

OTH<-OTH %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

OTHGHmean<-mean(na.omit(OTH$grassheight))
OTHGHsd<-sd(na.omit(OTH$grassheight))
breaks<-c(-Inf,OTHGHmean-OTHGHsd,OTHGHmean+OTHGHsd,Inf)
tags<-c("0","1","0")
OTH$GHgroup_tags<-cut(OTH$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$GHgroup_tags)
OTH$GHgroup_tags<-as.numeric(as.character(OTH$GHgroup_tags))

OTH<-OTH %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

OTHFHmean<-mean(na.omit(OTH$forb.height))
OTHFHsd<-sd(na.omit(OTH$forb.height))
breaks<-c(-Inf,unique(c(OTHFHmean-(2*OTHFHsd),OTHFHmean+(2*OTHFHsd))),Inf)
tags<-c("0","1","0")
OTH$FHgroup_tags<-cut(OTH$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$FHgroup_tags)
OTH$FHgroup_tags<-as.numeric(as.character(OTH$FHgroup_tags))

OTH<-OTH %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
OTHNNmean<-mean(na.omit(OTH$nonnative))
OTHNNsd<-sd(na.omit(OTH$nonnative))
breaks<-c(-Inf,unique(OTHNNmean-OTHNNsd,OTHNNmean+OTHNNsd),Inf)
breaks
tags<-c("0","1")
OTH$NNgroup_tags<-cut(OTH$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$NNgroup_tags)
OTH$NNgroup_tags<-as.numeric(as.character(OTH$NNgroup_tags))

OTH<-OTH %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

OTHFNmean<-mean(na.omit(OTH$forbnumber))
OTHFNsd<-sd(na.omit(OTH$forbnumber))
breaks<-c(-Inf,unique(c(OTHFNmean-(2*OTHFNsd),OTHFNmean+(2*OTHFNsd))),Inf)
tags<-c("0","1","0")
OTH$FNgroup_tags<-cut(OTH$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$FNgroup_tags)
OTH$FNgroup_tags<-as.numeric(as.character(OTH$FNgroup_tags))
OTH<-OTH %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

OTHRmean<-mean(na.omit(OTH$rock))
OTHRsd<-sd(na.omit(OTH$rock))
breaks<-c(-Inf,unique(c(OTHRmean-(2*OTHRsd),OTHRmean+(2*OTHRsd))),Inf)
tags<-c("0","1","0")
OTH$Rgroup_tags<-cut(OTH$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Rgroup_tags)
OTH$Rgroup_tags<-as.numeric(as.character(OTH$Rgroup_tags))

OTH<-OTH %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

OTHLmean<-mean(na.omit(OTH$litter))
OTHLsd<-sd(na.omit(OTH$litter))
breaks<-c(-Inf,unique(c(OTHLmean-(2*OTHLsd),OTHLmean+(2*OTHLsd))),Inf)
tags<-c("0","1","0")
OTH$Lgroup_tags<-cut(OTH$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$Lgroup_tags)
OTH$Lgroup_tags<-as.numeric(as.character(OTH$Lgroup_tags))

OTH<-OTH %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


OTHSUmean<-mean(na.omit(OTH$succulent))
OTHSUsd<-sd(na.omit(OTH$succulent))
breaks<-c(-Inf,unique(c(OTHSUmean-(2*OTHSUsd),OTHSUmean+(2*OTHSUsd))),Inf)
tags<-c("0","1","0")
OTH$SUgroup_tags<-cut(OTH$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$SUgroup_tags)
OTH$SUgroup_tags<-as.numeric(as.character(OTH$SUgroup_tags))

OTH<-OTH %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

OTHGAmean<-mean(na.omit(OTH$gaps))
OTHGAsd<-sd(na.omit(OTH$gaps))
breaks<-c(-Inf,unique(c(OTHGAmean-(2*OTHGAsd),OTHGAmean+(2*OTHGAsd))),Inf)
tags<-c("0","1","0")
OTH$GAgroup_tags<-cut(OTH$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$GAgroup_tags)
OTH$GAgroup_tags<-as.numeric(as.character(OTH$GAgroup_tags))

OTH<-OTH %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
OTHL<-OTH %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
OTHL<-gather(OTHL,indicator,value,-plotID,na.rm=TRUE)
OTHL<- OTHL %>%
  group_by(plotID) %>%
  mutate(sum=n())
OTHGraph<-OTH
OTHGraph$sum<-OTHL$sum[match(OTHGraph$plotID,OTHL$plotID)]
OTHGraph[is.na(OTHGraph)] <- 0
OTHGraph<-OTHGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
OTHGraph$list<-(1:length(OTHGraph$graph))

OTHB<-ggplot(OTHGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-OTHB+ggtitle("Plots within expected range OTH")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
OTH2<-OTH
OTH2$graph<-OTHGraph$graph[match(OTH2$plotID,OTHGraph$plotID)]
OTH2$graph<-as.numeric(OTH2$graph)
OTH2$graph<-round(OTH2$graph,digits=2)
OTH3<-OTH2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
OTH3<-OTH3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(OTH3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(OTH3)
write.csv(OTH3,"OTHIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
OTHA1<-OTH
OTH2$graph<-as.numeric(OTHGraph$graph)
OTHA1$graph<-round(OTH2$graph,digits=2)
OTHA<-OTHA1 %>%
  select(plotID,allotment,graph)
OTHA<-OTHA%>%
  arrange(allotment)
names(OTHA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(OTHA)

write.csv(OTHA,"OTHallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
OTH$baresoil.bin<-OTH$baresoil.bin*100
OTH$soilstabilitybin<-OTH$soilstabilitybin*100
OTH$treecoverbin<-OTH$treecoverbin*100
OTH$shrubcoverbin<-OTH$shrubcoverbin*100
OTH$grasscoverbin<-OTH$grasscoverbin*100
OTH$grassheightbin<-OTH$grassheightbin*100
OTH$forb.heightbin<-OTH$forb.heightbin*100
OTH$forbcoverbin<-OTH$forbcoverbin*100
OTH$gapsbin<-OTH$gapsbin*100
OTH$nonnativebin<-OTH$nonnativebin*100
OTH$nativebin<-OTH$nativebin*100
OTH$succulentbin<-OTH$succulentbin*100
OTH$litterbin<-OTH$litterbin*100
OTH$rockbin<-OTH$rockbin*100
OTH$forbnumberbin<-OTH$forbnumberbin*100
OTH$graph<-OTHGraph$graph[match(OTH$plotID,OTHGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
OTH$SUMgroup_tags<-cut(OTH$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(OTH$SUMgroup_tags)
OTH$SUMgroup_tags<-as.numeric(as.character(OTH$SUMgroup_tags))

OTH<-OTH %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
OTHR<-OTH
OTH<-OTH[!duplicated(OTH$nativebin),]
stratablanks<-rep("",15)
Strata<-c("OTH",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(OTH$baresoil.bin,digits=2),round(OTH$soilstabilitybin,digits=2),round(OTH$litterbin,digits=2),round(OTH$nativebin,digits=2),round(OTH$grasscoverbin,digits=2),round(OTH$forbcoverbin,digits=2),
                   round(OTH$shrubcoverbin,digits=2),round(OTH$nonnativebin,digits=2),round(OTH$forbnumberbin,digits=2),round(OTH$rockbin,digits=2),
                   round(OTH$treecoverbin,digits=2),round(OTH$succulentbin,digits=2),round(OTH$gapsbin,digits=2),round(OTH$forb.heightbin,digits=2),round(OTH$grassheightbin,digits=2),round(OTH$SUMbin,digits=2))
OTHdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(OTHdataframe)
#to wrtie csv
write.csv(OTHdataframe,"OTHPercentofIndicatorsMeeting.csv")
#ROF
ROF<-subset(bench,Actual.Eco.Site=="ROF")
ROFmean<-mean(na.omit(ROF$baresoil))
ROFsd<-sd(na.omit(ROF$baresoil))
breaks<-c(-Inf,unique(c(ROFmean-(2*ROFsd),ROFmean+(2*ROFsd))),Inf)
tags<-c("0","1","0")
ROF$BSgroup_tags<-cut(ROF$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$BSgroup_tags)
ROF$BSgroup_tags<-as.numeric(as.character(ROF$BSgroup_tags))

ROF<-ROF %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

ROFSSmean<-mean(na.omit(ROF$soilstability))
ROFSSsd<-sd(na.omit(ROF$soilstability))
breaks<-c(-Inf,unique(c(ROFSSmean-(2*ROFSSsd),ROFSSmean+(2*ROFSSsd))),Inf)
breaks
tags<-c("0","1","0")
ROF$SSgroup_tags<-cut(ROF$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$SSgroup_tags)
ROF$SSgroup_tags<-as.numeric(as.character(ROF$SSgroup_tags))

ROF<-ROF %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

ROFNmean<-mean(na.omit(ROF$native))
ROFNsd<-sd(na.omit(ROF$native))
breaks<-c(-Inf,unique(c(ROFNmean-(2*ROFNsd),ROFNmean+(2*ROFNsd))),Inf)
tags<-c("0","1","0")
ROF$Ngroup_tags<-cut(ROF$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Ngroup_tags)
ROF$Ngroup_tags<-as.numeric(as.character(ROF$Ngroup_tags))

ROF<-ROF %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


ROFGmean<-mean(na.omit(ROF$grasscover))
ROFGsd<-sd(na.omit(ROF$grasscover))
breaks<-c(-Inf,unique(c(ROFGmean-(2*ROFGsd),ROFGmean+(2*ROFGsd))),Inf)
tags<-c("0","1","0")
ROF$Ggroup_tags<-cut(ROF$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Ggroup_tags)
ROF$Ggroup_tags<-as.numeric(as.character(ROF$Ggroup_tags))

ROF<-ROF %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


ROFFmean<-mean(na.omit(ROF$forbcover))
ROFFsd<-sd(na.omit(ROF$forbcover))
breaks<-c(-Inf,unique(c(ROFFmean-(2*ROFFsd),ROFFmean+(2*ROFFsd))),Inf)
tags<-c("0","1","0")
ROF$Fgroup_tags<-cut(ROF$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Fgroup_tags)
ROF$Fgroup_tags<-as.numeric(as.character(ROF$Fgroup_tags))

ROF<-ROF %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

ROFSmean<-mean(na.omit(ROF$shrubcover))
ROFSsd<-sd(na.omit(ROF$shrubcover))
breaks<-c(-Inf,unique(c(ROFSmean-(2*ROFSsd),ROFSmean+(2*ROFSsd))),Inf)
tags<-c("0","1","0")
ROF$Sgroup_tags<-cut(ROF$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Sgroup_tags)
ROF$Sgroup_tags<-as.numeric(as.character(ROF$Sgroup_tags))

ROF<-ROF %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

ROFTmean<-mean(na.omit(ROF$treecover))
ROFTsd<-sd(na.omit(ROF$treecover))
breaks<-c(-Inf,unique(c(ROFTmean-(2*ROFTsd),ROFTmean+(2*ROFTsd))),Inf)
tags<-c("0","1","0")
ROF$Tgroup_tags<-cut(ROF$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Tgroup_tags)
ROF$Tgroup_tags<-as.numeric(as.character(ROF$Tgroup_tags))

ROF<-ROF %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

ROFGHmean<-mean(na.omit(ROF$grassheight))
ROFGHsd<-sd(na.omit(ROF$grassheight))
breaks<-c(-Inf,ROFGHmean-ROFGHsd,ROFGHmean+ROFGHsd,Inf)
tags<-c("0","1","0")
ROF$GHgroup_tags<-cut(ROF$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$GHgroup_tags)
ROF$GHgroup_tags<-as.numeric(as.character(ROF$GHgroup_tags))

ROF<-ROF %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

ROFFHmean<-mean(na.omit(ROF$forb.height))
ROFFHsd<-sd(na.omit(ROF$forb.height))
breaks<-c(-Inf,unique(c(ROFFHmean-(2*ROFFHsd),ROFFHmean+(2*ROFFHsd))),Inf)
tags<-c("0","1","0")
ROF$FHgroup_tags<-cut(ROF$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$FHgroup_tags)
ROF$FHgroup_tags<-as.numeric(as.character(ROF$FHgroup_tags))

ROF<-ROF %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
ROFNNmean<-mean(na.omit(ROF$nonnative))
ROFNNsd<-sd(na.omit(ROF$nonnative))
breaks<-c(-Inf,unique(ROFNNmean-ROFNNsd,ROFNNmean+ROFNNsd),Inf)
breaks
tags<-c("0","1")
ROF$NNgroup_tags<-cut(ROF$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$NNgroup_tags)
ROF$NNgroup_tags<-as.numeric(as.character(ROF$NNgroup_tags))

ROF<-ROF %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

ROFFNmean<-mean(na.omit(ROF$forbnumber))
ROFFNsd<-sd(na.omit(ROF$forbnumber))
breaks<-c(-Inf,unique(c(ROFFNmean-(2*ROFFNsd),ROFFNmean+(2*ROFFNsd))),Inf)
tags<-c("0","1","0")
ROF$FNgroup_tags<-cut(ROF$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$FNgroup_tags)
ROF$FNgroup_tags<-as.numeric(as.character(ROF$FNgroup_tags))
ROF<-ROF %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

ROFRmean<-mean(na.omit(ROF$rock))
ROFRsd<-sd(na.omit(ROF$rock))
breaks<-c(-Inf,unique(c(ROFRmean-(2*ROFRsd),ROFRmean+(2*ROFRsd))),Inf)
tags<-c("0","1","0")
ROF$Rgroup_tags<-cut(ROF$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Rgroup_tags)
ROF$Rgroup_tags<-as.numeric(as.character(ROF$Rgroup_tags))

ROF<-ROF %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

ROFLmean<-mean(na.omit(ROF$litter))
ROFLsd<-sd(na.omit(ROF$litter))
breaks<-c(-Inf,unique(c(ROFLmean-(2*ROFLsd),ROFLmean+(2*ROFLsd))),Inf)
tags<-c("0","1","0")
ROF$Lgroup_tags<-cut(ROF$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$Lgroup_tags)
ROF$Lgroup_tags<-as.numeric(as.character(ROF$Lgroup_tags))

ROF<-ROF %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


ROFSUmean<-mean(na.omit(ROF$succulent))
ROFSUsd<-sd(na.omit(ROF$succulent))
breaks<-c(-Inf,unique(c(ROFSUmean-(2*ROFSUsd),ROFSUmean+(2*ROFSUsd))),Inf)
tags<-c("0","1","0")
ROF$SUgroup_tags<-cut(ROF$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$SUgroup_tags)
ROF$SUgroup_tags<-as.numeric(as.character(ROF$SUgroup_tags))

ROF<-ROF %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

ROFGAmean<-mean(na.omit(ROF$gaps))
ROFGAsd<-sd(na.omit(ROF$gaps))
breaks<-c(-Inf,unique(c(ROFGAmean-(2*ROFGAsd),ROFGAmean+(2*ROFGAsd))),Inf)
tags<-c("0","1","0")
ROF$GAgroup_tags<-cut(ROF$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$GAgroup_tags)
ROF$GAgroup_tags<-as.numeric(as.character(ROF$GAgroup_tags))

ROF<-ROF %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
ROFL<-ROF %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
ROFL<-gather(ROFL,indicator,value,-plotID,na.rm=TRUE)
ROFL<- ROFL %>%
  group_by(plotID) %>%
  mutate(sum=n())
ROFGraph<-ROF
ROFGraph$sum<-ROFL$sum[match(ROFGraph$plotID,ROFL$plotID)]
ROFGraph[is.na(ROFGraph)] <- 0
ROFGraph<-ROFGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
ROFGraph$list<-(1:length(ROFGraph$graph))

ROFB<-ggplot(ROFGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-ROFB+ggtitle("Plots within expected range ROF")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
ROF2<-ROF
ROF2$graph<-ROFGraph$graph[match(ROF2$plotID,ROFGraph$plotID)]
ROF2$graph<-as.numeric(ROF2$graph)
ROF2$graph<-round(ROF2$graph,digits=2)
ROF3<-ROF2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
ROF3<-ROF3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(ROF3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(ROF3)
write.csv(ROF3,"ROFIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
ROFA1<-ROF
ROF2$graph<-ROFGraph[match(ROF2$plotID,ROFGraph$plotID)]
ROF2$graph<-as.numeric(ROFGraph$graph)
ROFA1$graph<-round(ROF2$graph,digits=2)
ROFA<-ROFA1 %>%
  select(plotID,allotment,graph)
ROFA<-ROFA%>%
  arrange(allotment)
names(ROFA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(ROFA)

write.csv(ROFA,"ROFallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
ROF$baresoil.bin<-ROF$baresoil.bin*100
ROF$soilstabilitybin<-ROF$soilstabilitybin*100
ROF$treecoverbin<-ROF$treecoverbin*100
ROF$shrubcoverbin<-ROF$shrubcoverbin*100
ROF$grasscoverbin<-ROF$grasscoverbin*100
ROF$grassheightbin<-ROF$grassheightbin*100
ROF$forb.heightbin<-ROF$forb.heightbin*100
ROF$forbcoverbin<-ROF$forbcoverbin*100
ROF$gapsbin<-ROF$gapsbin*100
ROF$nonnativebin<-ROF$nonnativebin*100
ROF$nativebin<-ROF$nativebin*100
ROF$succulentbin<-ROF$succulentbin*100
ROF$litterbin<-ROF$litterbin*100
ROF$rockbin<-ROF$rockbin*100
ROF$forbnumberbin<-ROF$forbnumberbin*100
ROF$graph<-ROFGraph$graph[match(ROF$plotID,ROFGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
ROF$SUMgroup_tags<-cut(ROF$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(ROF$SUMgroup_tags)
ROF$SUMgroup_tags<-as.numeric(as.character(ROF$SUMgroup_tags))

ROF<-ROF %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
ROFR<-ROF
ROF<-ROF[!duplicated(ROF$nativebin),]
stratablanks<-rep("",15)
Strata<-c("ROF",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(ROF$baresoil.bin,digits=2),round(ROF$soilstabilitybin,digits=2),round(ROF$litterbin,digits=2),round(ROF$nativebin,digits=2),round(ROF$grasscoverbin,digits=2),round(ROF$forbcoverbin,digits=2),
                   round(ROF$shrubcoverbin,digits=2),round(ROF$nonnativebin,digits=2),round(ROF$forbnumberbin,digits=2),round(ROF$rockbin,digits=2),
                   round(ROF$treecoverbin,digits=2),round(ROF$succulentbin,digits=2),round(ROF$gapsbin,digits=2),round(ROF$forb.heightbin,digits=2),round(ROF$grassheightbin,digits=2),round(ROF$SUMbin,digits=2))
ROFdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(ROFdataframe)
#to write csv
write.csv(ROFdataframe,"ROFPercentofIndicatorsMeeting.csv")
#SAL
SAL<-subset(bench,Actual.Eco.Site=="SAL")
SALmean<-mean(na.omit(SAL$baresoil))
SALsd<-sd(na.omit(SAL$baresoil))
breaks<-c(-Inf,unique(c(SALmean-(2*SALsd),SALmean+(2*SALsd))),Inf)
tags<-c("0","1","0")
SAL$BSgroup_tags<-cut(SAL$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$BSgroup_tags)
SAL$BSgroup_tags<-as.numeric(as.character(SAL$BSgroup_tags))

SAL<-SAL %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

SALSSmean<-mean(na.omit(SAL$soilstability))
SALSSsd<-sd(na.omit(SAL$soilstability))
breaks<-c(-Inf,unique(c(SALSSmean-(2*SALSSsd),SALSSmean+(2*SALSSsd))),Inf)
breaks
tags<-c("0","1","0")
SAL$SSgroup_tags<-cut(SAL$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$SSgroup_tags)
SAL$SSgroup_tags<-as.numeric(as.character(SAL$SSgroup_tags))

SAL<-SAL %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

SALNmean<-mean(na.omit(SAL$native))
SALNsd<-sd(na.omit(SAL$native))
breaks<-c(-Inf,unique(c(SALNmean-(2*SALNsd),SALNmean+(2*SALNsd))),Inf)
tags<-c("0","1","0")
SAL$Ngroup_tags<-cut(SAL$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Ngroup_tags)
SAL$Ngroup_tags<-as.numeric(as.character(SAL$Ngroup_tags))

SAL<-SAL %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


SALGmean<-mean(na.omit(SAL$grasscover))
SALGsd<-sd(na.omit(SAL$grasscover))
breaks<-c(-Inf,unique(c(SALGmean-(2*SALGsd),SALGmean+(2*SALGsd))),Inf)
tags<-c("0","1","0")
SAL$Ggroup_tags<-cut(SAL$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Ggroup_tags)
SAL$Ggroup_tags<-as.numeric(as.character(SAL$Ggroup_tags))

SAL<-SAL %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


SALFmean<-mean(na.omit(SAL$forbcover))
SALFsd<-sd(na.omit(SAL$forbcover))
breaks<-c(-Inf,unique(c(SALFmean-(2*SALFsd),SALFmean+(2*SALFsd))),Inf)
tags<-c("0","1","0")
SAL$Fgroup_tags<-cut(SAL$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Fgroup_tags)
SAL$Fgroup_tags<-as.numeric(as.character(SAL$Fgroup_tags))

SAL<-SAL %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

SALSmean<-mean(na.omit(SAL$shrubcover))
SALSsd<-sd(na.omit(SAL$shrubcover))
breaks<-c(-Inf,unique(c(SALSmean-(2*SALSsd),SALSmean+(2*SALSsd))),Inf)
tags<-c("0","1","0")
SAL$Sgroup_tags<-cut(SAL$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Sgroup_tags)
SAL$Sgroup_tags<-as.numeric(as.character(SAL$Sgroup_tags))

SAL<-SAL %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))
##had to change tags due to only 0 values
SALTmean<-mean(na.omit(SAL$treecover))
SALTsd<-sd(na.omit(SAL$treecover))
breaks<-c(-Inf,unique(c(SALTmean-(2*SALTsd),SALTmean+(2*SALTsd))),Inf)
tags<-c("0","1")
SAL$Tgroup_tags<-cut(SAL$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Tgroup_tags)
SAL$Tgroup_tags<-as.numeric(as.character(SAL$Tgroup_tags))

SAL<-SAL %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

SALGHmean<-mean(na.omit(SAL$grassheight))
SALGHsd<-sd(na.omit(SAL$grassheight))
breaks<-c(-Inf,SALGHmean-SALGHsd,SALGHmean+SALGHsd,Inf)
tags<-c("0","1","0")
SAL$GHgroup_tags<-cut(SAL$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$GHgroup_tags)
SAL$GHgroup_tags<-as.numeric(as.character(SAL$GHgroup_tags))

SAL<-SAL %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

SALFHmean<-mean(na.omit(SAL$forb.height))
SALFHsd<-sd(na.omit(SAL$forb.height))
breaks<-c(-Inf,unique(c(SALFHmean-(2*SALFHsd),SALFHmean+(2*SALFHsd))),Inf)
tags<-c("0","1","0")
SAL$FHgroup_tags<-cut(SAL$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$FHgroup_tags)
SAL$FHgroup_tags<-as.numeric(as.character(SAL$FHgroup_tags))

SAL<-SAL %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
SALNNmean<-mean(na.omit(SAL$nonnative))
SALNNsd<-sd(na.omit(SAL$nonnative))
breaks<-c(-Inf,unique(SALNNmean-SALNNsd,SALNNmean+SALNNsd),Inf)
breaks
tags<-c("0","1")
SAL$NNgroup_tags<-cut(SAL$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$NNgroup_tags)
SAL$NNgroup_tags<-as.numeric(as.character(SAL$NNgroup_tags))

SAL<-SAL %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

SALFNmean<-mean(na.omit(SAL$forbnumber))
SALFNsd<-sd(na.omit(SAL$forbnumber))
breaks<-c(-Inf,unique(c(SALFNmean-(2*SALFNsd),SALFNmean+(2*SALFNsd))),Inf)
tags<-c("0","1","0")
SAL$FNgroup_tags<-cut(SAL$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$FNgroup_tags)
SAL$FNgroup_tags<-as.numeric(as.character(SAL$FNgroup_tags))
SAL<-SAL %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

SALRmean<-mean(na.omit(SAL$rock))
SALRsd<-sd(na.omit(SAL$rock))
breaks<-c(-Inf,unique(c(SALRmean-(2*SALRsd),SALRmean+(2*SALRsd))),Inf)
tags<-c("0","1","0")
SAL$Rgroup_tags<-cut(SAL$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Rgroup_tags)
SAL$Rgroup_tags<-as.numeric(as.character(SAL$Rgroup_tags))

SAL<-SAL %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

SALLmean<-mean(na.omit(SAL$litter))
SALLsd<-sd(na.omit(SAL$litter))
breaks<-c(-Inf,unique(c(SALLmean-(2*SALLsd),SALLmean+(2*SALLsd))),Inf)
tags<-c("0","1","0")
SAL$Lgroup_tags<-cut(SAL$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$Lgroup_tags)
SAL$Lgroup_tags<-as.numeric(as.character(SAL$Lgroup_tags))

SAL<-SAL %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


SALSUmean<-mean(na.omit(SAL$succulent))
SALSUsd<-sd(na.omit(SAL$succulent))
breaks<-c(-Inf,unique(c(SALSUmean-(2*SALSUsd),SALSUmean+(2*SALSUsd))),Inf)
tags<-c("0","1","0")
SAL$SUgroup_tags<-cut(SAL$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$SUgroup_tags)
SAL$SUgroup_tags<-as.numeric(as.character(SAL$SUgroup_tags))

SAL<-SAL %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

SALGAmean<-mean(na.omit(SAL$gaps))
SALGAsd<-sd(na.omit(SAL$gaps))
breaks<-c(-Inf,unique(c(SALGAmean-(2*SALGAsd),SALGAmean+(2*SALGAsd))),Inf)
tags<-c("0","1","0")
SAL$GAgroup_tags<-cut(SAL$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$GAgroup_tags)
SAL$GAgroup_tags<-as.numeric(as.character(SAL$GAgroup_tags))

SAL<-SAL %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
SALL<-SAL %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)

SALL<-gather(SALL,indicator,value,-plotID,na.rm=TRUE)
SALL<- SALL %>%
  group_by(plotID) %>%
  mutate(sum=n())
SALGraph<-SAL
SALGraph$sum<-SALL$sum[match(SALGraph$plotID,SALL$plotID)]
SALGraph[is.na(SALGraph)] <- 0
SALGraph<-SALGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
SALGraph$list<-(1:length(SALGraph$graph))

SALB<-ggplot(SALGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-SALB+ggtitle("Plots within expected range SAL")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
SAL2<-SAL
SAL2$graph<-SALGraph$graph[match(SAL2$plotID,SALGraph$plotID)]
SAL2$graph<-SALGraph$graph[match(SAL2$plotID,SALGraph$plotID)]
SAL2$graph<-as.numeric(SALGraph$graph)
SAL2$graph<-round(SAL2$graph,digits=2)
SAL3<-SAL2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
SAL3<-SAL3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(SAL3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(SAL3)
write.csv(SAL3,"SALIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
SALA1<-SAL
SAL2$graph<-as.numeric(SALGraph$graph)
SALA1$graph<-round(SAL2$graph,digits=2)
SALA<-SALA1 %>%
  select(plotID,allotment,graph)
SALA<-SALA%>%
  arrange(allotment)
names(SALA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(SALA)

write.csv(SALA,"SALallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
SAL$baresoil.bin<-SAL$baresoil.bin*100
SAL$soilstabilitybin<-SAL$soilstabilitybin*100
SAL$treecoverbin<-SAL$treecoverbin*100
SAL$shrubcoverbin<-SAL$shrubcoverbin*100
SAL$grasscoverbin<-SAL$grasscoverbin*100
SAL$grassheightbin<-SAL$grassheightbin*100
SAL$forb.heightbin<-SAL$forb.heightbin*100
SAL$forbcoverbin<-SAL$forbcoverbin*100
SAL$gapsbin<-SAL$gapsbin*100
SAL$nonnativebin<-SAL$nonnativebin*100
SAL$nativebin<-SAL$nativebin*100
SAL$succulentbin<-SAL$succulentbin*100
SAL$litterbin<-SAL$litterbin*100
SAL$rockbin<-SAL$rockbin*100
SAL$forbnumberbin<-SAL$forbnumberbin*100
SAL$graph<-SALGraph$graph[match(SAL$plotID,SALGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
SAL$SUMgroup_tags<-cut(SAL$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAL$SUMgroup_tags)
SAL$SUMgroup_tags<-as.numeric(as.character(SAL$SUMgroup_tags))

SAL<-SAL %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
SALR<-SAL
SAL<-SAL[!duplicated(SAL$nativebin),]
stratablanks<-rep("",15)
Strata<-c("SAL",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmarkk")
Percent.Meeting<-c(round(SAL$baresoil.bin,digits=2),round(SAL$soilstabilitybin,digits=2),round(SAL$litterbin,digits=2),round(SAL$nativebin,digits=2),round(SAL$grasscoverbin,digits=2),round(SAL$forbcoverbin,digits=2),
                   round(SAL$shrubcoverbin,digits=2),round(SAL$nonnativebin,digits=2),round(SAL$forbnumberbin,digits=2),round(SAL$rockbin,digits=2),
                   round(SAL$treecoverbin,digits=2),round(SAL$succulentbin,digits=2),round(SAL$gapsbin,digits=2),round(SAL$forb.heightbin,digits=2),round(SAL$grassheightbin,digits=2),round(SAL$SUMbin,digits=2))
SALdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(SALdataframe)
#to write csv
write.csv(SALdataframe,"SALPercentofIndicatorsMeeting.csv")
#SAN
SAN<-subset(bench,Actual.Eco.Site=="SAN")
SANmean<-mean(na.omit(SAN$baresoil))
SANsd<-sd(na.omit(SAN$baresoil))
breaks<-c(-Inf,unique(c(SANmean-(2*SANsd),SANmean+(2*SANsd))),Inf)
tags<-c("0","1","0")
SAN$BSgroup_tags<-cut(SAN$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$BSgroup_tags)
SAN$BSgroup_tags<-as.numeric(as.character(SAN$BSgroup_tags))

SAN<-SAN %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

SANSSmean<-mean(na.omit(SAN$soilstability))
SANSSsd<-sd(na.omit(SAN$soilstability))
breaks<-c(-Inf,unique(c(SANSSmean-(2*SANSSsd),SANSSmean+(2*SANSSsd))),Inf)
breaks
tags<-c("0","1","0")
SAN$SSgroup_tags<-cut(SAN$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$SSgroup_tags)
SAN$SSgroup_tags<-as.numeric(as.character(SAN$SSgroup_tags))

SAN<-SAN %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

SANNmean<-mean(na.omit(SAN$native))
SANNsd<-sd(na.omit(SAN$native))
breaks<-c(-Inf,unique(c(SANNmean-(2*SANNsd),SANNmean+(2*SANNsd))),Inf)
tags<-c("0","1","0")
SAN$Ngroup_tags<-cut(SAN$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Ngroup_tags)
SAN$Ngroup_tags<-as.numeric(as.character(SAN$Ngroup_tags))

SAN<-SAN %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


SANGmean<-mean(na.omit(SAN$grasscover))
SANGsd<-sd(na.omit(SAN$grasscover))
breaks<-c(-Inf,unique(c(SANGmean-(2*SANGsd),SANGmean+(2*SANGsd))),Inf)
tags<-c("0","1","0")
SAN$Ggroup_tags<-cut(SAN$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Ggroup_tags)
SAN$Ggroup_tags<-as.numeric(as.character(SAN$Ggroup_tags))

SAN<-SAN %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


SANFmean<-mean(na.omit(SAN$forbcover))
SANFsd<-sd(na.omit(SAN$forbcover))
breaks<-c(-Inf,unique(c(SANFmean-(2*SANFsd),SANFmean+(2*SANFsd))),Inf)
tags<-c("0","1","0")
SAN$Fgroup_tags<-cut(SAN$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Fgroup_tags)
SAN$Fgroup_tags<-as.numeric(as.character(SAN$Fgroup_tags))

SAN<-SAN %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

SANSmean<-mean(na.omit(SAN$shrubcover))
SANSsd<-sd(na.omit(SAN$shrubcover))
breaks<-c(-Inf,unique(c(SANSmean-(2*SANSsd),SANSmean+(2*SANSsd))),Inf)
tags<-c("0","1","0")
SAN$Sgroup_tags<-cut(SAN$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Sgroup_tags)
SAN$Sgroup_tags<-as.numeric(as.character(SAN$Sgroup_tags))

SAN<-SAN %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

SANTmean<-mean(na.omit(SAN$treecover))
SANTsd<-sd(na.omit(SAN$treecover))
breaks<-c(-Inf,unique(c(SANTmean-(2*SANTsd),SANTmean+(2*SANTsd))),Inf)
tags<-c("0","1","0")
SAN$Tgroup_tags<-cut(SAN$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Tgroup_tags)
SAN$Tgroup_tags<-as.numeric(as.character(SAN$Tgroup_tags))

SAN<-SAN %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

SANGHmean<-mean(na.omit(SAN$grassheight))
SANGHsd<-sd(na.omit(SAN$grassheight))
breaks<-c(-Inf,SANGHmean-SANGHsd,SANGHmean+SANGHsd,Inf)
tags<-c("0","1","0")
SAN$GHgroup_tags<-cut(SAN$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$GHgroup_tags)
SAN$GHgroup_tags<-as.numeric(as.character(SAN$GHgroup_tags))

SAN<-SAN %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

SANFHmean<-mean(na.omit(SAN$forb.height))
SANFHsd<-sd(na.omit(SAN$forb.height))
breaks<-c(-Inf,unique(c(SANFHmean-(2*SANFHsd),SANFHmean+(2*SANFHsd))),Inf)
tags<-c("0","1","0")
SAN$FHgroup_tags<-cut(SAN$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$FHgroup_tags)
SAN$FHgroup_tags<-as.numeric(as.character(SAN$FHgroup_tags))

SAN<-SAN %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
SANNNmean<-mean(na.omit(SAN$nonnative))
SANNNsd<-sd(na.omit(SAN$nonnative))
breaks<-c(-Inf,unique(SANNNmean-SANNNsd,SANNNmean+SANNNsd),Inf)
breaks
tags<-c("0","1")
SAN$NNgroup_tags<-cut(SAN$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$NNgroup_tags)
SAN$NNgroup_tags<-as.numeric(as.character(SAN$NNgroup_tags))

SAN<-SAN %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

SANFNmean<-mean(na.omit(SAN$forbnumber))
SANFNsd<-sd(na.omit(SAN$forbnumber))
breaks<-c(-Inf,unique(c(SANFNmean-(2*SANFNsd),SANFNmean+(2*SANFNsd))),Inf)
tags<-c("0","1","0")
SAN$FNgroup_tags<-cut(SAN$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$FNgroup_tags)
SAN$FNgroup_tags<-as.numeric(as.character(SAN$FNgroup_tags))
SAN<-SAN %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

SANRmean<-mean(na.omit(SAN$rock))
SANRsd<-sd(na.omit(SAN$rock))
breaks<-c(-Inf,unique(c(SANRmean-(2*SANRsd),SANRmean+(2*SANRsd))),Inf)
tags<-c("0","1","0")
SAN$Rgroup_tags<-cut(SAN$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Rgroup_tags)
SAN$Rgroup_tags<-as.numeric(as.character(SAN$Rgroup_tags))

SAN<-SAN %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

SANLmean<-mean(na.omit(SAN$litter))
SANLsd<-sd(na.omit(SAN$litter))
breaks<-c(-Inf,unique(c(SANLmean-(2*SANLsd),SANLmean+(2*SANLsd))),Inf)
tags<-c("0","1","0")
SAN$Lgroup_tags<-cut(SAN$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$Lgroup_tags)
SAN$Lgroup_tags<-as.numeric(as.character(SAN$Lgroup_tags))

SAN<-SAN %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


SANSUmean<-mean(na.omit(SAN$succulent))
SANSUsd<-sd(na.omit(SAN$succulent))
breaks<-c(-Inf,unique(c(SANSUmean-(2*SANSUsd),SANSUmean+(2*SANSUsd))),Inf)
tags<-c("0","1","0")
SAN$SUgroup_tags<-cut(SAN$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$SUgroup_tags)
SAN$SUgroup_tags<-as.numeric(as.character(SAN$SUgroup_tags))

SAN<-SAN %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

SANGAmean<-mean(na.omit(SAN$gaps))
SANGAsd<-sd(na.omit(SAN$gaps))
breaks<-c(-Inf,unique(c(SANGAmean-(2*SANGAsd),SANGAmean+(2*SANGAsd))),Inf)
tags<-c("0","1","0")
SAN$GAgroup_tags<-cut(SAN$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$GAgroup_tags)
SAN$GAgroup_tags<-as.numeric(as.character(SAN$GAgroup_tags))

SAN<-SAN %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
SANL<-SAN %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
SANL<-gather(SANL,indicator,value,-plotID,na.rm=TRUE)
SANL<- SANL %>%
  group_by(plotID) %>%
  mutate(sum=n())
SANGraph<-SAN
SANGraph$sum<-SANL$sum[match(SANGraph$plotID,SANL$plotID)]
SANGraph[is.na(SANGraph)] <- 0
SANGraph<-SANGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
SANGraph$list<-(1:length(SANGraph$graph))

SANB<-ggplot(SANGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-SANB+ggtitle("Plots within expected range SAN")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
SAN2<-SAN
SAN2$graph<-SANGraph$graph[match(SAN2$plotID,SANGraph$plotID)]
SAN2$graph<-as.numeric(SANGraph$graph)
SAN2$graph<-round(SAN2$graph,digits=2)
SAN3<-SAN2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
SAN3<-SAN3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(SAN3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(SAN3)
write.csv(SAN3,"SANIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
SANA1<-SAN
SAN2$graph<-as.numeric(SANGraph$graph)
SANA1$graph<-round(SAN2$graph,digits=2)
SANA<-SANA1 %>%
  select(plotID,allotment,graph)
SANA<-SANA%>%
  arrange(allotment)
names(SANA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(SANA)

write.csv(SANA,"SANallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
SAN$baresoil.bin<-SAN$baresoil.bin*100
SAN$soilstabilitybin<-SAN$soilstabilitybin*100
SAN$treecoverbin<-SAN$treecoverbin*100
SAN$shrubcoverbin<-SAN$shrubcoverbin*100
SAN$grasscoverbin<-SAN$grasscoverbin*100
SAN$grassheightbin<-SAN$grassheightbin*100
SAN$forb.heightbin<-SAN$forb.heightbin*100
SAN$forbcoverbin<-SAN$forbcoverbin*100
SAN$gapsbin<-SAN$gapsbin*100
SAN$nonnativebin<-SAN$nonnativebin*100
SAN$nativebin<-SAN$nativebin*100
SAN$succulentbin<-SAN$succulentbin*100
SAN$litterbin<-SAN$litterbin*100
SAN$rockbin<-SAN$rockbin*100
SAN$forbnumberbin<-SAN$forbnumberbin*100
SAN$graph<-SANGraph$graph[match(SAN$plotID,SANGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
SAN$SUMgroup_tags<-cut(SAN$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(SAN$SUMgroup_tags)
SAN$SUMgroup_tags<-as.numeric(as.character(SAN$SUMgroup_tags))

SAN<-SAN %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
SANR<-SAN
SAN<-SAN[!duplicated(SAN$nativebin),]
stratablanks<-rep("",15)
Strata<-c("SAN",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(SAN$baresoil.bin,digits=2),round(SAN$soilstabilitybin,digits=2),round(SAN$litterbin,digits=2),round(SAN$nativebin,digits=2),round(SAN$grasscoverbin,digits=2),round(SAN$forbcoverbin,digits=2),
                   round(SAN$shrubcoverbin,digits=2),round(SAN$nonnativebin,digits=2),round(SAN$forbnumberbin,digits=2),round(SAN$rockbin,digits=2),
                   round(SAN$treecoverbin,digits=2),round(SAN$succulentbin,digits=2),round(SAN$gapsbin,digits=2),round(SAN$forb.heightbin,digits=2),round(SAN$grassheightbin,digits=2),round(SAN$SUMbin,digits=2))
SANdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(SANdataframe)
#to write csv
write.csv(SANdataframe,"SANPercentofIndicatorsMeeting.csv")
#FORE
FORE<-subset(bench,Actual.Eco.Site=="FORE")
FOREmean<-mean(na.omit(FORE$baresoil))
FOREsd<-sd(na.omit(FORE$baresoil))
breaks<-c(-Inf,unique(c(FOREmean-(2*FOREsd),FOREmean+(2*FOREsd))),Inf)
tags<-c("0","1","0")
FORE$BSgroup_tags<-cut(FORE$baresoil, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$BSgroup_tags)
FORE$BSgroup_tags<-as.numeric(as.character(FORE$BSgroup_tags))

FORE<-FORE %>%
  mutate(baresoil.bin=sum(na.omit(BSgroup_tags))/length(na.omit(BSgroup_tags)))

FORESSmean<-mean(na.omit(FORE$soilstability))
FORESSsd<-sd(na.omit(FORE$soilstability))
breaks<-c(-Inf,unique(c(FORESSmean-(2*FORESSsd),FORESSmean+(2*FORESSsd))),Inf)
breaks
tags<-c("0","1","0")
FORE$SSgroup_tags<-cut(FORE$soilstability, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$SSgroup_tags)
FORE$SSgroup_tags<-as.numeric(as.character(FORE$SSgroup_tags))

FORE<-FORE %>%
  mutate(soilstabilitybin=sum(na.omit(SSgroup_tags))/length(na.omit(SSgroup_tags)))

FORENmean<-mean(na.omit(FORE$native))
FORENsd<-sd(na.omit(FORE$native))
breaks<-c(-Inf,unique(c(FORENmean-(2*FORENsd),FORENmean+(2*FORENsd))),Inf)
tags<-c("0","1","0")
FORE$Ngroup_tags<-cut(FORE$native, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Ngroup_tags)
FORE$Ngroup_tags<-as.numeric(as.character(FORE$Ngroup_tags))

FORE<-FORE %>%
  mutate(nativebin=sum(na.omit(Ngroup_tags))/length(na.omit(Ngroup_tags)))


FOREGmean<-mean(na.omit(FORE$grasscover))
FOREGsd<-sd(na.omit(FORE$grasscover))
breaks<-c(-Inf,unique(c(FOREGmean-(2*FOREGsd),FOREGmean+(2*FOREGsd))),Inf)
tags<-c("0","1","0")
FORE$Ggroup_tags<-cut(FORE$grasscover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Ggroup_tags)
FORE$Ggroup_tags<-as.numeric(as.character(FORE$Ggroup_tags))

FORE<-FORE %>%
  mutate(grasscoverbin=sum(na.omit(Ggroup_tags))/length(na.omit(Ggroup_tags)))


FOREFmean<-mean(na.omit(FORE$forbcover))
FOREFsd<-sd(na.omit(FORE$forbcover))
breaks<-c(-Inf,unique(c(FOREFmean-(2*FOREFsd),FOREFmean+(2*FOREFsd))),Inf)
tags<-c("0","1","0")
FORE$Fgroup_tags<-cut(FORE$forbcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Fgroup_tags)
FORE$Fgroup_tags<-as.numeric(as.character(FORE$Fgroup_tags))

FORE<-FORE %>%
  mutate(forbcoverbin=sum(na.omit(Fgroup_tags))/length(na.omit(Fgroup_tags)))

FORESmean<-mean(na.omit(FORE$shrubcover))
FORESsd<-sd(na.omit(FORE$shrubcover))
breaks<-c(-Inf,unique(c(FORESmean-(2*FORESsd),FORESmean+(2*FORESsd))),Inf)
tags<-c("0","1","0")
FORE$Sgroup_tags<-cut(FORE$shrubcover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Sgroup_tags)
FORE$Sgroup_tags<-as.numeric(as.character(FORE$Sgroup_tags))

FORE<-FORE %>%
  mutate(shrubcoverbin=sum(na.omit(Sgroup_tags))/length(na.omit(Sgroup_tags)))

FORETmean<-mean(na.omit(FORE$treecover))
FORETsd<-sd(na.omit(FORE$treecover))
breaks<-c(-Inf,unique(c(FORETmean-(2*FORETsd),FORETmean+(2*FORETsd))),Inf)
tags<-c("0","1","0")
FORE$Tgroup_tags<-cut(FORE$treecover, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Tgroup_tags)
FORE$Tgroup_tags<-as.numeric(as.character(FORE$Tgroup_tags))

FORE<-FORE %>%
  mutate(treecoverbin=sum(na.omit(Tgroup_tags))/length(na.omit(Tgroup_tags)))

FOREGHmean<-mean(na.omit(FORE$grassheight))
FOREGHsd<-sd(na.omit(FORE$grassheight))
breaks<-c(-Inf,FOREGHmean-FOREGHsd,FOREGHmean+FOREGHsd,Inf)
tags<-c("0","1","0")
FORE$GHgroup_tags<-cut(FORE$grassheight, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$GHgroup_tags)
FORE$GHgroup_tags<-as.numeric(as.character(FORE$GHgroup_tags))

FORE<-FORE %>%
  mutate(grassheightbin=sum(na.omit(GHgroup_tags))/length(na.omit(GHgroup_tags)))

FOREFHmean<-mean(na.omit(FORE$forb.height))
FOREFHsd<-sd(na.omit(FORE$forb.height))
breaks<-c(-Inf,unique(c(FOREFHmean-(2*FOREFHsd),FOREFHmean+(2*FOREFHsd))),Inf)
tags<-c("0","1","0")
FORE$FHgroup_tags<-cut(FORE$forb.height, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$FHgroup_tags)
FORE$FHgroup_tags<-as.numeric(as.character(FORE$FHgroup_tags))

FORE<-FORE %>%
  mutate(forb.heightbin=sum(na.omit(FHgroup_tags))/length(na.omit(FHgroup_tags)))

#since only 3 bins, had to change "tags" to two categories
FORENNmean<-mean(na.omit(FORE$nonnative))
FORENNsd<-sd(na.omit(FORE$nonnative))
breaks<-c(-Inf,unique(FORENNmean-FORENNsd,FORENNmean+FORENNsd),Inf)
breaks
tags<-c("0","1")
FORE$NNgroup_tags<-cut(FORE$nonnative, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$NNgroup_tags)
FORE$NNgroup_tags<-as.numeric(as.character(FORE$NNgroup_tags))

FORE<-FORE %>%
  mutate(nonnativebin=sum(na.omit(NNgroup_tags))/length(na.omit(NNgroup_tags)))

FOREFNmean<-mean(na.omit(FORE$forbnumber))
FOREFNsd<-sd(na.omit(FORE$forbnumber))
breaks<-c(-Inf,unique(c(FOREFNmean-(2*FOREFNsd),FOREFNmean+(2*FOREFNsd))),Inf)
tags<-c("0","1","0")
FORE$FNgroup_tags<-cut(FORE$forbnumber, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$FNgroup_tags)
FORE$FNgroup_tags<-as.numeric(as.character(FORE$FNgroup_tags))
FORE<-FORE %>%
  mutate(forbnumberbin=sum(na.omit(FNgroup_tags))/length(na.omit(FNgroup_tags)))

FORERmean<-mean(na.omit(FORE$rock))
FORERsd<-sd(na.omit(FORE$rock))
breaks<-c(-Inf,unique(c(FORERmean-(2*FORERsd),FORERmean+(2*FORERsd))),Inf)
tags<-c("0","1","0")
FORE$Rgroup_tags<-cut(FORE$rock, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Rgroup_tags)
FORE$Rgroup_tags<-as.numeric(as.character(FORE$Rgroup_tags))

FORE<-FORE %>%
  mutate(rockbin=sum(na.omit(Rgroup_tags))/length(na.omit(Rgroup_tags)))

FORELmean<-mean(na.omit(FORE$litter))
FORELsd<-sd(na.omit(FORE$litter))
breaks<-c(-Inf,unique(c(FORELmean-(2*FORELsd),FORELmean+(2*FORELsd))),Inf)
tags<-c("0","1","0")
FORE$Lgroup_tags<-cut(FORE$litter, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$Lgroup_tags)
FORE$Lgroup_tags<-as.numeric(as.character(FORE$Lgroup_tags))

FORE<-FORE %>%
  mutate(litterbin=sum(na.omit(Lgroup_tags))/length(na.omit(Lgroup_tags)))


FORESUmean<-mean(na.omit(FORE$succulent))
FORESUsd<-sd(na.omit(FORE$succulent))
breaks<-c(-Inf,unique(c(FORESUmean-(2*FORESUsd),FORESUmean+(2*FORESUsd))),Inf)
tags<-c("0","1","0")
FORE$SUgroup_tags<-cut(FORE$succulent, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$SUgroup_tags)
FORE$SUgroup_tags<-as.numeric(as.character(FORE$SUgroup_tags))

FORE<-FORE %>%
  mutate(succulentbin=sum(na.omit(SUgroup_tags))/length(na.omit(SUgroup_tags)))

FOREGAmean<-mean(na.omit(FORE$gaps))
FOREGAsd<-sd(na.omit(FORE$gaps))
breaks<-c(-Inf,unique(c(FOREGAmean-(2*FOREGAsd),FOREGAmean+(2*FOREGAsd))),Inf)
tags<-c("0","1","0")
FORE$GAgroup_tags<-cut(FORE$gaps, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$GAgroup_tags)
FORE$GAgroup_tags<-as.numeric(as.character(FORE$GAgroup_tags))

FORE<-FORE %>%
  mutate(gapsbin=sum(na.omit(GAgroup_tags))/length(na.omit(GAgroup_tags)))

##
FOREL<-FORE %>%
  select(plotID,BSgroup_tags,SSgroup_tags,Ggroup_tags,Fgroup_tags,Ngroup_tags,
         NNgroup_tags,Tgroup_tags,Sgroup_tags,GHgroup_tags,FHgroup_tags,FNgroup_tags,
         Rgroup_tags,SUgroup_tags,Lgroup_tags,GAgroup_tags)
FOREL<-gather(FOREL,indicator,value,-plotID,na.rm=TRUE)
FOREL<- FOREL %>%
  group_by(plotID) %>%
  mutate(sum=n())
FOREGraph<-FORE
FOREGraph$sum<-FOREL$sum[match(FOREGraph$plotID,FOREL$plotID)]
FOREGraph[is.na(FOREGraph)] <- 0
FOREGraph<-FOREGraph %>%
  mutate(graph=((BSgroup_tags+SSgroup_tags+Ggroup_tags+Fgroup_tags+Ngroup_tags+
                   NNgroup_tags+Tgroup_tags+Sgroup_tags+GHgroup_tags+FHgroup_tags+FNgroup_tags+
                   Rgroup_tags+SUgroup_tags+Lgroup_tags+GAgroup_tags)/sum)*100)
FOREGraph$list<-(1:length(FOREGraph$graph))

FOREB<-ggplot(FOREGraph,aes(x=list,y=graph,color=graph,label=plotID))+geom_point()+geom_text_repel(aes(label=plotID),hjust=0, vjust=0)
B3<-FOREB+ggtitle("Plots within expected range FORE")+labs(x="Plot ID",y="Percent of indicators within expected range")+theme(axis.text.x=element_blank())
B3+scale_colour_gradient(low = "red", high = "green3", na.value = NA)+ theme(legend.position = "none") 


#table of benhcmarks met for appendix
FORE2<-FORE
FORE2$graph<-FOREGraph$graph[match(FORE2$plotID,FOREGraph$plotID)]
FORE2$graph<-as.numeric(FOREGraph$graph)
FORE2$graph<-round(FORE2$graph,digits=2)
FORE3<-FORE2 %>%
  select(plotID,allotment,BSgroup_tags,SSgroup_tags,Fgroup_tags,Ggroup_tags,Sgroup_tags,Tgroup_tags,FHgroup_tags,
         Ngroup_tags,NNgroup_tags,GAgroup_tags,Lgroup_tags,Rgroup_tags,SUgroup_tags,FNgroup_tags,GHgroup_tags,graph)
FORE3<-FORE3%>%
  arrange(graph) %>%
  filter(graph < 100)

names(FORE3)<-c("Plot.ID","Allotment","Bare.Soil","Soil.Stability","Forb","Grass","Shrub","Tree","Forb.Height","Nonnoxious","Noxious","Gaps","Litter","Rock","Succulent","Forb.Number","Grass.Height","Percent.Meeting")
formattable(FORE3)
write.csv(FORE3,"FOREIndicatorsmet.csv")
#to view allotments and percent of plot meeting benchmark
FOREA1<-FORE
FORE2$graph<-as.numeric(FOREGraph$graph)
FOREA1$graph<-round(FORE2$graph,digits=2)
FOREA<-FOREA1 %>%
  select(plotID,allotment,graph)
FOREA<-FOREA%>%
  arrange(allotment)
names(FOREA)<-c("Plot.ID","Allotment.Name","Percent.Meeting")
formattable(FOREA)

write.csv(FOREA,"FOREallotmentspercentmeeting.csv")

#REMOVE DUPLICATES
FORE$baresoil.bin<-FORE$baresoil.bin*100
FORE$soilstabilitybin<-FORE$soilstabilitybin*100
FORE$treecoverbin<-FORE$treecoverbin*100
FORE$shrubcoverbin<-FORE$shrubcoverbin*100
FORE$grasscoverbin<-FORE$grasscoverbin*100
FORE$grassheightbin<-FORE$grassheightbin*100
FORE$forb.heightbin<-FORE$forb.heightbin*100
FORE$forbcoverbin<-FORE$forbcoverbin*100
FORE$gapsbin<-FORE$gapsbin*100
FORE$nonnativebin<-FORE$nonnativebin*100
FORE$nativebin<-FORE$nativebin*100
FORE$succulentbin<-FORE$succulentbin*100
FORE$litterbin<-FORE$litterbin*100
FORE$rockbin<-FORE$rockbin*100
FORE$forbnumberbin<-FORE$forbnumberbin*100
FORE$graph<-FOREGraph$graph[match(FORE$plotID,FOREGraph$plotID)]
breaks<-c(0,100,Inf)
tags<-c("0","1")
FORE$SUMgroup_tags<-cut(FORE$graph, breaks=breaks,include.lowest=TRUE, right=FALSE,labels=tags)
summary(FORE$SUMgroup_tags)
FORE$SUMgroup_tags<-as.numeric(as.character(FORE$SUMgroup_tags))

FORE<-FORE %>%
  mutate(SUMbin=(sum(na.omit(SUMgroup_tags))/length(na.omit(SUMgroup_tags))*100))
FORER<-FORE
FORE<-FORE[!duplicated(FORE$nativebin),]
stratablanks<-rep("",15)
Strata<-c("FORE",stratablanks)
Indicator<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs", "Percent Rock Cover",
             "Percent Tree Cover","Percent Succulent Cover","Percent Gap Cover","Forb Height","Grass Height","Total Percent of Plots Meeting Benchmark")
Percent.Meeting<-c(round(FORE$baresoil.bin,digits=2),round(FORE$soilstabilitybin,digits=2),round(FORE$litterbin,digits=2),round(FORE$nativebin,digits=2),round(FORE$grasscoverbin,digits=2),round(FORE$forbcoverbin,digits=2),
                   round(FORE$shrubcoverbin,digits=2),round(FORE$nonnativebin,digits=2),round(FORE$forbnumberbin,digits=2),round(FORE$rockbin,digits=2),
                   round(FORE$treecoverbin,digits=2),round(FORE$succulentbin,digits=2),round(FORE$gapsbin,digits=2),round(FORE$forb.heightbin,digits=2),round(FORE$grassheightbin,digits=2),round(FORE$SUMbin,digits=2))
FOREdataframe<-data.frame(Strata,Indicator,Percent.Meeting)
formattable(FOREdataframe)
#to write csv
write.csv(FOREdataframe,"FOREPercentofIndicatorsMeeting.csv")


#binding dataa frames for a summary by allotment
df<-rbind(BHR,LBR)
df<-rbind(df,LOAR)
df<-rbind(df,MOR)
df<-rbind(df,OTHR)
df<-rbind(df,ROFR)
df<-rbind(df,SALR)
df<-rbind(df,SANR)
df<-rbind(df,FORER)
##in another script I already ran the data for intensification plots
df<-rbind(df,PVR)
df<-rbind(df,GUSGOR)
df<-rbind(df,GUSGPR)
df<-rbind(df,GUSGWR)

df<-df %>%
  group_by(allotment) %>%
  mutate(sumofind.met=sum(SUMgroup_tags)) %>%
  mutate(allotmenttotalplots=length(allotment)) %>%
  mutate(percentallotmentmeeting=(sumofind.met/allotmenttotalplots)*100)%>%
  ungroup() 

df2<-df %>%
  filter(allotmenttotalplots >= 3)

df<-df[!duplicated(df$allotment),]

Allotment<-df$allotment
Number.of.Plots.Meeting.Benchmark<-df$sumofind.met
Total.Number.of.Plots.in.Allotment<-df$allotmenttotalplots
Percent.of.Plots.Meeting<-round(df$percentallotmentmeeting,digits=2)
Adataframe<-data.frame(Allotment,Number.of.Plots.Meeting.Benchmark,Total.Number.of.Plots.in.Allotment,Percent.of.Plots.Meeting)

formattable(Adataframe)
write.csv(Adataframe,"Totalallotmentmeeting.csv")

df2<-df2[!duplicated(df2$allotment),]

df2<- df2 %>%
  filter(percentallotmentmeeting < 100) %>%
  arrange(percentallotmentmeeting)

Allotment<-df2$allotment
Number.of.Plots.Meeting.Benchmark<-df2$sumofind.met
Total.Number.of.Plots.in.Allotment<-df2$allotmenttotalplots
Percent.of.Plots.Meeting<-round(df2$percentallotmentmeeting,digits=2)
Adataframe2<-data.frame(Allotment,Number.of.Plots.Meeting.Benchmark,Total.Number.of.Plots.in.Allotment,Percent.of.Plots.Meeting)

formattable(Adataframe2)
write.csv(Adataframe2,"TotalAllotment3plots.csv")
