#this code is only if you have set benchmarks! see developing benchmarks... code if you do not have set benchmarks. this will produce
#a table of the percent of plots meeting benchmark per indicator per stratum and a scatterplot of these results which dispays expected
#range

benchtool<-read.csv("~/Allyears_query_U.csv")
View(benchtool)
library(tidyverse)
library(dplyr)
library(formattable)
library(gridExtra)
library(grid)

benchtool$Tree<-benchtool$Noxious.Tree.Cover.Pct.Any.Hit+benchtool$NonNoxious.Tree.Cover.Pct.Any.Hit
bench<- benchtool %>%
  select(Bare.Soil.Pct,Soil.Stability.All, Total.Litter.Cover.Pct.First.Hit,
         NonNoxious.Plant.Cover.Pct.Any.Hit, Grass.Cover.Pct.Any.Hit, Forb.Cover.Pct.Any.Hit,
         Shrub.Cover.Pct.Any.Hit,Tree,Sagebrush.Cover.Pct.Any.Hit,NonSagebrush.Shrub.Cover.Pct.Any.Hit,
         Average.Grass.Height.cm,Average.Forb.Height.cm,Noxious.Cover.Pct.Any.Hit,
         Number.Preferred.Forb.Species,Actual.Eco.Site)

#set the benchmarks. "Tags" are the meeting or not meeting labels. For example, 
##0-10% (technically 9.9 but not coded this way) is not meeting, 10-40% is meeting, and 40-100% is not meeting
###tags for this example will be tags<-c("[NO)","[YES)","[NO)"). SADE is my ecological site or strata.
###breaks are the ecological benchmarks. max and min will be used for graphing max being the upper benchmark limit
###and min being the lower benchmark limit

SADE<-subset(bench,Actual.Eco.Site=="SD")
SADEBAREGROUNDbreaks<-c(0,10,40,100)
SADEBAREGROUNDtags<-c("[NO)","[YES)","[NO)")
SADEBAREGROUNDmax<-10
SADEBAREGROUNDmin<-40

SADESOILSTABILITYbreaks<-c(0,3,6)
SADESOILSTABILITYtags<-c("[NO)","[YES)")
SADESOILSTABILITYmax<-6
SADESOILSTABILITYmin<-3

SADELITTERbreaks<-c(0,5,20,100)
SADELITTERtags<-c("[NO)","[YES)","[NO)")
SADELITTERmax<-20
SADELITTERmin<-5

SADENATIVEbreaks<-c(0,20,40)
SADENATIVEtags<-c("[NO)","[YES)")
SADENATIVEmax<-40
SADENATIVEmin<-20

SADEGRASSbreaks<-c(0,5,19,100)
SADEGRASStags<-c("[NO)","[YES)","[NO)")
SADEGRASSmax<-19
SADEGRASSmin<-5

SADEFORBbreaks<-c(0,5,14,100)
SADEFORBtags<-c("[NO)","[YES)","[NO)")
SADEFORBmax<-14
SADEFORBmin<-5

SADESHRUBbreaks<-c(0,10,39,100)
SADESHRUBtags<-c("[NO)","[YES)","[NO)")
SADESHRUBmax<-39
SADESHRUBmin<-10

SADENONNATIVEbreaks<-c(0,19,100)
SADENONNATIVEtags<-c("[YES)","[NO)")
SADENONNATIVEmax<-19
SADENONNATIVEmin<-0

SADENUMBEROFFORBSbreaks<-c(0,5,100)
SADENUMBEROFFORBStags<-c("[NO)","[YES)")
SADENUMBEROFFORBSmax<-100
SADENUMBEROFFORBSmin<-5

#compiling the data for the table. no output will be seen until the  table section!
#bareground 
group_tags<-cut(na.omit(SADE$Bare.Soil.Pct), breaks=SADEBAREGROUNDbreaks,include.lowest=TRUE, right=FALSE,labels=SADEBAREGROUNDtags)
group_tags<-data.frame(group_tags)
SADEPBGY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADEPBG<-SADEPBGY[!duplicated(SADEPBGY$sumY),]


#soil stability
group_tags<-cut(na.omit(SADE$Soil.Stability.All), breaks=SADESOILSTABILITYbreaks,include.lowest=TRUE, right=FALSE,labels=SADESOILSTABILITYtags)
group_tags<-data.frame(group_tags) 
SADESASY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADESAS<-SADESASY[!duplicated(SADESASY$sumY),]


#Litter
group_tags<-cut(na.omit(SADE$Total.Litter.Cover.Pct.First.Hit), breaks=SADELITTERbreaks,include.lowest=TRUE, right=FALSE,labels=SADELITTERtags)
group_tags<-data.frame(group_tags)
SADELY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADEL<-SADELY[!duplicated(SADELY$sumY),]


#native
group_tags<-cut(na.omit(SADE$NonNoxious.Plant.Cover.Pct.Any.Hit), breaks=SADENATIVEbreaks,include.lowest=TRUE, right=FALSE,labels=SADENATIVEtags)
group_tags<-data.frame(group_tags)
SADENY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADEN<-SADENY[!duplicated(SADENY$sumY),]

#percent cover grass
group_tags<-cut(na.omit(SADE$Grass.Cover.Pct.Any.Hit), breaks=SADEGRASSbreaks,include.lowest=TRUE, right=FALSE,labels=SADEGRASStags)
group_tags<-data.frame(group_tags)
SADEGY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADEG<-SADEGY[!duplicated(SADEGY$sumY),]

#percent cover forb
group_tags<-cut(na.omit(SADE$Forb.Cover.Pct.Any.Hit), breaks=SADEFORBbreaks,include.lowest=TRUE, right=FALSE,labels=SADEFORBtags)
group_tags<-data.frame(group_tags)
SADEFY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADEF<-SADEFY[!duplicated(SADEFY$sumY),]

#percent cover shrub
group_tags<-cut(na.omit(SADE$Shrub.Cover.Pct.Any.Hit), breaks=SADESHRUBbreaks,include.lowest=TRUE, right=FALSE,labels=SADESHRUBtags)
group_tags<-data.frame(group_tags)
SADESY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADES<-SADESY[!duplicated(SADESY$sumY),]


#nonnative
group_tags<-cut(na.omit(SADE$Noxious.Cover.Pct.Any.Hit), breaks=SADENONNATIVEbreaks,include.lowest=TRUE, right=FALSE,labels=SADENONNATIVEtags)
group_tags<-data.frame(group_tags)
SADENNY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADENN<-SADENNY[!duplicated(SADENNY$sumY),]

#number of forbs
group_tags<-cut(na.omit(SADE$Number.Preferred.Forb.Species), breaks=SADENUMBEROFFORBSbreaks,include.lowest=TRUE, right=FALSE,labels=SADENUMBEROFFORBStags)
group_tags<-data.frame(group_tags)
SADENFY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SADENF<-SADENFY[!duplicated(SADENFY$sumY),]



#table
##note stratablanks<-rep("",8) is based on having 9 benchmark categories, therefore if you have
##5 categories then change 8 to 4
stratablanks<-rep("",8)
Strata<-c("SD",stratablanks)
Category<- c("Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs")
Percent.Meeting<-c(round(SADEPBG$sumY,digits=2),round(SADESAS$sumY,digits=2),round(SADEL$sumY,digits=2),round(SADEN$sumY,digits=2),round(SADEG$sumY,digits=2),round(SADEF$sumY,digits=2),
                   round(SADES$sumY,digits=2),round(SADENN$sumY,digits=2),round(SADENF$sumY,digits=2))
SADEdataframe<-data.frame(Strata,Category,Percent.Meeting)
formattable(SADEdataframe)

#to add another strata. SAST=sage steppe
SAST<-subset(bench,Actual.Eco.Site=="SS")
SASTBAREGROUNDbreaks<-c(0,10,35,100)
SASTBAREGROUNDtags<-c("[NO)","[YES)","[NO)")
SASTBAREGROUNDmax<-35
SASTBAREGROUNDmin<-10

SASTSOILSTABILITYbreaks<-c(0,4,6)
SASTSOILSTABILITYtags<-c("[NO)","[YES)")
SASTSOILSTABILITYmax<-6
SASTSOILSTABILITYmin<-4

SASTLITTERbreaks<-c(0,10,40,100)
SASTLITTERtags<-c("[NO)","[YES)","[NO)")
SASTLITTERmax<-40
SASTLITTERmin<-10

#SASTNATIVEbreaks=NO BENCHMARK SET, THEREFORE OMITTING
#SASTNATIVEtags=NO BENCHMARK SET, THEREFORE OMITTING

SASTGRASSbreaks<-c(0,10,100)
SASTGRASStags<-c("[NO)","[YES)")
SASTGRASSmax<-100
SASTGRASSmin<-10

SASTFORBbreaks<-c(0,5,40,100)
SASTFORBtags<-c("[NO)","[YES)","[NO)")
SASTFORBmax<-40
SASTFORBmin<-5

#SASTSHRUBbreaks=NO BENCHMARK SET, THEREFORE OMITTING
#SASTSHRUBtags=NO BENCHMARK SET, THEREFORE OMITTING

SASTNONNATIVEbreaks<-c(0,10,100)
SASTNONNATIVEtags<-c("[YES)","[NO)")
SASTNONNATIVEmax<-10
SASTNONNATIVEmin<-0

SASTNUMBEROFFORBSbreaks<-c(0,5,100)
SASTNUMBEROFFORBStags<-c("[NO)","[YES)")
SASTNUMBEROFFORBmax<-100
SASTNUMBEROFFORBmin<-5

#bareground 
group_tags<-cut(na.omit(SAST$Bare.Soil.Pct), breaks=SASTBAREGROUNDbreaks,include.lowest=TRUE, right=FALSE,labels=SASTBAREGROUNDtags)
group_tags<-data.frame(group_tags)
SASTPBGY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTPBG<-SASTPBGY[!duplicated(SASTPBGY$sumY),]


#soil stability
group_tags<-cut(na.omit(SAST$Soil.Stability.All), breaks=SASTSOILSTABILITYbreaks,include.lowest=TRUE, right=FALSE,labels=SASTSOILSTABILITYtags)
group_tags<-data.frame(group_tags) 
SASTSASY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTSAS<-SASTSASY[!duplicated(SASTSASY$sumY),]


#Litter
group_tags<-cut(na.omit(SAST$Total.Litter.Cover.Pct.First.Hit), breaks=SASTLITTERbreaks,include.lowest=TRUE, right=FALSE,labels=SASTLITTERtags)
group_tags<-data.frame(group_tags)
SASTLY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTL<-SASTLY[!duplicated(SASTLY$sumY),]


#native
##NONE SET FOR THIS STRATA

#percent cover grass
group_tags<-cut(na.omit(SAST$Grass.Cover.Pct.Any.Hit), breaks=SASTGRASSbreaks,include.lowest=TRUE, right=FALSE,labels=SASTGRASStags)
group_tags<-data.frame(group_tags)
SASTGY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTG<-SASTGY[!duplicated(SASTGY$sumY),]

#percent cover forb
group_tags<-cut(na.omit(SAST$Forb.Cover.Pct.Any.Hit), breaks=SASTFORBbreaks,include.lowest=TRUE, right=FALSE,labels=SASTFORBtags)
group_tags<-data.frame(group_tags)
SASTFY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTF<-SASTFY[!duplicated(SASTFY$sumY),]

#percent cover shrub
##NONE SET FOR THIS STRATA


#nonnative
group_tags<-cut(na.omit(SAST$Noxious.Cover.Pct.Any.Hit), breaks=SASTNONNATIVEbreaks,include.lowest=TRUE, right=FALSE,labels=SASTNONNATIVEtags)
group_tags<-data.frame(group_tags)
SASTNNY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTNN<-SASTNNY[!duplicated(SASTNNY$sumY),]

#number of forbs
group_tags<-cut(na.omit(SAST$Number.Preferred.Forb.Species), breaks=SASTNUMBEROFFORBSbreaks,include.lowest=TRUE, right=FALSE,labels=SASTNUMBEROFFORBStags)
group_tags<-data.frame(group_tags)
SASTNFY<-group_tags %>%
  mutate(sumT=sum(length(group_tags))) %>%
  filter(group_tags=="[YES)") %>%
  mutate(sum=sum(length(group_tags))) %>%
  mutate(sumY=(sum/sumT)*100)
SASTNF<-SASTNFY[!duplicated(SASTNFY$sumY),]

#table, note since a native benchmark was omitted from this strata the only thing that is changed is Percent.Meeting 
##SASTN$...is removed and replaced with "N/A", SAME WITH SHRUBCOVER
stratablanks<-rep("",8)
Strata<-c("","SS",stratablanks)
Category<- c("","Percent Bare Ground", "Soil Aggregate Stability","Litter","Nonnoxious Species","Percent Grass Cover",
             "Percent Forb Cover","Percent Shrub Cover", "Percent Noxious Species","Number of Forbs")
Percent.Meeting<-c("",round(SASTPBG$sumY,digits=2),round(SASTSAS$sumY,digits=2),round(SASTL$sumY,digits=2),"N/A",round(SASTG$sumY,digits=2),round(SASTF$sumY,digits=2),
                   "N/A",round(SASTNN$sumY,digits=2),round(SASTNF$sumY,digits=2))
SASTdataframe<-data.frame(Strata,Category,Percent.Meeting)
SSdataframe<-rbind.data.frame(SADEdataframe,SASTdataframe)
formattable(SSdataframe)

#to graph 
##SADE (salt desert)
B<-ggplot(SADE,aes(x=1,y=Bare.Soil.Pct))
B2<-B+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADEBAREGROUNDmin,ymax=SADEBAREGROUNDmax),
            fill="palegreen1",alpha=0.03)
B3<-B2+ggtitle("Average Percent Bare Soil ")+labs(x="SD Plots",y="Average % Bare Soil")+theme(axis.text.x=element_blank())


L<-ggplot(SADE,aes(x=1,y=Total.Litter.Cover.Pct.First.Hit))
L2<-L+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADELITTERmin,ymax=SADELITTERmax),
            fill="palegreen1",alpha=0.03)
L3<-L2+ggtitle("Average Percent Litter Cover ")+labs(x="SD Plots",y="Average % Litter Cover")+theme(axis.text.x=element_blank())


NN<-ggplot(SADE,aes(x=1,y=NonNoxious.Plant.Cover.Pct.Any.Hit))
NN2<-NN+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADENATIVEmin,ymax=SADENATIVEmax),
            fill="palegreen1",alpha=0.03)
NN3<-NN2+ggtitle("Average Percent Nonnoxious Species ")+labs(x="SD Plots",y="Average % Nonnoxious Species")+theme(axis.text.x=element_blank())


G<-ggplot(SADE,aes(x=1,y=Grass.Cover.Pct.Any.Hit))
G2<-G+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADEGRASSmin,ymax=SADEGRASSmax),
            fill="palegreen1",alpha=0.03)
G3<-G2+ggtitle("Average Percent Grass Cover ")+labs(x="SD Plots",y="Average % Grass Cover")+theme(axis.text.x=element_blank())

F<-ggplot(SADE,aes(x=1,y=Forb.Cover.Pct.Any.Hit))
F2<-F+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADEFORBmin,ymax=SADEFORBmax),
            fill="palegreen1",alpha=0.03)
F3<-F2+ggtitle("Average Percent Forb Cover ")+labs(x="SD Plots",y="Average % Forb Cover")+theme(axis.text.x=element_blank())


S<-ggplot(SADE,aes(x=1,y=Shrub.Cover.Pct.Any.Hit))
S2<-S+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADESHRUBmin,ymax=SADESHRUBmax),
            fill="palegreen1",alpha=0.03)
S3<-S2+ggtitle("Average Percent Shrub Cover ")+labs(x="SD Plots",y="Average % Shrub Cover")+theme(axis.text.x=element_blank())


N<-ggplot(SADE,aes(x=1,y=Noxious.Cover.Pct.Any.Hit))
N2<-N+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADENONNATIVEmin,ymax=SADENONNATIVEmax),
            fill="palegreen1",alpha=0.03)
N3<-N2+ggtitle("Average Percent Noxious Species ")+labs(x="SD Plots",y="Average % Noxious Species")+theme(axis.text.x=element_blank())

NF<-ggplot(SADE,aes(x=1,y=Number.Preferred.Forb.Species))
NF2<-NF+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADENUMBEROFFORBSmin,ymax=SADENUMBEROFFORBSmax),
            fill="palegreen1",alpha=0.03)
NF3<-NF2+ggtitle("Number of Preferred Forbs ")+labs(x="SD Plots",y="Number of Preferred Forbs")+theme(axis.text.x=element_blank())

SAS<-ggplot(SADE,aes(x=1,y=Soil.Stability.All))
SAS2<-SAS+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SADESOILSTABILITYmin,ymax=SADESOILSTABILITYmax),
            fill="palegreen1",alpha=0.03)
SAS3<-SAS2+ggtitle("Number of Preferred Forbs ")+labs(x="SD Plots",y="Number of Preferred Forbs")+theme(axis.text.x=element_blank())

grid.arrange(B3,SAS3,L3,NN3,G3,F3,S3,N3,NF3,nrow=2,
             top = textGrob("Display of Plots within Benchmark by Benchmark Category",gp=gpar(fontsize=20,font=3)))

#to graph 
##SAST (sage steppe)
B<-ggplot(SAST,aes(x=1,y=Bare.Soil.Pct))
B2<-B+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTBAREGROUNDmin,ymax=SASTBAREGROUNDmax),
            fill="palegreen1",alpha=0.03)
B3<-B2+ggtitle("Average Percent Bare Soil ")+labs(x="SS Plots",y="Average % Bare Soil")+theme(axis.text.x=element_blank())


L<-ggplot(SAST,aes(x=1,y=Total.Litter.Cover.Pct.First.Hit))
L2<-L+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTLITTERmin,ymax=SASTLITTERmax),
            fill="palegreen1",alpha=0.03)
L3<-L2+ggtitle("Average Percent Litter Cover ")+labs(x="SS Plots",y="Average % Litter Cover")+theme(axis.text.x=element_blank())


#N<- OMITTING NO NATIVE BENCHMARK SET

G<-ggplot(SAST,aes(x=1,y=Grass.Cover.Pct.Any.Hit))
G2<-G+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTGRASSmin,ymax=SASTGRASSmax),
            fill="palegreen1",alpha=0.03)
G3<-G2+ggtitle("Average Percent Grass Cover ")+labs(x="SS Plots",y="Average % Grass Cover")+theme(axis.text.x=element_blank())

F<-ggplot(SAST,aes(x=1,y=Forb.Cover.Pct.Any.Hit))
F2<-F+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTFORBmin,ymax=SASTFORBmax),
            fill="palegreen1",alpha=0.03)
F3<-F2+ggtitle("Average Percent Forb Cover ")+labs(x="SS Plots",y="Average % Forb Cover")+theme(axis.text.x=element_blank())


#s<-OMITTING NO SHRUB BENCHMARK SET

N<-ggplot(SAST,aes(x=1,y=Noxious.Cover.Pct.Any.Hit))
N2<-N+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTNONNATIVEmin,ymax=SASTNONNATIVEmax),
            fill="palegreen1",alpha=0.03)
N3<-N2+ggtitle("Average Percent Noxious Species ")+labs(x="SS Plots",y="Average % Noxious Species")+theme(axis.text.x=element_blank())

NF<-ggplot(SAST,aes(x=1,y=Number.Preferred.Forb.Species))
NF2<-NF+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTNUMBEROFFORBmin,ymax=SASTNUMBEROFFORBmax),
            fill="palegreen1",alpha=0.03)
NF3<-NF2+ggtitle("Number of Preferred Forbs ")+labs(x="SS Plots",y="Number of Preferred Forbs")+theme(axis.text.x=element_blank())

SAS<-ggplot(SAST,aes(x=1,y=Soil.Stability.All))
SAS2<-SAS+geom_jitter()+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=SASTSOILSTABILITYmin,ymax=SASTSOILSTABILITYmax),
            fill="palegreen1",alpha=0.03)
SAS3<-SAS2+ggtitle("Soil Stability ")+labs(x="SS Plots",y="Average Soil Stability")+theme(axis.text.x=element_blank())

grid.arrange(B3,SAS3,L3,G3,F3,N3,NF3,nrow=2, 
             top = textGrob("Display of Plots within Benchmark by Benchmark Category",gp=gpar(fontsize=20,font=3)))













