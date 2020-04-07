#packages installed= data.table, dplyr, formattable, tidyr using function install.packages("") for example install.packages ("tidyr")
library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(formattable)
library(qwraps2)
library(stringr)

#Actual.Eco.Site is a column I added to each TerrADat csv to assign the ecological site
#CSVs have to be saved as CSV with the names as follows: plots = "Allyears_plots" , soil horizon = "Allyears_soilhorizons"
#query results = "Allyears_query", plant specis = "PSPPALL" , species richness = "Allyears_species_rich"
#LPI detail = "LPI_all"

#Plotdata has the actual.eco.site assigned, the "match" code below is adding a column that will assign actual.eco.site to every row
##using the Plotdata csv
Plotdata<-read.csv("~/Allyears_plots.csv")
AIMdata<-read.csv("~/Allyears_query.csv")
AIMdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(AIMdata$Primary.Key,Plotdata$PrimaryKey)]
PSPSdata<-read.csv("~/PSPPALL.csv")
PSPSdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(PSPSdata$PrimaryKey,Plotdata$PrimaryKey)]
SPdata<-read.csv("~/Allyears_species_rich.csv")
SPdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(SPdata$PrimaryKey,Plotdata$PrimaryKey)]
ST<-read.csv("~/LPI_all.csv")
#when downloading LPI detail from TerrADat online you will have to label the PrimaryKey column PrimaryKey, you can use the View()
##command to ensure you're matching with the correct column
ST1<-ST
ST<-ST %>%
  select(HeightWoody,HeightHerbaceous,SpeciesWoody,SpeciesHerbaceous,PrimaryKey)
STN<-na.omit(ST)
STN$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(STN$PrimaryKey,Plotdata$PrimaryKey)]
ST1$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(ST1$PrimaryKey,Plotdata$PrimaryKey)]

PS2<-PSPSdata[!duplicated(PSPSdata$Species),]

ST1$Topcancovertype<-PS2$GrowthHabitSub[match(ST1$TopCanopy,PS2$Species)]  
ST1$l1covertype<-PS2$GrowthHabitSub[match(ST1$Lower1,PS2$Species)]  
ST1$l2cancovertype<-PS2$GrowthHabitSub[match(ST1$Lower2,PS2$Species)]  
ST1$l3cancovertype<-PS2$GrowthHabitSub[match(ST1$Lower3,PS2$Species)]  
ST1$l4cancovertype<-PS2$GrowthHabitSub[match(ST1$Lower4,PS2$Species)]  
ST1$l5cancovertype<-PS2$GrowthHabitSub[match(ST1$Lower5,PS2$Species)]  
ST1$l6cancovertype<-PS2$GrowthHabitSub[match(ST1$Lower6,PS2$Species)]  
ST1$l7cancovertype<-PS2$GrowthHabitSub[match(ST1$Lower7,PS2$Species)]  

#not used but helpful for converting a list to a dataframe, needs package plyr
test<- apply(ST1, 2, function(x) which(x == "Sedge"))
str(test)
length(test)
df <- ldply (test, data.frame)
str(df)
df<-spread(df,.id,X..i..)
#
#
ST2<-unite(ST1,combined,Topcancovertype,l1covertype,l2cancovertype,l3cancovertype,l4cancovertype,l5cancovertype,l6cancovertype,l7cancovertype,sep=".",remove=TRUE)
ST2<- ST2 %>%
  filter(str_detect(combined,"Sedge"))
ST2<- ST2 %>%
  group_by(PrimaryKey) %>%
  mutate(sedgehits=length(PrimaryKey)) %>%
  ungroup()

ST1$sedgehits<-ST2$sedgehits[match(ST1$PrimaryKey,ST2$PrimaryKey)]

ST1<- ST1 %>%
  group_by(PrimaryKey) %>%
  mutate(totalplothits=length(PrimaryKey)) %>%
  mutate(Sedge.Pct.Any.Hit=(sedgehits/totalplothits)*100)%>%
  ungroup()
ST1$Sedge.Pct.Any.Hit[is.na(ST1$Sedge.Pct.Any.Hit)] <- 0
AIMdata$Sedge.Pct.Any.Hit<-ST1$Sedge.Pct.Any.Hit[match(AIMdata$Primary.Key,ST1$PrimaryKey)]
View(AIMdata)

