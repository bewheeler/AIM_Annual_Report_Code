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
#LPI detail = "LPI_all", leaving all dataframes in in case unknown codes will be subsetted

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
ST1<-ST
ST1$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(ST1$PrimaryKey,Plotdata$PrimaryKey)]


##finding unknowns in species richness (in theory everything from LPI should be in here, but just being safe LPI unknowns were also determined)
PF<-paste0("PF",0:300)
AF<-paste0("AF",0:300)
PG<-paste0("PG",0:300)
AG<-paste0("AG",0:300)
TR<-paste0("TR",0:300)
SH<-paste0("SH",0:300)
list<-c(AF,PF,AG,PG,TR,SH)

SP2<-SPdata %>%
  select(PrimaryKey,SpeciesList,DateLoadedInDb) %>%
  filter(SpeciesList %in% list)

##checking species
PSPSdata$DateLoadedInDb<-SPdata$DateLoadedInDb[match(PSPSdata$PrimaryKey,SPdata$PrimaryKey)]
PSPS2<-PSPSdata %>%
  select(PrimaryKey,Species,DateLoadedInDb) %>%
  filter(Species %in% list)

##determining codes in LPI
STT<-ST1 %>%
  select(PrimaryKey,TopCanopy,DateLoadedInDb) %>%
  filter(TopCanopy %in% list)


STL1<-ST1 %>%
  select(PrimaryKey,Lower1,DateLoadedInDb) %>%
  filter(Lower1 %in% list)


STL2<-ST1 %>%
  select(PrimaryKey,Lower2,DateLoadedInDb) %>%
  filter(Lower2 %in% list)


STL3<-ST1 %>%
  select(PrimaryKey,Lower3,DateLoadedInDb) %>%
  filter(Lower3 %in% list)


STL4<-ST1 %>%
  select(PrimaryKey,Lower4,DateLoadedInDb) %>%
  filter(Lower4 %in% list)

STL5<-ST1 %>%
  select(PrimaryKey,Lower5,DateLoadedInDb) %>%
  filter(Lower5 %in% list)

STL6<-ST1 %>%
  select(PrimaryKey,Lower6,DateLoadedInDb) %>%
  filter(Lower6 %in% list)

STL7<-ST1 %>%
  select(PrimaryKey,Lower7,DateLoadedInDb) %>%
  filter(Lower7 %in% list)
##because a number could be repeated in different years, the code must be united with the date
STT<-unite(STT,TopCanopy,TopCanopy,DateLoadedInDb,sep="_",remove=TRUE)
STL1<-unite(STL1,Lower1,Lower1,DateLoadedInDb,sep="_",remove=TRUE)
STL2<-unite(STL2,Lower2,Lower2,DateLoadedInDb,sep="_",remove=TRUE)
STL3<-unite(STL3,Lower3,Lower3,DateLoadedInDb,sep="_",remove=TRUE)
STL4<-unite(STL4,Lower4,Lower4,DateLoadedInDb,sep="_",remove=TRUE)
STL5<-unite(STL5,Lower5,Lower5,DateLoadedInDb,sep="_",remove=TRUE)
STL6<-unite(STL6,Lower6,Lower6,DateLoadedInDb,sep="_",remove=TRUE)
STL7<-unite(STL7,Lower7,Lower7,DateLoadedInDb,sep="_",remove=TRUE)
SP2<-unite(SP2,SpeciesList,SpeciesList,DateLoadedInDb,sep="_",remove=TRUE)
PSPS2<-unite(PSPS2,Species,Species,DateLoadedInDb,sep="_",remove=TRUE)
#making a list and removing duplicates
list2<-c(as.character(STT$TopCanopy),as.character(STL1$Lower1),as.character(STL2$Lower2),as.character(STL3$Lower3),as.character(STL4$Lower4),as.character(STL5$Lower5),as.character(STL6$Lower6),as.character(STL7$Lower7),as.character(SP2$SpeciesList),as.character(PSPS2$Species))
list2<-unique(list2)
df <- data.frame(matrix(list2, nrow=length(list2), byrow=T))
df<- df %>% separate(matrix.list2..nrow...length.list2...byrow...T.,c("Unknown_Code","Date"),sep="_")
View(df)
#exporting csv to saved file
write.csv(df,"unknown_codes_in_data.csv")

