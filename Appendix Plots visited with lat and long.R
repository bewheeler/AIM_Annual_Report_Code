#plots visited
#packages installed= data.table, dplyr, formattable, tidyr using function install.packages("") for example install.packages ("tidyr")
library(tidyr)
library(data.table)
library(dplyr)


#Actual.Eco.Site is a column I added to each TerrADat csv to assign the ecological site
#CSVs have to be saved as CSV with the names as follows: plots = "Allyears_plots" , soil horizon = "Allyears_soilhorizons"
#query results = "Allyears_query", plant specis = "PSPPALL" , species richness = "Allyears_species_rich"
#LPI detail = "LPI_all"

#Plotdata has the actual.eco.site assigned, the "match" code below is adding a column that will assign actual.eco.site to every row
##using the Plotdata csv
Plotdata<-read.csv("~/Allyears_plots.csv")
AIMdata<-read.csv("~/Allyears_query.csv")
AIMdata$Actual.Eco.Site<-Plotdata$Actual.Eco.Site[match(AIMdata$Primary.Key,Plotdata$PrimaryKey)]
AIMdata$PlotID<-Plotdata$PlotID[match(AIMdata$Primary.Key,Plotdata$PrimaryKey)]
#then simply select the columns of interest and export!
AIMdata<- AIMdata %>%
  select(PlotID,Latitude_NAD83,Longitude_NAD83,Date.Visited)
write.csv(AIMdata,"PlotsVisitedlatlong.csv")