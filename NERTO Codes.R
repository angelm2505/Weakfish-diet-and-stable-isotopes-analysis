
setwd("~/Grad school (maryland)/Research/NERTO/NERTO22-20211207T152254Z-001/NERTO22")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)

#Loading the data
df1 <- read_excel(("SmithNOAA_ChesMMAP.xlsx"), sheet = "Station")
df2 <- read_excel(("SmithNOAA_ChesMMAP.xlsx"), sheet = "Individual")
df3 <- read_excel(("SmithNOAA_ChesMMAP.xlsx"), sheet = "Catch")
df4 <- read_excel(("SmithNOAA_ChesMMAP.xlsx"), sheet = "Diet")

df5 <- read_excel(("SmithNOAA_NEAMAP.xlsx"), sheet = "Station")
df6 <- read_excel(("SmithNOAA_NEAMAP.xlsx"), sheet = "Individual")
df7 <- read_excel(("SmithNOAA_NEAMAP.xlsx"), sheet = "Catch")
df8 <- read_excel(("SmithNOAA_NEAMAP.xlsx"), sheet = "Diet")

#cm sheets joining
joined_v1 <- left_join(df4, df2[,c(1, 5, 6, 10)], 
                       by = c("STATION", "SpecimenLabelID"))

joined_v2 <- left_join(joined_v1, df3[,c(1:3, 7, 10, 11)], 
                       by = c("STATION"))

joined_v3 <- left_join(joined_v2, df1[,c(1, 3:6, 7, 8)], 
                       by = c("STATION"))

#creating csv file for cm
write.csv(joined_v3, 'cm.master.angel.csv') 

#nm sheets joining
joined_v4 <- left_join(df8,df6[,c(1,2,5,6,10)], 
                       by = c("STATION", "SpecimenLabelID"))

joined_v5 <- left_join(joined_v4, df7[,c(1,2,9,10)], 
                       by = c("STATION"))

joined_v6 <- left_join(joined_v5, df4[,c(1,3:6,7,8)], 
                       by = c("STATION"))                        

#creating csv file for nm
write.csv(joined_v6, 'nm.master.angel.csv') 







#cr master data files for cm, neamap data (weakfish)
cm <- read.csv('cm.master.csv')
nm <- read.csv('nm.master.csv')
load("nmfs.master.RData")
nmfs=nmfs.master

#data manipulation and setup for CCA

#3 dataframes (data sets), ultimately want bind these as 1 dataframe, but need to do some manipulation first.
cm
nm
nmfs

## rm no catch records!!
#data manipulation, collapse cm, nm over py length.
#Aggregate function works as a sum
cm.w=aggregate(list('pyamtw'=cm$weight_g_py), 
               by=list('station'=cm$station, 
                       'pdid'=cm$specimenlabelid, 
                       'preyspeciescommon'=cm$preyspeciescommon, 
                       'preyspecieslatin'=cm$preyspecieslatin, 
                       'length_mm_pd'=cm$length_mm_pd, 
                       'weight_kg_pd'=cm$weight_kg_pd, 
                       'year'=cm$year, 'month'=cm$month, 
                       'date'=cm$date, 'count'=cm$count, 
                       'biomass_kg'=cm$biomass_kg, 
                       'season'=cm$season, 
                       'lat'=cm$lat, 'long'=cm$long, 
                       'bottomwatertemp_c'=cm$bottomwatertemp_c, 
                       'stratumarea_km2'=cm$stratumarea_km2), sum, na.rm=T)

cm.n=aggregate(list('pyamtn'=cm$number), 
               by=list('station'=cm$station,'pdid'=cm$specimenlabelid, 
                       'preyspeciescommon'=cm$preyspeciescommon, 
                       'preyspecieslatin'=cm$preyspecieslatin,
                       'length_mm_pd'=cm$length_mm_pd,
                       'weight_kg_pd'=cm$weight_kg_pd, 'year'=cm$year, 
                       'month'=cm$month, 'date'=cm$date, 'count'=cm$count, 
                       'biomass_kg'=cm$biomass_kg, 'season'=cm$season, 
                       'lat'=cm$lat, 'long'=cm$long, 
                       'bottomwatertemp_c'=cm$bottomwatertemp_c, 
                       'stratumarea_km2'=cm$stratumarea_km2), sum, na.rm=T)

cm2=merge(cm.w, cm.n[,c(1:4,17)], by=c('station', 'pdid', 'preyspeciescommon', 
                                       'preyspecieslatin'), all.x=T)

cm2$area= 'CB'

#aggregate (list('name'=data$col)) <- aggregating by that specific column and 
#then naming that column

nm.n=aggregate(list('pyamtn'=nm$number), 
               by=list('station'=nm$station,'pdid'=nm$specimenlabelid, 
                       'preyspeciescommon'=nm$preyspeciescommon, 
                       'preyspecieslatin'=nm$preyspecieslatin,
                       'length_mm_pd'=nm$length_mm_pd,
                       'weight_kg_pd'=nm$weight_kg_pd, 'year'=nm$year
                       , 'date'=nm$date, 'count'=nm$count, 
                       'biomass_kg'=nm$biomass_kg, 'season'=nm$season, 
                       'lat'=nm$lat, 'long'=nm$long, 
                       'bottomwatertemp_c'=nm$bottomwatertemp_c, 
                       'stratumarea_km2'=nm$stratumarea_km2), sum, na.rm=T)

nm.w=aggregate(list('pyamtw'=nm$weight_g_p), 
               by=list('station'=nm$station,'pdid'=nm$specimenlabelid, 
                       'preyspeciescommon'=nm$preyspeciescommon, 
                       'preyspecieslatin'=nm$preyspecieslatin,
                       'length_mm_pd'=nm$length_mm_pd,
                       'weight_kg_pd'=nm$weight_kg_pd, 'year'=nm$year
                       , 'date'=nm$date, 'count'=nm$count, 
                       'biomass_kg'=nm$biomass_kg, 'season'=nm$season, 
                       'lat'=nm$lat, 'long'=nm$long, 
                       'bottomwatertemp_c'=nm$bottomwatertemp_c, 
                       'stratumarea_km2'=nm$stratumarea_km2), sum, na.rm=T)

nm2=merge(nm.w, nm.n[,c(1:4,16)], by=c('station', 'pdid', 'preyspeciescommon', 
                                       'preyspecieslatin'), all.x=T)


nm2$area = 'Coast'

#need to remove month from cm2 for rbind
cm2$month=NULL

#Merging cm and nm
cm_nm <- rbind(cm2, nm2)

write.csv(cm_nm, 'cm.nm.csv')

cm.nm <- read.csv()

head(nmfs)

#for all data, need to standardize column headings, will also need to create standardized prey categories.