#data manipulation and setup for CCA

#set local working directory
setwd("e:/FWDP2020/angel/weak/data/")

library(googledrive)
#authentication and access to google account.
drive_auth()


#set local working directory
#locate files and download into local folder.
drive_find(type='csv')
drive_download('cm.master.csv', overwrite = T)
drive_download('nm.master.csv', overwrite = T)

drive_find(q = "name contains 'nmfs.master'")
drive_download('nmfs.master.RData', overwrite = T)

cm=read.csv('cm.master.csv', sep=',', h=T, na.strings=c(NA))
nm=read.csv('nm.master.csv', sep=',', h=T, na.strings=c(NA))

load('nmfs.master.RData')

#rename nmfs.master
nmfs=nmfs.master

#3 dataframes (data sets), ultimately want bind these as 1 dataframe, but need to do some manipulation first.
cm
nm
nmfs

## rm no catch records!!
#data manipulation, collapse cm, nm over py length.
cm.w=aggregate(list('pyamtw'=cm$weight_g_py), by=list('station'=cm$station, 'stratum'=cm$stratum, 'pdid'=cm$specimenlabelid, 'preyspeciescommon'=cm$preyspeciescommon, 'preyspecieslatin'=cm$preyspecieslatin, 'length_mm_pd'=cm$length_mm_pd, 'weight_kg_pd'=cm$weight_kg_pd, 'expfactmeas'=cm$expfactmeas, 'year'=cm$year, 'month'=cm$month, 'date'=cm$date, 'count'=cm$count, 'biomass_kg'=cm$biomass_kg, 'season'=cm$season, 'lat'=cm$lat, 'long'=cm$long, 'bottomwatertemp_c'=cm$bottomwatertemp_c, 'stratumarea_km2'=cm$stratumarea_km2), sum, na.rm=T)
cm.n=aggregate(list('pyamtn'=cm$number), by=list('station'=cm$station,'stratum'=cm$stratum, 'pdid'=cm$specimenlabelid, 'preyspeciescommon'=cm$preyspeciescommon, 'preyspecieslatin'=cm$preyspecieslatin, 'length_mm_pd'=cm$length_mm_pd, 'weight_kg_pd'=cm$weight_kg_pd, 'expfactmeas'=cm$expfactmeas, 'year'=cm$year, 'month'=cm$month, 'date'=cm$date, 'count'=cm$count, 'biomass_kg'=cm$biomass_kg, 'season'=cm$season, 'lat'=cm$lat, 'long'=cm$long, 'bottomwatertemp_c'=cm$bottomwatertemp_c, 'stratumarea_km2'=cm$stratumarea_km2), sum, na.rm=T)
cm2=merge(cm.w, cm.n[,c(1:5,19)], by=c('station','stratum', 'pdid', 'preyspeciescommon', 'preyspecieslatin'), all.x=T)
cm2$area='CB'

#need to remove month from cm2 for rbind
cm2$month=NULL

#FIXED add fix here; waiting for response back from neamap-- only gave us individual data 2007-2013, need 2007-2021.**********************
#for now moving forward with null lengths and weights for 2014+
#nm.w=aggregate(list('pyamtw'=nm$weight_g_py), by=list('station'=nm$station, 'stratum'=nm$stratum, 'pdid'=nm$specimenlabelid, 'preyspeciescommon'=nm$preyspeciescommon, 'preyspecieslatin'=nm$preyspecieslatin,  'year'=nm$year,'count'=nm$count, 'biomass_kg'=nm$biomass_kg, 'season'=nm$season, 'lat'=nm$lat, 'long'=nm$long, 'bottomwatertemp_c'=nm$bottomwatertemp_c, 'stratumarea_km2'=nm$stratumarea_km2),sum, na.rm=T)#,  had to rm length_mm_pd, weight_kg_pd, and date
nm.w=aggregate(list('pyamtw'=nm$weight_g_py), by=list('station'=nm$station, 'stratum'=nm$stratum, 'pdid'=nm$specimenlabelid, 'preyspeciescommon'=nm$preyspeciescommon, 'preyspecieslatin'=nm$preyspecieslatin, 'length_mm_pd'=nm$length_mm_pd, 'weight_kg_pd'=nm$weight_kg_pd, 'expfactmeas'=nm$expfactmeas, 'year'=nm$year, 'date'=nm$date, 'count'=nm$count, 'biomass_kg'=nm$biomass_kg, 'season'=nm$season, 'lat'=nm$lat, 'long'=nm$long, 'bottomwatertemp_c'=nm$bottomwatertemp_c, 'stratumarea_km2'=nm$stratumarea_km2), sum, na.rm=T)
nm.n=aggregate(list('pyamtn'=nm$number), by=list('station'=nm$station, 'stratum'=nm$stratum, 'pdid'=nm$specimenlabelid, 'preyspeciescommon'=nm$preyspeciescommon, 'preyspecieslatin'=nm$preyspecieslatin, 'year'=nm$year, 'count'=nm$count, 'biomass_kg'=nm$biomass_kg, 'season'=nm$season, 'lat'=nm$lat, 'long'=nm$long, 'bottomwatertemp_c'=nm$bottomwatertemp_c, 'stratumarea_km2'=nm$stratumarea_km2), sum, na.rm=T)
nm2=merge(nm.w, nm.n[,c(1:5,14)], by=c('station', 'stratum', 'pdid', 'preyspeciescommon', 'preyspecieslatin'), all.x=T)
nm2$area='COAST'

#FIXED 
#add in length_mm_pd, weight_kg_pd, and date, need to add specimenlabelid back to nm2
#nm2$specimenlabelid=nm2$pdid

#FIXED
#create nm with unique station, specimenlabelid, and lenth_mm_pd , weight_kg_pd, date, and year
#nm.fix=unique(nm[,c('station', 'specimenlabelid', 'length_mm_pd', 'weight_kg_pd', 'date', 'year' )])
#nm3=merge(nm2, nm.fix[,c(1:5)], by=c('station', 'specimenlabelid'), all.x=T)

#FIXED
#reorder nm3 to match cm2
#nm4=nm3[,c(1,3:6,18,19,7,20,8:17)]
#rbind cm2, nm2
d1=rbind(cm2, nm2)


#for all data, need to standardize column headings, will also need to create standardized prey categories.
#nmfs data keep cols
kp=c('cruise6', 'station', 'pdid', 'pdlen', 'pdwgt', 'sizecat', 'stratum', 'declat', 'declon', 'year', 'season', 'geoarea', 'collcat', 'collsci', 'pyamtw', 'pynum', 'catnum', 'catwgt', 'numlen', 'stratum_area', 'bottemp')

nmfs2=nmfs[,kp]
#change col names then rearrange to match d1 or to match nmfs
#convert length_mm_pd to pdlen in cm
d1$pdlen= round(d1$length_mm_pd/10, 0)
d1$cruise6=NA
d1$pdwgt=d1$weight_kg_pd/1000
#create standard size categories to match nmfs data
d1$sizecat=ifelse(d1$pdlen<=25, 'S', ifelse(d1$pdlen>=26&d1$pdlen<=50, 'M', ifelse(d1$pdlen>50, 'L', 'X')))
#create standard season categories to match nmfs data
d1$season=ifelse(d1$season=='Spring', 'SPRING', ifelse(d1$season=='Fall', 'FALL', ifelse(d1$season=='March'|d1$season=='May', 'SPRING', ifelse(d1$season=='July', 'SUMMER', ifelse(d1$season=='September'|d1$season=='November', 'FALL', 'x')))))
d1$collcat=NA
d1$collsci=d1$preyspecieslatin
d1$catnum=d1$count
d1$catwgt=d1$biomass_kg
d1$stratum_area=d1$stratumarea_km2
d1$bottemp=d1$bottomwatertemp_c
d1$bottomwatertemp_c=NULL
d1$stratumarea_km2=NULL
d1$biomass_kg=NULL
d1$count=NULL
d1$length_mm_pd=NULL
d1$weight_kg_pd=NULL
d1$preyspecieslatin=NULL

#bring in cm and nm and nmfs.count.sizecat
cm.count.sizecat=read.csv('e:/FWDP2020/angel/weak/master/cm.count.sizecat.csv', h=T, sep=',', na.strings = c(NA))
nm.count.sizecat=read.csv('e:/FWDP2020/angel/weak/master/nm.count.sizecat.csv', h=T, sep=',', na.strings = c(NA))
nmfs.count.sizecat=read.csv('e:/FWDP2020/angel/weak/master/nmfs.count.sizecat.csv', h=T, sep=',', na.strings = c(NA))
ct.size=rbind(cm.count.sizecat, nm.count.sizecat)

d1.=merge(d1, ct.size[,c(2,4,5)], by=c('station', 'sizecat'), all.x=T)
d1.$numlen=d1.$expfactmeas

#make some changes to nmfs cols
colnames(nmfs2)[8:9]=c('lat', 'long')
colnames(nmfs2)[12]='area'
colnames(nmfs2)[16]='pyamtn'
nmfs2$preyspeciescommon=NA
nmfs2$date=NA

#merge count.sizecat into nmfs2
nmfs2.=merge(nmfs2, nmfs.count.sizecat[,c(2:5)], by=c('cruise6', 'station', 'sizecat'), all.x = T)


#reorder columns to match
nmfs3=nmfs2.[,c(1,2,4,5,6,3,7:19,24,20:23)]
d2=d1.[,c(16,1,4,15,17,2,3,10:11,7,9,14,18,19,12,13,20,21,25,24,22,23,5,8)]



#final dataset
d=rbind(d2,nmfs3)

#make prey names (collsci) all caps for now.
d$collsci=toupper(d$collsci)


#create standard list of prey names.
pylist=sort(unique(d$collsci))
sum.pylist=aggregate(list('tpyamtw'=d$pyamtw), by=list('collsci'=d$collsci), sum,na.rm=T)
sum.pylist$tot=sum(sum.pylist$tpyamtw)
sum.pylist$pct=(sum.pylist$tpyamtw/sum.pylist$tot)*100
#write.csv(sum.pylist, 'sum.pylist.csv')

#bring in updated sum.pylist2 with standardized prey names.

sum.pylist2=read.csv('sum.pylist2.csv', sep=',', h=T, na.strings = c(NA))
names(d)

dfin=merge(d, sum.pylist2[,c(2,3)], by=c('collsci'),all.x = T)

#final dataset w standardized prey names
dfin

#check sums of pyamtw with 'test' dataset.
test=aggregate(list('tpyamtw'=dfin$pyamtw), by=list('collsci2'=dfin$collsci2), sum, na.rm=T)
test$tot=sum(test$tpyamtw)
test$pct=(test$tpyamtw/test$tot)*100
test2=test[order(-test$pct),]
sum(test2$pct[test2$pct>1])

#check data-- found 1 nm stomach w acetes sp (shrimp) at 3436 g-- error 
#also 1 stom w 3173 g of unid fish, 1 stom w 584 g of unid fish and small pdlen, 1 stom w 473 g bay anchovy and small pdlen
dfin$qa=ifelse((dfin$station=='NM20070901112'&dfin$pdid==7)|(dfin$station=='NM20120901007'&dfin$pdid==6)|(dfin$station=='NM20110901042'&dfin$pdid==19)|(dfin$station=='NM20120901019'&dfin$pdid==10), 'rm', 'kp')

#rm stomachs w error, 5 ROWS
dfin2=subset(dfin, dfin$qa!='rm')
#rm qa column
dfin2$qa=NULL

#recheck sums of pyamtw with 'test' dataset.
test=aggregate(list('tpyamtw'=dfin2$pyamtw), by=list('collsci2'=dfin2$collsci2), sum, na.rm=T)
test$tot=sum(test$tpyamtw)
test$pct=(test$tpyamtw/test$tot)*100
test2=test[order(-test$pct),]
sum(test2$pct[test2$pct>1])

#add column for dataset cm, nm, nmfs
G=c('MAB', 'SNE', 'GB', 'GoM')
dfin2$data=ifelse(dfin2$area=='CB', 'CM', ifelse(dfin2$area=='COAST', 'NM', ifelse(dfin2$area%in%G, 'NMFS', 'x')))

#create new station column to rm 'NM...' and 'CM...'
dfin2$station2=ifelse(dfin2$data=='CM'|dfin2$data=='NM', substr(dfin2$station, 3,nchar(dfin2$station)), dfin2$station)
dfin2$station=as.numeric(dfin2$station2)
dfin2$station2=NULL

#create cruise6 for cm and nm data
dfin2$cruise6.=ifelse(dfin2$data=='CM'|dfin2$data=='NM', substr(dfin2$station, 1,6), dfin2$cruise6)
dfin2$cruise6=as.numeric(dfin2$cruise6.)
dfin2$cruise6.=NULL

#create collsci3 with prey groupings? hold for now.


#fix for collsci2
dfin2$collsci2=ifelse(dfin2$collsci=='ALOSA SPP.', 'ALOSA SP', ifelse(dfin2$collsci=='LIBINIA SPP.', 'DECAPODA CRAB', as.character(dfin2$collsci2)))

#write dfin for analyses.
write.csv(dfin2, 'dfin.csv')
