#cr master data files for cm, neamap data (weakfish)

setwd('e:/FWDP2020/angel/weak/master')

#cm first
cm.station=read.csv('cm.station.csv', h=T, sep=',', na.strings = c(NA))
cm.catch=read.csv('cm.catch.csv', h=T, sep=',', na.strings = c(NA))
cm.ind=read.csv('cm.individual.csv', h=T, sep=',', na.strings = c(NA))
cm.diet=read.csv('cm.diet.csv', h=T, sep=',', na.strings = c(NA))

names(cm.station)=tolower(names(cm.station))
names(cm.catch)=tolower(names(cm.catch))
names(cm.ind)=tolower(names(cm.ind))
names(cm.diet)=tolower(names(cm.diet))


cm.master=merge(merge(merge(cm.diet, cm.ind[,c(1,5,6,10)], by=c('station', 'specimenlabelid'), all.x=T), cm.catch[,c(1:3,7,10,11)], by=c('station'), all.x=T), cm.station[,c(1,3:8)], by=c('station'), all.x=T)
names(cm.master)[10]='length_mm_py'
names(cm.master)[14]='length_mm_pd'
names(cm.master)[9]='weight_g_py'
names(cm.master)[15]='weight_kg_pd'


write.csv(cm.master, 'cm.master.csv')


#neamap
nm.station=read.csv('neamap.station.csv', h=T, sep=',', na.strings = c(NA))
nm.catch=read.csv('neamap.catch.csv', h=T, sep=',', na.strings = c(NA))
#bring in new ind files from VIMS 6/8/22
#nm.ind=read.csv('neamap.individual.csv', h=T, sep=',', na.strings = c(NA))
nm.ind.0712=read.csv('neamap.ind.0712.csv', h=T, sep=',', na.strings = c(NA))
nm.ind.1316=read.csv('neamap.ind.1316.csv', h=T, sep=',', na.strings = c(NA))
nm.ind.1720=read.csv('neamap.ind.1720.csv', h=T, sep=',', na.strings = c(NA))
nm.diet=read.csv('neamap.diet.csv', h=T, sep=',', na.strings = c(NA))

names(nm.station)=tolower(names(nm.station))
names(nm.catch)=tolower(names(nm.catch))
names(nm.ind.0712)=tolower(names(nm.ind.0712))
names(nm.ind.1316)=tolower(names(nm.ind.1316))
names(nm.ind.1720)=tolower(names(nm.ind.1720))
names(nm.diet)=tolower(names(nm.diet))

#fix for new ind col names
names(nm.ind.1316)[5]='length_mm'
names(nm.ind.1720)[5]='length_mm'

#rbind nm.inds before merge
nm.ind=rbind(nm.ind.0712, nm.ind.1316, nm.ind.1720)

#fix for new ind 
#11022 rows.
nm.master=merge(merge(merge(nm.diet, nm.ind[,c(1,2,5,6,10)], by=c('station', 'specimenlabelid'), all.x=T), nm.catch[,c(1,2,9,10)],by=c('station'), all.x=T), nm.station[,c(1,3:8)], by=c('station'), all.x=T)

names(nm.master)[10]='length_mm_py'
names(nm.master)[15]='length_mm_pd'
names(nm.master)[9]='weight_g_py'
names(nm.master)[16]='weight_kg_pd'

nm.master2=nm.master[,c(1:13,15:17,14,18:25)]

write.csv(nm.master2, 'nm.master.csv')




