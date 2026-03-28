#weak analyses and CCA

#change to your personal wd
setwd('e:/FWDP2020/angel/weak/data')

d=read.csv('dfin.csv', sep=',', h=T, na.strings = c(NA))

#limit years to 2007-2019 to match nm and nmfs data
#cb-- 2002-2018
#nm-- 2007-2021
#nmfs--1973-2019
d.=subset(d, d$year>=2007&d$year<=2019)

#remove stomachs with no catch and zero weight etc, keep EMPTY.
#rm prey NEMATODA, CRUSTACEA LARVAE, MACRO ALGAE, NO CATCH, NO SAMPLE TAKEN, PLATYHELMINTHES
`%nin%`=Negate(`%in%`)
rm.py=c('NEMATODA', 'CRUSTACEA LARVAE', 'MACRO ALGAE', 'NO CATCH', 'NO SAMPLE TAKEN', 'PLATYHELMINTHES')

d2=subset(d., d.$collsci%nin%rm.py)

#order by cruise6, station, pdid, then create id number for each stomach, find n stomachs.
d3=d2[order(d2$cruise6, d2$station, d2$pdid),]



#count n stomachs
d.stom=aggregate(list('tpyamtw'=d3$pyamtw), by=list('cruise6'=d3$cruise6, 'station'=d3$station, 'pdid'=d3$pdid, 'data'=d3$data, 'year'=d3$year, 'area'=d3$area, 'season'=d3$season, 'sizecat'=d3$sizecat),sum, na.rm=T)
#count n stomachs by factors
d.stom.area=aggregate(list('nstom'=d.stom$pdid), by=list('area'=d.stom$area),length)
d.stom.year=aggregate(list('nstom'=d.stom$pdid), by=list('year'=d.stom$year),length)
d.stom.season=aggregate(list('nstom'=d.stom$pdid), by=list('season'=d.stom$season),length)
d.stom.size=aggregate(list('nstom'=d.stom$pdid), by=list('sizecat'=d.stom$sizecat),length)

#count n stomachs by area, year, and season (by sampling design)
d.stom.fac=aggregate(list('nstom'=d.stom$pdid), by=list('area'=d.stom$area,'year'=d.stom$year, 'season'=d.stom$season, 'sizecat'=d.stom$sizecat),length)
#looks like weakfish move inshore/cb in spring, so little/no stomachs from nmfs spring..
#instead of year combine by ?
d.stom$year2=ifelse(d.stom$year>=2007&d.stom$year<=2013, '2007.2013', ifelse(d.stom$year>=2014&d.stom$year<=2019, '2014.2019', 'x'))
d.stom$year3=ifelse(d.stom$year>=2007&d.stom$year<=2010, '2007.2010', ifelse(d.stom$year>=2011&d.stom$year<=2014, '2011.2014', ifelse(d.stom$year>=2015, '2015.2019', 'x')))

#also combine SNE/MAB as MAB for nmfs
d.stom$area2=ifelse(d.stom$area=='MAB'|d.stom$area=='SNE', 'MAB', as.character(d.stom$area))
d.stom.fac=aggregate(list('nstom'=d.stom$pdid), by=list('area2'=d.stom$area2, 'year2'=d.stom$year2, 'season'=d.stom$season, 'sizecat'=d.stom$sizecat),length)
#looks like factors to include will be area2: CB, COAST, MAB; year2: 2007-2013 and 2014-2019; season: spring and fall (limited data for mab spring); sizecat: M and S. 
#3x2x2x2=24 combinations

#dataset to use, factors: area2, year2, season, sizecat
d3$area2=ifelse(d3$area=='MAB'|d3$area=='SNE', 'MAB', as.character(d3$area))
d3$year2=ifelse(d3$year>=2007&d3$year<=2013, '2007.2013', ifelse(d3$year>=2014&d3$year<=2019, '2014.2019', 'x'))

A=c('MAB', 'COAST', 'CB')

d4=subset(d3, (d3$sizecat=='M'|d3$sizecat=='S')&(d3$season=='SPRING'|d3$season=='FALL')&(d3$area2%in%A))


#limit factor combinations to those with min number of stomachs.
#troph diversity curves









#perform weighted diet amounts across the factors- year, area, season, sizecat
#set up data and run cca


