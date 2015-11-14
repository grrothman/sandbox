#clear environment
#rm(list = ls(all = TRUE))
#gc(reset=TRUE)

#load relevant analysis libraries if first open of studio
library('data.table')
library('ggplot2')
library('scales')
library('grid')

d.orig <- as.data.table(read.csv('~/Documents/sandbox/D3_shtuff/crime_data_scratch/data.csv'))
d <- copy(d.orig)

d.code <- d[,unique(crimedescr),ucr_ncic_code]
d.code <- d.code[order(ucr_ncic_code)]

# ggplot(d.code[,ucr_ncic_code := factor(ucr_ncic_code)], aes(x=ucr_ncic_code))+
#   geom_histogram(binwidth = 1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

d.code <- d.code[,frequency := .N, ucr_ncic_code]
#d.code.singles <- d.code[frequency==1]

d.code.refs.orig <- as.data.table(read.csv('~/Documents/sandbox/D3_shtuff/crime_data_scratch/NCIC codes ref.csv'))
d.code.refs <- copy(d.code.refs.orig)
d.code.refs[, NCIC_Uniform_Offense_Classifications := as.character(NCIC_Uniform_Offense_Classifications),]
d.code.refs[,row.num := .I,]
d.code.categs <- d.code.refs[toupper(substr(NCIC_Uniform_Offense_Classifications,1,3))==substr(NCIC_Uniform_Offense_Classifications,1,3)]
setnames(d.code.categs, "NCIC_Uniform_Offense_Classifications", "Offense_Category")

#d.code.categs[,row.num := NULL,]
d.code.refs <- merge(d.code.refs,d.code.categs[,row.num,list(Offense_Category)],'row.num', all.x=TRUE)
d.code.refs <- d.code.refs[order(row.num)]


prev.row <- NA
for (i in seq(1,d.code.refs[,.N])) {
  current.row.categ <- d.code.refs$Offense_Category[i]
  
  if (is.na(current.row.categ) & !(is.na(prev.row))){
    d.code.refs$Offense_Category[i] <- prev.row
  }
  
  prev.row <- d.code.refs$Offense_Category[i]

}

d.code.refs <- d.code.refs[!(is.na(ucr_ncic_code))]
d.code.refs[,row.num := NULL,]

d.w.categs <- merge(d,d.code.refs, c('ucr_ncic_code'), all.x = TRUE)

d.w.categs <- d.w.categs[ucr_ncic_code==7000, Offense_Category := crimedescr,]

d.w.categs[,code.freq := .N, ucr_ncic_code]
d.w.categs[,categ.freq := .N, Offense_Category]
d.w.categs[,district.freq := .N, district]
d.w.categs[,beat.freq := .N, beat]
d.w.categs[,grid.freq := .N, grid]

d.w.categs[, beat := substring(beat,2,2),]

palette <- c('#1f78b4', '#ff7f00', '#33a02c', '#6a3d9a', '#e31a1c', '#fdbf6f', '#b15928', '#a6cee3', '#b2df8a', '#cab2d6', '#fb9a99', '#ffff99', '#A8A8A8', '#000000', '#999999')
ggplot(d.w.categs, aes(x=longitude, y=latitude, shape=as.factor(district), size=district.freq))+
  geom_point(alpha=0.5)+
  #geom_point(data = d.w.categs, shape=15, size=2, aes(x=longitude, y=latitude, color=beat))+
  #scale_color_manual(values=palette)+
  scale_y_continuous(breaks=seq(38.3,38.8,0.025))+
  scale_x_continuous(breaks=seq(-121.7,-121.2,0.025))+
  theme_bw()+
  guides(colour = guide_legend(override.aes = list(size=10)))

d.categ.class <- unique(d.w.categs[,min(ucr_ncic_code),Offense_Category])
d.categ.class[V1<1600,violence := 'violent']
d.categ.class[V1>2200 & V1<2500,violence := 'violent']
d.categ.class[Offense_Category=='ASSAULT WITH WEAPON - I RPT',violence := 'violent']
d.categ.class[Offense_Category=='SHOOT INTO OCCUP DWELL - I RPT',violence := 'violent']
d.categ.class[Offense_Category=='BURGLARY - I RPT',violence := 'violent']
d.categ.class[Offense_Category=='SHOOT INTO OCCUP DWELL - I RPT',violence := 'violent']
d.categ.class[Offense_Category=='ROBBERY - I RPT',violence := 'violent']
d.categ.class[Offense_Category=='BOMBS/THREATS/EXPLOSIV- I RPT',violence := 'violent']
d.categ.class[Offense_Category=='BOMBS/THREATS/EXPLOSIV- I RPT',violence := 'nonviolent']
d.categ.class[Offense_Category=='HOMICIDE ASSAULT - I RPT',violence := 'nonviolent']
d.categ.class[is.na(violence),violence := 'nonviolent']

d.w.categs <- merge(d.w.categs,d.categ.class[,violence,Offense_Category],c('Offense_Category'))

