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

ggplot(d.code[,ucr_ncic_code := factor(ucr_ncic_code)], aes(x=ucr_ncic_code))+
  geom_histogram(binwidth = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

d.code <- d.code[,frequency := .N, ucr_ncic_code]
d.code.singles <- d.code[frequency==1]

d.code.refs.orig <- as.data.table(read.csv('~/Documents/sandbox/D3_shtuff/crime_data_scratch/NCIC codes ref.csv'))
d.code.refs <- copy(d.code.refs.orig)
d.code.refs[,row.num := .I,]
d.code.categs <- d.code.refs[toupper(NCIC_Uniform_Offense_Classifications)==NCIC_Uniform_Offense_Classifications]
d.code.categs[,categ.id := .I,] #rownum=id
d.code.categs[,row.num := NULL,]
d.code.refs <- merge(d.code.refs,d.code.categs,c('NCIC_Uniform_Offense_Classifications','ucr_ncic_code','nibrs_Code'), all.x=TRUE)
d.code.refs <- d.code.refs[order(row.num)]
#copy categ.id down where categ.id is.na and .I-1 !is.na



