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
  
  if (is.na(current.row.categ) & !(is.na(prev.row.categ))){
    d.code.refs$Offense_Category[i] <- prev.row.categ
  }
  
  prev.row.categ <- d.code.refs$Offense_Category[i]

}

d.code.refs <- d.code.refs[!(is.na(ucr_ncic_code))]
d.code.refs[,row.num := NULL,]


d.code.refs[1:20]