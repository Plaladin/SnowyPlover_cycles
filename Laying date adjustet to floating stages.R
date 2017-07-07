# adjust laying date to floating stage of youngest egg

# load data
source(paste(wd, 'Prepare_Data.R',sep=""))
fs <- read.csv(file=paste(wd, "floating_stages.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE)
com <- read.csv(file=paste(wd, "comments.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE)

fs$nest_id <- paste(fs$year,fs$site,fs$nest,sep="")


nn$float_1 <- fs$float_1[match(nn$nest,fs$nest_id)]
nn$float_2 <- fs$float_2[match(nn$nest,fs$nest_id)]
nn$float_3 <- fs$float_3[match(nn$nest,fs$nest_id)]
nn$clutch_size <- fs$clutch_size[match(nn$nest,fs$nest_id)]
# for comparison
#nn$float_1_ <- fs$float_1[match(nn$nest,fs$nest_id)]
#nn$float_2_ <- fs$float_2[match(nn$nest,fs$nest_id)]
#nn$float_3_ <- fs$float_3[match(nn$nest,fs$nest_id)]

#for failed nests we take the floating stage of the youngest (!) egg when floated. Result in days minus found date
#A - 0 days meaning lay date is the same date as floated, e.g. if found on 501, the lay date is 501
#AB- 1 day meaning lay date is the same date as floated, e.g. if found on 501, the lay date is 430
#B - 2 days meaning lay date is found date minus 2 days, e.g. if found on 501, the lay date is 429
#C - 5 days …
#D - 8 days
#E - 10 days
#F - 11 days (and older), e.g. if found on 501, the lay date is 420

# change the floating stages to their according numeric value

nn$float_1<- gsub("AB","1",nn$float_1)
nn$float_1<- gsub("A","0",nn$float_1)
nn$float_1<- gsub("B","2",nn$float_1)
nn$float_1<- gsub("C","5",nn$float_1)
nn$float_1<- gsub("D","8",nn$float_1)
nn$float_1<- gsub("E","10",nn$float_1)
nn$float_1<- gsub("F","11",nn$float_1)

nn$float_2<- gsub("AB","1",nn$float_2)
nn$float_2<- gsub("A","0",nn$float_2)
nn$float_2<- gsub("B","2",nn$float_2)
nn$float_2<- gsub("C","5",nn$float_2)
nn$float_2<- gsub("D","8",nn$float_2)
nn$float_2<- gsub("E","10",nn$float_2)
nn$float_2<- gsub("F","11",nn$float_2)

nn$float_3<- gsub("AB","1",nn$float_3)
nn$float_3<- gsub("A","0",nn$float_3)
nn$float_3<- gsub("B","2",nn$float_3)
nn$float_3<- gsub("C","5",nn$float_3)
nn$float_3<- gsub("D","8",nn$float_3)
nn$float_3<- gsub("E","10",nn$float_3)
nn$float_3<- gsub("F","11",nn$float_3)

nn$float_min= pmin(nn$float_1,nn$float_2,nn$float_3, na.rm=TRUE) # smallest value equals the youngest egg
nn$float_min<- as.numeric(nn$float_min)
nn$found <- as.Date(nn$found, "%Y-%m-%d")

library(lubridate)
nn$laid_new <- nn$found - days(nn$float_min) # new laying date

fs$laid_new <- nn$laid_new[match(fs$nest_id,nn$nest)]

com$nest_id <- paste(com$year,com$site,com$nest,sep="")

fs$comment <- com$comment[match(fs$nest_id,com$nest_id)]
fs$laid_old <- nn$laid[match(fs$nest_id,nn$nest)]
fs$fate <- nn$fate[match(fs$nest_id,nn$nest)]
fs$found <- nn$found[match(fs$nest_id,nn$nest)]
fs<- fs[,c(1,8,4,5,6,7,12,13,11,9,10)]

#check comments and change laid_new back to laid_old if needed

fs$laid_old <- as.Date(fs$laid_old, "%Y-%m-%d",tz = "UTC")
fs$laid_new <- as.Date(fs$laid_new, "%Y-%m-%d",tz = "UTC")

fs$laid_new[fs$nest_id=='2006B1'] = fs$laid_old[fs$nest_id=='2006B1']
fs$laid_new[fs$nest_id=='2006B208'] = fs$laid_old[fs$nest_id=='2006B208']
fs$laid_new[fs$nest_id=='2006B209'] = fs$laid_old[fs$nest_id=='2006B209']
fs$laid_new[fs$nest_id=='2006D43'] = fs$laid_old[fs$nest_id=='2006D43']
fs$laid_new[fs$nest_id=='2007A123'] = fs$laid_old[fs$nest_id=='2007A123']
fs$laid_new[fs$nest_id=='2007B103'] = fs$laid_old[fs$nest_id=='2007B103']
fs$laid_new[fs$nest_id=='2009C104'] = fs$laid_old[fs$nest_id=='2009C104']
fs$laid_new[fs$nest_id=='2010A106'] = fs$laid_old[fs$nest_id=='2010A106']
fs$laid_new[fs$nest_id=='2010A108'] = fs$laid_old[fs$nest_id=='2010A108']
fs$laid_new[fs$nest_id=='2010B101'] = fs$laid_old[fs$nest_id=='2010B101']
fs$laid_new[fs$nest_id=='2016A5'] = fs$laid_old[fs$nest_id=='2016A5']

nn <- nn[,c(1:20)]
nn$laid <- fs$laid_new[match(nn$nest, fs$nest_id)]

#write.csv(nn, "/Users/Silvia/SnowyPlover/Prepare_Data.csv")