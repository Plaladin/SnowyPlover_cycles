# Scrape tide data

library(rvest)

#2006-2007 (2008 must be loaded seperately)
tides_06_07 <- lapply(paste0("http://tides.mobilegeographics.com/calendar/year/3689.html?y=",2006:2007, "&m=4&d=1"),
                      function(url){
                        url %>% read_html() %>% 
                          html_nodes("table") %>% 
                          html_table()
                      })    
tides_06_07_list <- do.call(rbind, tides_06_07) 
tides_06_07_df <- do.call(rbind, tides_06_07_list)

# 2008
tides_08 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2008&m=4&d=1")
tides_08_tbls <- html_nodes(tides_08, "table")
tides_08_list <- html_table(tides_08_tbls) # list of the tables
tides_08_df <- ldply(tides_08_list, data.frame) # convert this into a big dataframe
tides_08_df$Low.2 <- NULL
tides_08_df$High.3 <- NULL # these two extra coloums were the reason I had to scrape 08 seperatly
colnames(tides_08_df) <- colnames(tides_06_07_df) # some of the colnames didn't match (needed for rbind)

# 2009-2016
tides_09_16 <- lapply(paste0("http://tides.mobilegeographics.com/calendar/year/3689.html?y=",2009:2016, "&m=4&d=1"),
                      function(url){
                        url %>% read_html() %>% 
                          html_nodes("table") %>% 
                          html_table()
                      }) 
tides_09_16_list <- do.call(rbind, tides_09_16)
tides_09_16_df <- do.call(rbind, tides_09_16_list)

# 2006-2016 merged together

tides_all <- rbind( tides_06_07_df, tides_08_df, tides_09_16_df)
tides_all$date <- seq(as.Date("2006/1/1"), as.Date("2016/12/31"), "days")

tides_all$date <- format(tides_all$date, "%Y-%d-%m")
tides_all$date <- as.Date(tides_all$date, "%Y-%d-%m" )

tides_all$year <- format(tides_all$date, "%Y")

tides_all <- subset(tides_all, select = -c(Day,Sunrise,Sunset) )
tides_all <- tides_all[,c(8,7,6,1,2,3,4,5)]
colnames(tides_all) <- c("year","date","event","high_1","low_1","high_2","low_2","high_3")
tides_all$high_1 <- substring(tides_all$high_1,15,19)
tides_all$low_1 <- substring(tides_all$low_1,15,19)
tides_all$high_2 <- substring(tides_all$high_2,15,19)
tides_all$low_2 <- substring(tides_all$low_2,15,19)
tides_all$high_3 <- substring(tides_all$high_3,15,19)
tides_all$high_1 <- as.numeric(tides_all$high_1)*100
tides_all$high_2 <- as.numeric(tides_all$high_2)*100
tides_all$high_3 <- as.numeric(tides_all$high_3)*100
tides_all$low_1 <- as.numeric(tides_all$low_1)*100
tides_all$low_2 <- as.numeric(tides_all$low_2)*100

tides_all$event <- gsub('First Quarter', 'fq', tides_all$event)
tides_all$event <- gsub('Full Moon', 'fm', tides_all$event)
tides_all$event <- gsub('Last Quarter', 'lq', tides_all$event)
tides_all$event <- gsub('New Moon', 'nm', tides_all$event)

# I want to have the highest high tides on a given day
ht <- tides_all[,c(1,2,4,6,8)]
ht$m= pmax(ht$high_1,ht$high_2,ht$high_3, na.rm=TRUE) #compare those two and give me the max
ht$m = ht$m * 100
# add a coloum with the max tide height to tides_all 
tides_all$max_tide_height <- ht$m


## Scrape the lunar data
# 2006-2016

moon_06 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2006"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_06_list <- do.call(rbind, moon_06) 
moon_06_df <- do.call(rbind, moon_06_list)
moon_06_df <- moon_06_df[,c(1,8,11)]
colnames(moon_06_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_06_df=moon_06_df[moon_06_df$date %in% dig,] 
moon_06_df$date <- seq(as.Date("2006/1/1"), as.Date("2006/12/31"), "days")


moon_07 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2007"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_07_list <- do.call(rbind, moon_07) 
moon_07_df <- do.call(rbind, moon_07_list)
moon_07_df <- moon_07_df[,c(1,8,11)]
colnames(moon_07_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_07_df=moon_07_df[moon_07_df$date %in% dig,] # works, nice!
moon_07_df$date <- seq(as.Date("2007/1/1"), as.Date("2007/12/31"), "days")


moon_08 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2008"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_08_list <- do.call(rbind, moon_08) 
moon_08_df <- do.call(rbind, moon_08_list)
moon_08_df <- moon_08_df[,c(1,8,11)]
colnames(moon_08_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_08_df=moon_08_df[moon_08_df$date %in% dig,] # works, nice!
moon_08_df$date <- seq(as.Date("2008/1/1"), as.Date("2008/12/31"), "days")


moon_09 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2009"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_09_list <- do.call(rbind, moon_09) 
moon_09_df <- do.call(rbind, moon_09_list)
moon_09_df <- moon_09_df[,c(1,8,11)]
colnames(moon_09_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_09_df=moon_09_df[moon_09_df$date %in% dig,] # works, nice!
moon_09_df$date <- seq(as.Date("2009/1/1"), as.Date("2009/12/31"), "days")
View(moon_09_df)


moon_10 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2010"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_10_list <- do.call(rbind, moon_10) 
moon_10_df <- do.call(rbind, moon_10_list)
moon_10_df <- moon_10_df[,c(1,8,11)]
colnames(moon_10_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_10_df=moon_10_df[moon_10_df$date %in% dig,] # works, nice!
moon_10_df$date <- seq(as.Date("2010/1/1"), as.Date("2010/12/31"), "days")
View(moon_10_df)


moon_11 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2011"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_11_list <- do.call(rbind, moon_11) 
moon_11_df <- do.call(rbind, moon_11_list)
moon_11_df <- moon_11_df[,c(1,8,11)]
colnames(moon_11_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_11_df=moon_11_df[moon_11_df$date %in% dig,] # works, nice!
moon_11_df$date <- seq(as.Date("2011/1/1"), as.Date("2011/12/31"), "days")
View(moon_11_df)


moon_12 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2012"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_12_list <- do.call(rbind, moon_12) 
moon_12_df <- do.call(rbind, moon_12_list)
moon_12_df <- moon_12_df[,c(1,8,11)]
colnames(moon_12_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_12_df=moon_12_df[moon_12_df$date %in% dig,] # works, nice!
moon_12_df$date <- seq(as.Date("2012/1/1"), as.Date("2012/12/31"), "days")
View(moon_12_df)



moon_13 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2013"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_13_list <- do.call(rbind, moon_13) 
moon_13_df <- do.call(rbind, moon_13_list)
moon_13_df <- moon_13_df[,c(1,8,11)]
colnames(moon_13_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_13_df=moon_13_df[moon_13_df$date %in% dig,] # works, nice!
moon_13_df$date <- seq(as.Date("2013/1/1"), as.Date("2013/12/31"), "days")
View(moon_13_df)


moon_14 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2014"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_14_list <- do.call(rbind, moon_14) 
moon_14_df <- do.call(rbind, moon_14_list)
moon_14_df <- moon_14_df[,c(1,8,11)]
colnames(moon_14_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_14_df=moon_14_df[moon_14_df$date %in% dig,] # works, nice!
moon_14_df$date <- seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days")
View(moon_14_df)


moon_15 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2015"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_15_list <- do.call(rbind, moon_15) 
moon_15_df <- do.call(rbind, moon_15_list)
moon_15_df <- moon_15_df[,c(1,8,11)]
colnames(moon_15_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_15_df=moon_15_df[moon_15_df$date %in% dig,] # works, nice!
moon_15_df$date <- seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days")
View(moon_15_df)


moon_16 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2016"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_16_list <- do.call(rbind, moon_16) 
moon_16_df <- do.call(rbind, moon_16_list)
moon_16_df <- moon_16_df[,c(1,8,11)]
colnames(moon_16_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_16_df=moon_16_df[moon_16_df$date %in% dig,] # works, nice!
moon_16_df$date <- seq(as.Date("2016/1/1"), as.Date("2016/12/31"), "days")
View(moon_16_df)

#2006-2016 merged

moon_all <- rbind (moon_06_df, moon_07_df, moon_08_df , moon_09_df, moon_10_df, moon_11_df, moon_12_df, moon_13_df,  moon_14_df, moon_15_df, moon_16_df)

moon_all$time <- gsub("Moon does not pass the meridian on this day.","NA",moon_all$time )
moon_all$illumination <- gsub("Moon does not pass the meridian on this day.","NA",moon_all$illumination )

## Interpolation for illumination at 12 pm
Sys.setenv(TZ="UTC")
moon_all$datetime<- as.POSIXct(paste(moon_all$date, moon_all$time), format="%Y-%m-%d %H:%M")
moon_all$illumination <- gsub("%","",moon_all$illumination)
moon_all$illumination <- gsub(",",".",moon_all$illumination)
moon_all$illumination <- as.numeric(moon_all$illumination)/100

f <- approxfun(moon_all$datetime,moon_all$illumination)

start <- as.POSIXct("2006-1-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")
end   <- as.POSIXct("2016-12-31 12:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")

x <- seq(start, end, "days")
moon_all$noon <- x
moon_all$interpolated <- f(x)
moon_all$year <- format(moon_all$date, "%Y")
moon_all <- moon_all[,c(7,4,3,5,6)]
colnames(moon_all) <- c("year","meridian_passing","illumination_mp","noon","illumination_noon")






