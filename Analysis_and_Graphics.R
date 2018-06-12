# Testing for semilunar rhythms in nesting schedules of Snowy Plovers

# LOAD & PREPARE DATA
wd <- "/Users/Silvia/Dropbox/SnowyPlover_cycles (1)/" # define your working directory
source(paste(wd, 'Constants_Functions.R',sep="")) # save these two files before in your working directory
source(paste(wd, 'Prepare_Data.R',sep=""))

## load all packages
x<-c('waffle','arm','effects', 'data.table','ggplot2','grid', 'lattice','magrittr','multcomp','plyr','raster','RColorBrewer','sqldf','stringr','XLConnect', 'lubridate')
# install.packages(x)
lapply(x, require,character.only = TRUE)



#     Graphics 
#########################

# plot nest frequencies per year with tideheight and illumination
# 
NP <- c((seq(as.Date("2006-04-01"), as.Date("2006-07-31"), by="days")), 
        seq(as.Date("2007-04-01"), as.Date("2007-07-31"), by="days"), 
        seq(as.Date("2008-04-01"), as.Date("2008-07-31"), by="days"), 
        seq(as.Date("2009-04-01"), as.Date("2009-07-31"), by="days"), 
        seq(as.Date("2010-04-01"), as.Date("2010-07-31"), by="days"), 
        seq(as.Date("2011-04-01"), as.Date("2011-07-31"), by="days"), 
        seq(as.Date("2012-04-01"), as.Date("2012-07-31"), by="days"),
        seq(as.Date("2013-04-01"), as.Date("2013-07-31"), by="days"),
        seq(as.Date("2015-04-01"), as.Date("2015-07-31"), by="days"),
        seq(as.Date("2016-04-01"), as.Date("2016-07-31"), by="days"))

np <- as.data.frame(NP) # nesting period
colnames(np) <- c("date")
np["nests_laid"] <- rep(0, length(np$date)) # new col were the number of layed nests are supposed to go
np$date <- as.POSIXct(np$date)
np$n_nest <- (dd$n_nest[match(np$date,dd$datetime_)])
np[is.na(np)] <- 0
np$flooded_ = NA
# get illumination
np$illum <- ii$illumination_noon[match(as.character(np$date), substring(ii$noon,1,10))]
np$illum <- round(np$illum,2)

np$yday <- yday(np$date)
np$year <- as.numeric(format(as.Date(np$date, format="%d/%m/%Y"),"%Y"))

# max tide height
np$max_tide_height = tt$max_tide_height[match(np$date, tt$date)]

d$yday = yday(d$laid)
d = d[order(d$laid),]

# get the median for the laying date
median_l <- tapply(d$yday, as.factor(d$year),median) 
median_l <- as.data.frame.table(median_l)
colnames(median_l) <- c("year","median") 
median_l$year <- as.integer(as.character(median_l$year)) # needs to be the same class as base data

geom.text.size = 4
#theme.size = (14/5) * geom.text.size
theme.size = 12


p = ggplot(d, aes(x= yday , fill=as.factor(flooded_))) + 
  geom_histogram(bins = 105) + 
  facet_grid(year ~.) +
  geom_text(aes(x, y,label=lab),
            data=data.frame(x=190, y=13, flooded_ = c("yes","no"), lab=c("2006 (N=159)", "2007 (N=124)", "2008 (N=74)", "2009 (N=72)", "2010 (N=91)", "2011 (N=61)", "2012 (N=33)", "2013 (N=28)", "2015 (N=38)", "2016 (N=43)"),
                            year=c(2006,2007,2008,2009,2010,2011,2012,2013,2015,2016)), size=geom.text.size)+
  scale_x_continuous(breaks= c(91,121,152,182), labels=c("April", "May", "June", "July")) + 
  labs(x = "Nest initiation", y = "Number of initiated nests",fill = 'Flooded')+  
  scale_fill_manual(values=c("darkgrey", "blue"))+
  coord_cartesian( xlim = c(90,195))+
  theme(panel.border=element_rect(colour="black",fill=NA, size=0.25),text = element_text(size = theme.size),
        strip.background = element_blank(), strip.text.y = element_blank(),
        axis.line=element_line(colour="black", size=0.25),
        panel.grid = element_blank(),
        panel.background = element_blank()
        
  )
p = p + geom_line(data = np[!is.na(np$max_tide_height),], aes(x = yday, y = max_tide_height / 15), size =0.5,color='steelblue') # tide height
p = p + geom_vline(aes(xintercept = median), median_l, size = 0.2) # median of laying dates
p = p + scale_y_continuous(breaks=c(0,5,10),labels=c("0","5","10"), name= "Number of initiated nests",
                           sec.axis = sec_axis(~ . * 15, name = "Tide height [m]", breaks = c(0,50,100,150)))





# Barplot, Nest fates per year

n_ = n[!is.na(nn$fate),]
table(n_$fate)
n_$fate[n_$fate == "broken"] <- "other"
n_$fate[n_$fate == "irrigation"] <- "other"
n_$fate[n_$fate == "shrimp farm?"] <- "other"
n_$fate[n_$fate == "trampled"] <- "other"

t <- as.data.frame(table(n_$fate))
t$Var1 <- as.character(t$Var1)
t$percent<- 100* prop.table(t$Freq) # get the percentages for legend
# combine percentage with bar plot
cols<- c('#8dd3c7','#fb8072','#bebada','#ffffb3','royalblue','cyan','sandybrown','darkgrey')
n_$fate <- as.factor(n_$fate)
table(n_$fate)

n_$fate <- factor(n_$fate,levels= rev(c("hatch","pred","unknown", "aban","tide","rain","unhatch","other"))) # reorder levels according to the frequencies I calculated
n_$year <- as.factor(n_$year)


# 700:500 scale for saving
p <- ggplot(n_, aes(x=year, fill=as.factor(fate)))+ geom_bar()+ labs(x = "Year", y = "Number of initiated nests")
p <- p + scale_fill_manual(values=rev(cols), labels = rev(c("hatched (53.7%)","predated (17.6%)","unknown (8.6%)", "abandoned (7.6%)","tide-flooded (5.7%)","rain-flooded (3.2%)","unhatched (2.8%)","other (1%)")))
p <- p + scale_y_continuous(breaks= seq(0,160,10), expand = c(0, 0)) + expand_limits(y=165)
p$labels$fill <- "Nest fates"
p + theme(axis.line = element_line(colour = "black", size = 0.1),
          panel.grid.major = element_line(colour = "grey30", size = 0.1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),panel.background = element_blank())



#     Models (GLM, LMM, GLMM) 
##################################

# GLM, population trend
# get the sum of nests per year
#su <- tapply(np$n_nest, as.factor(np$year),sum)
su <- tapply(dd$n_nest, as.factor(dd$year),sum)
su <- as.data.frame.table(su)
colnames(su) <- c("year","sum_nests") 
# nests are declinig over years:
m <- glm(formula= sum_nests ~ year, data=su, family=poisson)
summary(m) 


###
# tideheight in lunar cycle (LMM)
# predict tideheight as a function of day in the lunar month, controling for cycle and year variability

# prepare
#tt$rad_st= 2*tt$days_after_st*pi/14.765
tt$rad_st= 2*tt$days_after_st*pi/tt$dur # use actual duration of spring tide cycle

m = lmer(max_tide_height ~ cos(rad_st) + sin(rad_st) +
           ((cos(rad_st) + sin(rad_st))|year/st_cycle),data=tt, REML= FALSE)	

summary(glht(m))
#plot(allEffects(m))

#Credible intervals and medians from posterior sampling
sims <- sim(m,n.sims=5000)
# randomly draw posterior samples of the coefficients based on the fitted models. 
# We can draw as many sample as we want and based on Bayesian theory the 0.025 and 0.975 quantiles of the sampled values will form the 95% credible intervals around the fitted value (the one we got from the model output).
crI<-apply(sims@fixef,2,function(x) quantile(x,probs=c(0.025,0.5,0.975)))
# CI for random effects
#simulated uncertainty for random intercepts effects (year)
ranef.sim <- ranef(sims)$year
CIrandom <- apply(ranef.sim, 2, function(x) quantile(x,probs=c(.025, .975)))




### FLOODING
# Does the risk of flooding differ across the tidal cycle? 

# prepare	
d = nn[!is.na(nn$fate),]
d$fate[d$fate=='tide'] = 'flood'
d$flooded = ifelse(d$fate=='flood',1,0)
d$rad_st = 2*d$days_after_st*pi/d$dur

# GLMM
m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|female), family = 'binomial', d)
nsim <- 5000
bsim <- sim(m, n.sim=nsim)  

v = apply(bsim@fixef, 2, quantile, prob=c(0.5)) # medians
ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975)) # CIs
# values to predict for		
newD=data.frame(days_after_st = seq(0,max(d$days_after_st), length.out=300))
newD$rad_st =2*newD$days_after_st*pi/14.75

# exactly the model which was used has to be specified here 
X <- model.matrix(~ sin(rad_st)+cos(rad_st),data=newD)	

# calculate predicted values and creditability intervals
newD$pred <-plogis(X%*%v) 
predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
pp=newD	

# raw data
x = ddply(d,.(year, days_after_st), summarise, mean_ = mean(flooded), sd_ = sd(flooded), n = length(year))
y = data.frame(year = unique(d$year), col_ =1:length(unique(d$year)), stringsAsFactors = FALSE)
x$col_ = y$col_[match(x$year, y$year)]
d$days_after_r = round(d$days_after_st)
x2 = ddply(d,.(days_after_r), summarise, mean_ = mean(flooded), sd_ = sd(flooded), n = length(year))	

# effect plot 
par(mar=c(1.3,3,0,3),oma = c(3, 2, 0.5, 1.5),ps=12, mgp=c(1.5,1,0), las=1, cex=0.9, col.axis="grey30",font.main = 1, 
    col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=1.2,cex.main=1, cex.axis=1, tcl=-0.1,bty="n",xpd=TRUE)

plot(pp$pred~pp$days_after_st, 
     xlim=c(0,15), ylim=c(0,0.25),
     xaxt='n', yaxt= 'n',
     xaxs = 'i', yaxs = 'i',
     ylab = "",xlab = NULL,type='n')

axis(1, at=seq(0,15, by = 3), label=TRUE, tck=-0.013, mgp=c(0,0.5,0))
axis(2, at= c(0,0.05,0.10,0.15,0.20,0.25),labels=c(0,0.05,0.10,0.15,0.20,0.25), col.axis="grey30", las=2,tck=-0.011, mgp=c(0,0.5,0))
mtext("Days from spring tide",side=1,line=2, cex=1, las=1, col='grey30')
mtext("Probability of getting flooded",side=2,line=2.8, cex=1, las=3, col='grey30')
#mtext("Days from spring tide",side=1,line=2.6, cex=1.2, las=1, col='grey30')
#mtext("Number of initiated nests",side=2,line=2.8, cex=1.2, las=3, col='grey30')

polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
                                                      rev(pp$upr)), border=NA, col="lightgrey") #0,0,0 black 0.5 is transparents RED
lines(pp$days_after_st, pp$pred, col="black",lwd=1)
symbols( jitter(x2$days_after_r),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 

# legend
text(14.2 ,0.2355, expression(italic('N')*' nests:'),xpd=NA, cex=1,col='grey30') #side = 4,line=-1, padj=-65,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
symbols(c(16,16),c(0.245,0.226),circles=sqrt(c(10,60)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
text(c(16,16),c(0.24,0.217),labels=c(10,60), xpd=NA, cex=0.9,col='grey30')													

# Effect plot with predicted values ggplot
ggplot(pp, aes(x=days_after_st, y=pred)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha= 0.3), fill= "lightgrey") +
  geom_line(size= 0.7, color= "black") +
  labs(x = "Days after Spring Tide", y = "Flooded nests [%]") +
  scale_x_continuous(breaks= seq(0,15,3)) +
  theme (panel.border=element_rect(colour="black",fill=NA, size=0.25),text = element_text(size = 14),axis.text=element_text(size=12),
         strip.background = element_blank(), strip.text.y = element_blank(),
         axis.line=element_line(colour="black", size=0.25),
         panel.grid = element_blank(),
         panel.background = element_blank(),
         panel.spacing = unit(0, "mm"),
         legend.position="none")



# NEST INITIATION
# Does the nest initiation follow lunar-tide cycle?

#prepare
dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
dd$l_n = 1:nrow(dd)

# GLMM
m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year/st_cycle/first_second)+ (1|l_n), family = 'poisson',dd)
summary(glht(m))

# simulation
nsim <- 5000
bsim <- sim(m, n.sim=nsim)  

v = apply(bsim@fixef, 2, quantile, prob=c(0.5)) # medians
ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975)) # CIs
# values to predict for		
newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300))
newD$rad_st =2*newD$days_after_st*pi/14.75
# exactly the model which was used has to be specified here 
X <- model.matrix(~ sin(rad_st) + cos(rad_st),data=newD)	

# calculate predicted values and creditability intervals
newD$pred <-exp(X%*%v)
predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
pp=newD	

# raw data
dd$days_after_r = round(dd$days_after_st)
x2 = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))	

# effect plot 
# aspect ratio 520:530
par(mar=c(1.3,3,0,3),oma = c(3, 2, 0.5, 1.5),ps=12, mgp=c(1.5,1,0), las=1, cex=0.9, col.axis="grey30",font.main = 1, 
    col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=1.2,cex.main=1, cex.axis=1, tcl=-0.1,bty="n",xpd=TRUE)

plot(pp$pred~pp$days_after_st, 
     xlim=c(0,16), ylim=c(0, 1.4),
     xaxt='n', yaxt='n',
     xaxs = 'i', yaxs = 'i',
     ylab = "",xlab = NULL,type='n')


#axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
axis(1, at=seq(0,15, by = 3), label=TRUE,tck=-0.013, mgp=c(0,0.5,0))
axis(2, at= c(0,0.2,0.4,0.6,0.8,1,1.2,1.4),labels=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4), col.axis="grey30",tck=-0.011, las=2)
#mtext("Days after spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
mtext("Days from spring tide",side=1,line=2, cex=1.2, las=1, col='grey30')
mtext("Number of initiated nests",side=2,line=2.8, cex=1.2, las=3, col='grey30')

polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
                                                      rev(pp$upr)), border=NA, col="lightgrey") #0,0,0 black 0.5 is transparents RED
lines(pp$days_after_st, pp$pred, col="black",lwd=1)
symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 

text(14.2 ,1.3, expression(italic('N')*' nests:'),xpd=NA, cex=1,col='grey30') #side = 4,line=-1, padj=-65,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
symbols(c(16,16),c(1.35,1.25),circles=sqrt(c(10,60)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
#symbols(c(17,17,17)+0.01,c(0.26,0.26-0.04,0.26-0.04*2),circles=sqrt(c(1,30,75)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE)
text(c(16,16),c(1.32,1.20),labels=c(10,60), xpd=NA, cex=1,col='grey30')


# Effect plot with predicted values ggplot
df= rbind(pp, x2)

ggplot(pp, aes(x=days_after_st, y=pred)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha= 0.3), fill= "lightgrey") +
  geom_line(size= 0.7, color= "black") +
  #geom_jitter(aes(x= x2$days_after_st, y= x2$n), width= x2$mean_) +
  labs(x = "Days after Spring Tide", y = "Clutches completed [%]") +
  scale_x_continuous(breaks= seq(0,15,3)) +
  theme (panel.border=element_rect(colour="black",fill=NA, size=0.25),text = element_text(size = 14),axis.text=element_text(size=12),
         strip.background = element_blank(), strip.text.y = element_blank(),
         axis.line=element_line(colour="black", size=0.25),
         panel.grid = element_blank(),
         panel.background = element_blank(),
         panel.spacing = unit(0, "mm"),
         legend.position="none")






# when were nests flooded by tide 
# ggplot(aes(y = yday(laid), x = as.factor(year)), data= n[n$fate=="tide",]) + geom_point() + geom_jitter()

l <- n[n$fate == "tide",]
l <- na.omit(l)
l$ylaid <- yday(l$laid)
l$yend <- yday(l$end)

ggplot(aes(y = ylaid, x = as.factor(year)), data= l) + geom_point() + geom_jitter()
ggplot(aes(y = yend, x = as.factor(year)), data= l) + geom_point() + geom_jitter()

sum(l$ylaid[l$year == 2012] < 160) # 13
sum(l$ylaid > 160) # 20
# if considering all years <50 % were flooded, if initiated after the 10th of june.
# if taking 2012 out, 41-13, >70 % were flooded, if initiated after the 10th of june
# considering an incubation of 30 days and the start of rainy season in mid-june, lets cut a line in mid-may (~135)
sum(l$ylaid[l$year == 2012] < 135) # 13
sum(l$ylaid <= 134)


# when were nests flooded by rain 
# ggplot(aes(y = yday(laid), x = as.factor(year)), data= n[n$fate=="tide",]) + geom_point() + geom_jitter()

l <- n[n$fate == "rain",]
l <- na.omit(l)
l$ylaid <- yday(l$laid)
l$yend <- yday(l$end)

ggplot(aes(y = ylaid, x = as.factor(year)), data= l) + geom_point() + geom_jitter()
ggplot(aes(y = yend, x = as.factor(year)), data= l) + geom_point() + geom_jitter()

sum(l$ylaid < 160) # 7
sum(l$ylaid > 160) # 15
# if considering all years >65 % were flooded by rain, if initiated after the mid-june.
sum(l$ylaid < 134) # 4
sum(l$ylaid > 134) # 19
# if considering all years >82 % were flooded by rain, if initiated after the mid-may, one month before the rainy season.





