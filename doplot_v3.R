# ==================
# Clear workspace
rm(list = ls()) 
graphics.off()

# ==================
# Set WD as needed by your machine
base_dir<-getwd()
setwd(base_dir)

# ==================
# function for installing & loading packages (installs only if not already installed)
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# ==================
# packages to install & load
packagesCRAN<-c("ggplot2","plyr","dplyr","devtools","reshape","tidyr","ggthemes","RColorBrewer","scales",
                "tidyverse","lubridate","viridis","png")

# iterate usePackage function across package names
lapply(packagesCRAN,FUN=usePackage)

# additional finicky packages, install and load separately
#install_github("dgrtwo/gganimate", force=TRUE)
#install_github("thomasp85/gganimate/releases/tag/v0.1.1", force=TRUE)
library("gganimate")

# ==================
# read the daterr
ice<-read.csv("icedata.csv")

# ==================
# prepare the daterr
names(ice)[4]<-"ice_duration" # rename this column
ice<-ice[-c(1:3),] # drop the first few years of record (data gaps)
ice$year<-as.numeric(substr(ice$WINTER,1,4))+1 # create 'year' column (1996 for northern winter of 1995-1996)
closed<-ice %>% select(year,ice_duration,date=CLOSED0) %>% as.data.frame() # slimmer data frame of closed events
closed$event<-"closed"
opened<-ice %>% select(year,ice_duration,date=OPENED0) %>% as.data.frame() # slimmer data frame of opened events
opened$event<-"opened"
ice2<-rbind(closed,opened) # slim data frame of closed + opened events

# create column for day of year (doy= days since Jan 1st)
ice2$doy<-0
monthstr<-substr(ice2$date,nchar(as.character(ice2$date))-2,nchar(as.character(ice2$date)))
dayofmo<-as.numeric(substr(ice2$date,1,nchar(as.character(ice2$date))-4))
ice2$doy[which(monthstr=="Nov")]<-dayofmo[which(monthstr=="Nov")]-30-31
ice2$doy[which(monthstr=="Dec")]<-dayofmo[which(monthstr=="Dec")]-30
ice2$doy[which(monthstr=="Jan")]<-dayofmo[which(monthstr=="Jan")]
ice2$doy[which(monthstr=="Feb")]<-dayofmo[which(monthstr=="Feb")]+31
ice2$doy[which(monthstr=="Mar")]<-dayofmo[which(monthstr=="Mar")]+31+28
ice2$doy[which(monthstr=="Apr")]<-dayofmo[which(monthstr=="Apr")]+31+28+31
ice2$doy[which(monthstr=="May")]<-dayofmo[which(monthstr=="May")]+31+28+31+30

ice2$x<-ice2$year
ice2$y<-ice2$doy
ice2$z<-ice2$year
ice2$txt<-ice2$year
ice2$txt2<-ice2$ice_duration
ice2<-unique(ice2)
ice2<-ice2[order(ice2$z),]
ice2$txt<-as.character(ice2$txt)
ice2<-ice2[order(ice2$txt,ice2$z),]
ice2$colour<-as.numeric(ice2$txt2)

# ==================
# workaround to set up the 'pause effect' after last plotted point
# because use of end_pause argument (gganimate) produced this error... Error: Additional frame variables must have the same length as the number of frames

plotdata<- ice2
pause.year<-2018
pausedata<-subset(plotdata,plotdata$year>=pause.year) # datapoints to pause on
pause.n<-100 # length of the pause (# of frames)
for (i in 1:pause.n){
  print(i)
  namei<-paste("pausedata",pause.n,sep="")
  xi<-pause.year+(i*.05/pause.n)
  pausedata.i<-pausedata
  pausedata.i$x<-xi
  if(i==1){pausedataX<-pausedata.i}
  if(i>1){pausedataX<-rbind(pausedataX,pausedata.i)}  
}

plotdata.pause<-rbind(plotdata,pausedataX) # combined data frame of plotdata + pausedata

# workaround to plot more points with warmer colors
# because 'viridis' package appears to have limited scaling options (?)
min.icedays<-60
plotdata.pause$ice_duration2<-plotdata.pause$ice_duration
plotdata.pause$ice_duration2[which(plotdata.pause$ice_duration2<=min.icedays)]<-min.icedays

# ==================
# do plots

# do plot version 1- original ice duration (best for 'magma' color scheme ?)
p1 <- ggplot(plotdata.pause, aes(x, y,frame=x,shape=event,color=ice_duration)) +
  geom_point(size=1.5)+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_bw()#

# do plot version 2- plots more points with warmer colors (best for 'viridis' color scheme?)
p2 <- ggplot(plotdata.pause, aes(x, y,frame=x,shape=event,color=ice_duration2)) +
  geom_point(size=1.5)+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_bw()#

p.magma<-p1 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,end=0.8,option="A")
p.viridis<-p2 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,option="D")

anim_save("ice.magma.gif", p.magma,width=400,height=250)
anim_save("ice.viridis.gif", p.viridis,width=400,height=250)

