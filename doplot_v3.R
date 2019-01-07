# ==========
# = Clear workspace =
rm(list = ls()) 
graphics.off()

# ==========
# = Set WD as needed by your machine!! 
base_dir<-getwd()
setwd(base_dir)

# ==================
# = Load Libraries
#function for installing packages first time only
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#packages needed
packagesCRAN<-c("plyr","dplyr","devtools","reshape","tidyr","ggthemes","RColorBrewer","scales",
                "tidyverse","lubridate","viridis","png")

#packagesCRAN<-c("plyr","dplyr","devtools","reshape","tidyr","ggthemes","RColorBrewer","scales",
#                "tidyverse","forcats","lubridate","viridis","gifski","png")

#iterate usePackage function across package names
lapply(packagesCRAN,FUN=usePackage)

#additional finicky packages, handle separately

library("ggplot2")

#install_github("dgrtwo/gganimate", force=TRUE)
#install_github("thomasp85/gganimate/releases/tag/v0.1.1", force=TRUE)
library("gganimate")

ice<-read.csv("data.csv")
names(ice)[4]<-"ice_duration"
ice<-ice[-c(1:3),]
ice$year<-as.numeric(substr(ice$WINTER,1,4))+1

doy<-function(data){
  if(data$closed_mo==11){
    closed_doy<-data$closed_day
  }
  if(data$closed_mo==12){
    closed_doy<-data$closed_day+30
  }
  if(data$closed_mo==1){
    closed_doy<-data$closed_day+30+31
  }
  if(data$closed_mo==2){
    closed_doy<-data$closed_day+30+31+31
  }
  if(data$closed_mo==3){
    closed_doy<-data$closed_day+30+31+31+28
  }
  if(data$closed_mo==4){
    closed_doy<-data$closed_day+30+31+31+28+31
  }
  
  if(data$opened_mo==11){
    opened_doy<-data$opened_day
  }
  if(data$opened_mo==12){
    opened_doy<-data$opened_day+30
  }
  if(data$opened_mo==1){
    opened_doy<-data$opened_day+30+31
  }
  if(data$opened_mo==2){
    opened_doy<-data$opened_day+30+31+31
  }
  if(data$opened_mo==3){
    opened_doy<-data$opened_day+30+31+31+28
  }
  if(data$opened_mo==4){
    opened_doy<-data$opened_day+30+31+31+28+31
  }
  if(data$opened_mo==5){
    opened_doy<-data$opened_day+30+31+31+28+31+30
  }  
  df<-data.frame(closed_doy,opened_doy)
  return(df)
}


closed<-ice %>% select(year,ice_duration,date=CLOSED0) %>% as.data.frame()
closed$event<-"closed"
opened<-ice %>% select(year,ice_duration,date=OPENED0) %>% as.data.frame()
opened$event<-"opened"
ice2<-rbind(closed,opened)
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

names(ice2)

##############################
datax<-ice2
datax$z<-datax$year
datax$x<-datax$year
#datax$y<-datax$DAYS
datax$y<-datax$doy
datax$txt<-datax$year
datax$txt2<-datax$ice_duration

for(i in 1:length(unique(datax$z))) {
  yeari<-unique(datax$z)[i]
  namei<-paste("datax.",yeari,sep="")
  data.i<-subset(datax,datax$z==yeari)
  assign(namei,data.i)
}

things<-ls()[grep("datax.*",ls())]

for(i in 1:length(things)){
  datai<-get(things[i])
  if(i==1){data.out<-datai}
  if(i>1){data.out<-rbind(data.out,datai)}
}
rbind<-data.out

rbind<-unique(rbind)
rbind<-rbind[order(rbind$z),]
rbind.end<-rbind.end[order(rbind.end$z,rbind.end$z,decreasing=TRUE),]

n.approx<-100

df<-rbind
plotdata<-df
plotdata$txt<-as.character(plotdata$txt)

plotdata<-plotdata[order(plotdata$txt,plotdata$z),]
#plotdata$colour[which(plotdata$x==0)]<-""
plotdata$colour<-as.numeric(plotdata$txt2)


# workaround to create the 'pause effect' after last plotted point
# use of end_pause argument (gganimate) produced this error... Error: Additional frame variables must have the same length as the number of frames

plotdata0<-plotdata
pausedata<-subset(plotdata,plotdata$year>=2018)
rep.pausedata <- function(data=pausedata,n=50,year=2018) 
{
  for (i in 1:n){
    namei<-paste("pausedata",n,sep="")
    xi<-year+(i*.05/n)
    pausedata.i<-data
    pausedata.i$x<-xi
    assign(namei,pausedata.i)
  }
}
lapply(pausedata,FUN=rep.pausedata)


rep.pausedata(pausedata)

pause.n<-50
pause.year<-2018
for (i in 1:pause.n){
  print(i)
  namei<-paste("pausedata",pause.n,sep="")
  xi<-pause.year+(i*.05/pause.n)
  pausedata.i<-pausedata
  pausedata.i$x<-xi
  if(i==1){pausedataX<-pausedata.i}
  if(i>1){pausedataX<-rbind(pausedataX,pausedata.i)}  
#  assign(pausedata.i,namei)  
}

plotdataB<-rbind(plotdata0,pausedataX)

#p.magma <- ggplot(plotdataB, aes(x, y,frame=x,shape=event,color=ice_duration)) +
#  geom_point(size=1.5)+
#  scale_color_viridis(direction=-1,end=0.8,option="A")+
#  ylim(-60,150)+
#  xlab("Year")+ylab("Days since January 1st")+
#  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
#  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
#  theme_bw()#

#p.cume.magma<-p.magma +  transition_manual(x,cumulative=TRUE)

plotdataB$ice_duration2<-plotdataB$ice_duration
plotdataB$ice_duration2[which(plotdataB$ice_duration2<=60)]<-60

p1 <- ggplot(plotdataB, aes(x, y,frame=x,shape=event,color=ice_duration)) +
  geom_point(size=1.5)+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_bw()#

p2 <- ggplot(plotdataB, aes(x, y,frame=x,shape=event,color=ice_duration2)) +
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

