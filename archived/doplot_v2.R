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

#plotdata0<-subset(plotdata,plotdata$year>=2000)
plotdata0<-plotdata
plotdata1<-subset(plotdata,plotdata$year>=2018)
plotdata2<-subset(plotdata,plotdata$year>=2018)
plotdata3<-subset(plotdata,plotdata$year>=2018)
plotdata4<-subset(plotdata,plotdata$year>=2018)
plotdata5<-subset(plotdata,plotdata$year>=2018)
plotdata6<-subset(plotdata,plotdata$year>=2018)
plotdata7<-subset(plotdata,plotdata$year>=2018)
plotdata8<-subset(plotdata,plotdata$year>=2018)
plotdata9<-subset(plotdata,plotdata$year>=2018)
plotdata10<-subset(plotdata,plotdata$year>=2018)
plotdata11<-subset(plotdata,plotdata$year>=2018)
plotdata12<-subset(plotdata,plotdata$year>=2018)
plotdata13<-subset(plotdata,plotdata$year>=2018)
plotdata14<-subset(plotdata,plotdata$year>=2018)
plotdata15<-subset(plotdata,plotdata$year>=2018)
plotdata16<-subset(plotdata,plotdata$year>=2018)
plotdata17<-subset(plotdata,plotdata$year>=2018)
plotdata18<-subset(plotdata,plotdata$year>=2018)
plotdata19<-subset(plotdata,plotdata$year>=2018)
plotdata20<-subset(plotdata,plotdata$year>=2018)
plotdata21<-subset(plotdata,plotdata$year>=2018)
plotdata22<-subset(plotdata,plotdata$year>=2018)
plotdata23<-subset(plotdata,plotdata$year>=2018)
plotdata24<-subset(plotdata,plotdata$year>=2018)
plotdata25<-subset(plotdata,plotdata$year>=2018)
plotdata26<-subset(plotdata,plotdata$year>=2018)
plotdata27<-subset(plotdata,plotdata$year>=2018)
plotdata28<-subset(plotdata,plotdata$year>=2018)
plotdata29<-subset(plotdata,plotdata$year>=2018)
plotdata30<-subset(plotdata,plotdata$year>=2018)
plotdata31<-subset(plotdata,plotdata$year>=2018)
plotdata32<-subset(plotdata,plotdata$year>=2018)
plotdata33<-subset(plotdata,plotdata$year>=2018)
plotdata34<-subset(plotdata,plotdata$year>=2018)
plotdata35<-subset(plotdata,plotdata$year>=2018)
plotdata36<-subset(plotdata,plotdata$year>=2018)
plotdata37<-subset(plotdata,plotdata$year>=2018)
plotdata38<-subset(plotdata,plotdata$year>=2018)
plotdata39<-subset(plotdata,plotdata$year>=2018)
plotdata40<-subset(plotdata,plotdata$year>=2018)
plotdata41<-subset(plotdata,plotdata$year>=2018)
plotdata42<-subset(plotdata,plotdata$year>=2018)
plotdata43<-subset(plotdata,plotdata$year>=2018)
plotdata44<-subset(plotdata,plotdata$year>=2018)
plotdata45<-subset(plotdata,plotdata$year>=2018)
plotdata46<-subset(plotdata,plotdata$year>=2018)
plotdata47<-subset(plotdata,plotdata$year>=2018)
plotdata48<-subset(plotdata,plotdata$year>=2018)
plotdata49<-subset(plotdata,plotdata$year>=2018)
plotdata50<-subset(plotdata,plotdata$year>=2018)

plotdata1$x<-2018.001
plotdata2$x<-2018.002
plotdata3$x<-2018.003
plotdata4$x<-2018.004
plotdata5$x<-2018.005
plotdata6$x<-2018.006
plotdata7$x<-2018.007
plotdata8$x<-2018.008
plotdata9$x<-2018.009
plotdata10$x<-2018.010
plotdata11$x<-2018.011
plotdata12$x<-2018.012
plotdata13$x<-2018.013
plotdata14$x<-2018.014
plotdata15$x<-2018.015
plotdata16$x<-2018.016
plotdata17$x<-2018.017
plotdata18$x<-2018.018
plotdata19$x<-2018.019
plotdata20$x<-2018.020
plotdata21$x<-2018.021
plotdata22$x<-2018.022
plotdata23$x<-2018.023
plotdata24$x<-2018.024
plotdata25$x<-2018.025
plotdata26$x<-2018.026
plotdata27$x<-2018.027
plotdata28$x<-2018.028
plotdata29$x<-2018.029
plotdata30$x<-2018.030
plotdata31$x<-2018.031
plotdata32$x<-2018.032
plotdata33$x<-2018.033
plotdata34$x<-2018.034
plotdata35$x<-2018.035
plotdata36$x<-2018.036
plotdata37$x<-2018.037
plotdata38$x<-2018.038
plotdata39$x<-2018.039
plotdata40$x<-2018.040
plotdata41$x<-2018.041
plotdata42$x<-2018.042
plotdata43$x<-2018.043
plotdata44$x<-2018.044
plotdata45$x<-2018.045
plotdata46$x<-2018.046
plotdata47$x<-2018.047
plotdata48$x<-2018.048
plotdata49$x<-2018.049
plotdata50$x<-2018.050
plotdataB<-rbind(plotdata0,
                  plotdata1,plotdata2,plotdata3,plotdata4,plotdata5,
                  plotdata6,plotdata7,plotdata8,plotdata9,plotdata10,
                  plotdata11,plotdata12,plotdata13,plotdata14,plotdata15,
                  plotdata16,plotdata17,plotdata18,plotdata19,plotdata20,
                  plotdata21,plotdata22,plotdata23,plotdata24,plotdata25,
                  plotdata26,plotdata27,plotdata28,plotdata29,plotdata30,
                  plotdata31,plotdata32,plotdata33,plotdata34,plotdata35,
                  plotdata36,plotdata37,plotdata38,plotdata39,plotdata30,
                  plotdata41,plotdata42,plotdata43,plotdata44,plotdata45,
                  plotdata46,plotdata47,plotdata48,plotdata49,plotdata50)

p.magma <- ggplot(plotdataB, aes(x, y,frame=x,shape=event,color=ice_duration)) +
  geom_point(size=1.5)+
  scale_color_viridis(direction=-1,end=0.8,option="A")+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_bw()#

p.cume.magma<-p.magma +  transition_manual(x,cumulative=TRUE)

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
#p.viridis<-p1 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,end=0.95,option="D")
#p.viridis<-p1 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,begin=0.2,end=1,option="D")
#p.viridis<-p2 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,end=0.95,option="D")
p.viridis<-p2 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,option="D")

#p.viridis<-p1 +  transition_manual(x,cumulative=TRUE)+scale_color_viridis(direction=-1,low,option="D")
#p.viridis<-p1 +  transition_manual(x,cumulative=TRUE)+scale_color_gradient2(low="yellow",mid="green",high="blue")
#p.viridis<-p1 +  transition_manual(x,cumulative=TRUE)+scale_color_gradient(low="yellow",high="purple")


anim_save("ice.magma.gif", p.magma,width=400,height=250)
anim_save("ice.viridis.gif", p.viridis,width=400,height=250)



#magick::image_write(p2, path="myanimation.gif")
