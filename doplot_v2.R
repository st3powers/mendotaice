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
packagesCRAN<-c("plyr","dplyr","devtools","reshape","tidyr","ggthemes","RColorBrewer","scales","tweenr",
                "tidyverse","forcats","lubridate","viridis","gifski","png")

#iterate usePackage function across package names
lapply(packagesCRAN,FUN=usePackage)

#additional finicky packages, handle separately

#install_github("tidyverse/ggplot2",force=TRUE)
library("ggplot2")

#install_github("dkahle/ggmap", force=TRUE)
#library("ggmap")

#install_github("hrbrmstr/ggalt", force=TRUE)
#library("ggalt")

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
tweendata<-df
tweendata$txt<-as.character(tweendata$txt)

tweendata<-tweendata[order(tweendata$txt,tweendata$z),]
#tweendata$colour[which(tweendata$x==0)]<-""
tweendata$colour<-as.numeric(tweendata$txt2)

#tweendata0<-subset(tweendata,tweendata$year>=2000)
tweendata0<-tweendata
tweendata1<-subset(tweendata,tweendata$year>=2018)
tweendata2<-subset(tweendata,tweendata$year>=2018)
tweendata3<-subset(tweendata,tweendata$year>=2018)
tweendata4<-subset(tweendata,tweendata$year>=2018)
tweendata5<-subset(tweendata,tweendata$year>=2018)
tweendata6<-subset(tweendata,tweendata$year>=2018)
tweendata7<-subset(tweendata,tweendata$year>=2018)
tweendata8<-subset(tweendata,tweendata$year>=2018)
tweendata9<-subset(tweendata,tweendata$year>=2018)
tweendata10<-subset(tweendata,tweendata$year>=2018)
tweendata11<-subset(tweendata,tweendata$year>=2018)
tweendata12<-subset(tweendata,tweendata$year>=2018)
tweendata13<-subset(tweendata,tweendata$year>=2018)
tweendata14<-subset(tweendata,tweendata$year>=2018)
tweendata15<-subset(tweendata,tweendata$year>=2018)
tweendata16<-subset(tweendata,tweendata$year>=2018)
tweendata17<-subset(tweendata,tweendata$year>=2018)
tweendata18<-subset(tweendata,tweendata$year>=2018)
tweendata19<-subset(tweendata,tweendata$year>=2018)
tweendata20<-subset(tweendata,tweendata$year>=2018)
tweendata21<-subset(tweendata,tweendata$year>=2018)
tweendata22<-subset(tweendata,tweendata$year>=2018)
tweendata23<-subset(tweendata,tweendata$year>=2018)
tweendata24<-subset(tweendata,tweendata$year>=2018)
tweendata25<-subset(tweendata,tweendata$year>=2018)
tweendata26<-subset(tweendata,tweendata$year>=2018)
tweendata27<-subset(tweendata,tweendata$year>=2018)
tweendata28<-subset(tweendata,tweendata$year>=2018)
tweendata29<-subset(tweendata,tweendata$year>=2018)
tweendata30<-subset(tweendata,tweendata$year>=2018)
tweendata31<-subset(tweendata,tweendata$year>=2018)
tweendata32<-subset(tweendata,tweendata$year>=2018)
tweendata33<-subset(tweendata,tweendata$year>=2018)
tweendata34<-subset(tweendata,tweendata$year>=2018)
tweendata35<-subset(tweendata,tweendata$year>=2018)
tweendata36<-subset(tweendata,tweendata$year>=2018)
tweendata37<-subset(tweendata,tweendata$year>=2018)
tweendata38<-subset(tweendata,tweendata$year>=2018)
tweendata39<-subset(tweendata,tweendata$year>=2018)
tweendata40<-subset(tweendata,tweendata$year>=2018)
tweendata41<-subset(tweendata,tweendata$year>=2018)
tweendata42<-subset(tweendata,tweendata$year>=2018)
tweendata43<-subset(tweendata,tweendata$year>=2018)
tweendata44<-subset(tweendata,tweendata$year>=2018)
tweendata45<-subset(tweendata,tweendata$year>=2018)
tweendata46<-subset(tweendata,tweendata$year>=2018)
tweendata47<-subset(tweendata,tweendata$year>=2018)
tweendata48<-subset(tweendata,tweendata$year>=2018)
tweendata49<-subset(tweendata,tweendata$year>=2018)
tweendata50<-subset(tweendata,tweendata$year>=2018)

tweendata1$x<-2018.001
tweendata2$x<-2018.002
tweendata3$x<-2018.003
tweendata4$x<-2018.004
tweendata5$x<-2018.005
tweendata6$x<-2018.006
tweendata7$x<-2018.007
tweendata8$x<-2018.008
tweendata9$x<-2018.009
tweendata10$x<-2018.010
tweendata11$x<-2018.011
tweendata12$x<-2018.012
tweendata13$x<-2018.013
tweendata14$x<-2018.014
tweendata15$x<-2018.015
tweendata16$x<-2018.016
tweendata17$x<-2018.017
tweendata18$x<-2018.018
tweendata19$x<-2018.019
tweendata20$x<-2018.020
tweendata21$x<-2018.021
tweendata22$x<-2018.022
tweendata23$x<-2018.023
tweendata24$x<-2018.024
tweendata25$x<-2018.025
tweendata26$x<-2018.026
tweendata27$x<-2018.027
tweendata28$x<-2018.028
tweendata29$x<-2018.029
tweendata30$x<-2018.030
tweendata31$x<-2018.031
tweendata32$x<-2018.032
tweendata33$x<-2018.033
tweendata34$x<-2018.034
tweendata35$x<-2018.035
tweendata36$x<-2018.036
tweendata37$x<-2018.037
tweendata38$x<-2018.038
tweendata39$x<-2018.039
tweendata40$x<-2018.040
tweendata41$x<-2018.041
tweendata42$x<-2018.042
tweendata43$x<-2018.043
tweendata44$x<-2018.044
tweendata45$x<-2018.045
tweendata46$x<-2018.046
tweendata47$x<-2018.047
tweendata48$x<-2018.048
tweendata49$x<-2018.049
tweendata50$x<-2018.050
tweendataB<-rbind(tweendata0,
                  tweendata1,tweendata2,tweendata3,tweendata4,tweendata5,
                  tweendata6,tweendata7,tweendata8,tweendata9,tweendata10,
                  tweendata11,tweendata12,tweendata13,tweendata14,tweendata15,
                  tweendata16,tweendata17,tweendata18,tweendata19,tweendata20,
                  tweendata21,tweendata22,tweendata23,tweendata24,tweendata25,
                  tweendata26,tweendata27,tweendata28,tweendata29,tweendata30,
                  tweendata31,tweendata32,tweendata33,tweendata34,tweendata35,
                  tweendata36,tweendata37,tweendata38,tweendata39,tweendata30,
                  tweendata41,tweendata42,tweendata43,tweendata44,tweendata45,
                  tweendata46,tweendata47,tweendata48,tweendata49,tweendata50)

p.magma <- ggplot(tweendataB, aes(x, y,frame=x,shape=event,color=ice_duration)) +
  geom_point(size=1.5)+
  scale_color_viridis(direction=-1,end=0.8,option="A")+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_bw()#

p.cume.magma<-p.magma +  transition_manual(x,cumulative=TRUE)

tweendataB$ice_duration2<-tweendataB$ice_duration
tweendataB$ice_duration2[which(tweendataB$ice_duration2<=60)]<-60

p1 <- ggplot(tweendataB, aes(x, y,frame=x,shape=event,color=ice_duration)) +
  geom_point(size=1.5)+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_bw()#

p2 <- ggplot(tweendataB, aes(x, y,frame=x,shape=event,color=ice_duration2)) +
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
