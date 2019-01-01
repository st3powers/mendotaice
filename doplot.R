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
packagesCRAN<-c("plyr","dplyr","devtools","reshape","gridExtra","hexbin","tidyr","rpart","rpart.plot",
                "party","partykit","dggridR","rgdal","ggthemes","RColorBrewer","scales","tweenr",
                "tidyverse","forcats","lubridate","viridis")

#iterate usePackage function across package names
lapply(packagesCRAN,FUN=usePackage)

#additional finicky packages, handle separately

#install_github("tidyverse/ggplot2",force=TRUE)
library("ggplot2")

#install_github("dkahle/ggmap", force=TRUE)
library("ggmap")

#install_github("hrbrmstr/ggalt", force=TRUE)
library("ggalt")

#install_github("dgrtwo/gganimate", force=TRUE)
library("gganimate")

ice<-read.csv("data.csv")
names(ice)[4]<-"ice_duration"
ice<-ice[-c(1:3),]
ice$year<-as.numeric(substr(ice$WINTER,1,4))+1
#ice$DAYS<-as.numeric(substr(ice$DAYS,nchar(as.character(ice$DAYS))-3,nchar(ice$DAYS)))

#ice<-ice[-which(is.na(ice$ice_duration)==TRUE),]

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
  #  return(closed_doy)
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


#ice$closed_doy<-doy(ice)[,1]
#ice$opened_doy<-doy(ice)[,2]

#as.Date(as.character(ice$OPENED))
#mdy(ice$OPENED)
names(ice2)


#closed<-ice %>% select(year,DAYS,date=CLOSED0,doy=closed_doy) %>% as.data.frame()
#closed$event<-"closed"
#opened<-ice %>% select(year,DAYS,date=OPENED0,doy=opened_doy) %>% as.data.frame()
#opened$event<-"opened"

#ice2<-rbind(closed,opened)

##############################
datax<-ice2
datax$z<-datax$year
datax$x<-datax$year
#datax$y<-datax$DAYS
datax$y<-datax$doy
datax$txt<-datax$year
datax$txt2<-datax$ice_duration
#datax$z<-1



for(i in 1:length(unique(datax$z))) {
  yeari<-unique(datax$z)[i]
  namei<-paste("datax.",yeari,sep="")
  data.i<-subset(datax,datax$z==yeari)
  assign(namei,data.i)
}

do<-0
if(do==1){
datax.2002<-subset(datax,datax$z==2002)
datax.2003<-subset(datax,datax$z==2003)
datax.2004<-subset(datax,datax$z==2004)
datax.2005<-subset(datax,datax$z==2005)
datax.2006<-subset(datax,datax$z==2006)
datax.2007<-subset(datax,datax$z==2007)
datax.2008<-subset(datax,datax$z==2008)
datax.2009<-subset(datax,datax$z==2009)
datax.2010<-subset(datax,datax$z==2010)
datax.2011<-subset(datax,datax$z==2011)
datax.2012<-subset(datax,datax$z==2012)
datax.2013<-subset(datax,datax$z==2013)

rbind<-rbind(datax.2002, datax.2003, datax.2004, datax.2005,
             datax.2006, datax.2007, datax.2008, datax.2009,
             datax.2010, datax.2011, datax.2012, datax.2013)
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
#rbind.end<-rbind(datax.2018)
rbind.end<-rbind.end[order(rbind.end$z,rbind.end$z,decreasing=TRUE),]


#rbind<-rbind(rbind,datax.2018,datax.2018,datax.2018,datax.2018,datax.2018,
#             datax.2018,datax.2018,datax.2018,datax.2018,datax.2018,
#             datax.2018,datax.2018,datax.2018,datax.2018,datax.2018,
#             datax.2018,datax.2018,datax.2018,datax.2018,datax.2018,
#             datax.2018,datax.2018,datax.2018,datax.2018,datax.2018,
#             datax.2018,datax.2018,datax.2018,datax.2018,datax.2018)



n.approx<-100
do<-0
if(do==1){
groups<-unique(rbind$z)
for(i in 1:length(groups)){
  datai<-subset(rbind,rbind$z==groups[i])
  data2.i<-datai
  data2.i$z<-datai$z+0.005
  data3.i<-datai
  data3.i$z<-datai$z+0.01
  data4.i<-datai
  data4.i$z<-datai$z+0.03
  datai<-rbind(datai,data2.i,data3.i)#,data3.i)#,data4.i)
  datai<-datai[order(datai$z),]
  approx.x.i<-approx(datai$x,n=n.approx)$y
  approx.y.i<-approx(datai$y,n=n.approx)$y
  approx.i<-approx(datai$x,datai$y,n=n.approx)
  #  approx.x.i<-approx.i$x
  #  approx.y.i<-approx.i$y
  approx.z.i<-approx(datai$z,n=n.approx)$y
  
  dfi<-data.frame(x=approx.x.i,y=approx.y.i,z=approx.z.i)
  dfi$colour<-"forestgreen"
  dfi$txt<-rep(datai$txt[1],length(dfi[,1]))
  dfi$txt2<-rep(datai$txt2[1],length(dfi[,1]))
  if(i==1){df<-dfi}
  if(i>1){df<-rbind(df,dfi)}
}
}

df<-rbind
tweendata<-df
tweendata$txt<-as.character(tweendata$txt)

tweendata<-tweendata[order(tweendata$txt,tweendata$z),]
#tweendata$colour[which(tweendata$x==0)]<-""
tweendata$colour<-as.numeric(tweendata$txt2)

# checking china- Why does it pulse in between years that have very similar x and y values? 
# what we want is a smoother interpolation using tween_state().
# on 7 Aug 2017 SP tried numerous "easing functions" (e.g., cubic-in-out, quadratic-in, etc.) and none product the desired behavior (?)
# remains unresolved
#subset(tweendata,tweendata$txt=="CHN" & tweendata$z>=2003 & tweendata$z<=2004)

#tweendata$y[which(tweendata$y<=-2)]<-0.5*tweendata$y[which(tweendata$y<=-2)]
#tweendata$y[which(tweendata$y<=-1.5)]<--1.5
#tweendata$y[which(tweendata$y>=1.5)]<-1.5
#tweendata$x[which(tweendata$x<=10^4.3 & tweendata$x>0)]<-10^4.3
#tweendata$y[which(tweendata$x==0)]<-0

do<-0

if(do==1){

p <- ggplot(tweendata, aes(x = (x+1), y = y),cumulative=TRUE) +
  geom_text(aes(frame=signif(z,7),label=txt,color=as.factor(colour)))+
  #  geom_line(aes(frame=z,label=txt,color=as.factor(colour)))+
  #  ggtitle(substr(z,1,4),data=tweendata)+
  #  xlim(0,100)+ylim(0,100)+
#  xlim(4.3,7.2)+ylim(-1.5,1.5)+
  xlab("Year")+ylab("Day")+
  theme(legend.position="none",axis.text=element_text(size=14),axis.title=element_text(size=14))
#
#  geom_hline(yintercept=1,linetype=2,color="gray",size=1.1)+
#  geom_hline(yintercept=-1,linetype=2,color="gray",size=1.1)+
#  geom_hline(yintercept=0.5, linetype=2,color="gray",size=1.1)+
#  annotate("text",x=6.7,y=-1.1,label="net export > consumption", color="gray")+
#  annotate("text", x=6.7,y=1.1,label="net import > consumption", color="gray")+
#  annotate("text", x=6.7,y=0.6,label="net import = 50% of consumption", color="gray")
#p<-p +
#  geom_point(aes(x = x, y = y), tweendata) 

animation::ani.options(interval = 1 / 10)
gganimate(p)#, title_frame = FALSE)




p<- ggplot(tweendata, aes(x = x, y = y, color = y, group = y)) +
  geom_line(size = 2, alpha = 0) + # making transparent so the entire color scale is plotted but not data that is not yet
#  geom_line(data = dplyr::filter(tweendata, year <= year),
#            aes(x = water_day, y = duration, color = winter_start), alpha = .5, size = 2) +
#  geom_line(data = dplyr::filter(ice_all, winter_start == winter_year),
#            aes(x = water_day, y = duration, color = winter_start), alpha = 1, size = 6) +
  theme_classic() +
  theme(axis.title = element_text(size =14),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.position = c(.1,.15),
        legend.text = element_text(size = 12),
        legend.title = element_blank())+
  ylab('Ice Duration (days)') +
#  scale_color_viridis_c(option = 'plasma', direction = -1, breaks = c(1855, 1935, 2015), trans= 'reverse') +
  annotate('text', x = 200, y = 30, label = year, size = 8)# +
#  scale_x_continuous(name = '', breaks = wd_breaks, labels = strftime(water_day_lookup$date[wd_breaks], format = '%b'))

animation::ani.options(interval = 1 / 10)
gganimate(p)#, title_frame = FALSE)

}

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
#tweendata1b

#tweendata1$size<-0.5
#tweendata1b<-tweendata1
#tweendata1b$doy<-tweendata1b$DAYS
#tweendata1b$size<-tweendata1b$DAYS/100
#tweendata1b$y<-100
#tweendata1<-rbind(tweendata1,tweendata1b)

p2 <- ggplot(tweendataB, aes(x, y,frame=x, cumulative = TRUE,shape=event,color=ice_duration)) +
  geom_point(size=1.5)+
  scale_color_viridis(direction=-1,end=0.8,option="A")+
#  scale_color_manual(values=c("dark gray","dark blue"))+
#  geom_segment(aes(data=tweendata1,xend = x, yend = 100), size = y/100, lineend = "butt",linejoin="bevel",color="gray")+
#  geom_text(x=x,y=y,label=DAYS,data=tweendata)+
#  geom_text(x=2000,y=150,label=year,data=tweendata1)+
  ylim(-60,150)+
  xlab("Year")+ylab("Days since January 1st")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
#  scale_y_continuous(breaks=seq(-60,-40,-20,0,20,40,60,80,100,120,140))+
#  scale_x_continuous(breaks=seq(1860,1880,1900,1920,1940,1960,1980,2000,2020))#+
#  geom_point(tweendata1,aes(x,y=100,size=y/100))+
#  guides(reverse=TRUE)+
#  guides(fill=guide_legend(title="ice duration (days)"))+
  theme_bw()#
#legend.position="none",axis.text=element_text(size=14),axis.title=element_text(size=14))

gganimate(p2, title_frame = FALSE,"ice.gif",
          ani.width= 400, ani.height=250)

#gganimate(p2,renderer = gifski_renderer(loop = F))

#system("convert -delay 10 charts/*.jpg warming2.gif")
#anim_save("ice.gif")
