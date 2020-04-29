library(readxl)

YSI <- read_excel("YSI data 2018 to Apr 2019.xlsx", 
                  sheet = "Sheet1", range = "A1:K10000", 
                  col_types = c("date", "date", "text","text", 
                                "text", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
colnames(YSI)<-c("Date","Time","Season","Loc","Site","Depth","Temp","Sal","pH","DO","Chl")
YSI$Month<-months(YSI$Date)
lc<-split(YSI,f=YSI$Loc)
US<-lc$US
DS<-lc$DS
YSI2<-rbind(US,DS)


st<-split(YSI2,f=YSI2$Site)
BCD<-st$`BCD`
BMP<-st$`BMP`

BTma<- aggregate(Temp ~ Season + Loc + Site, BCD,na.rm=TRUE, mean)
MTma<- aggregate(Temp ~ Season + Loc+ Site, BMP,na.rm=TRUE, mean)
BOma<- aggregate(DO ~ Season + Loc+ Site, BCD,na.rm=TRUE, mean)
MOma<- aggregate(DO ~ Season + Loc+ Site, BMP,na.rm=TRUE, mean)
BCma<- aggregate(Chl ~ Season + Loc+ Site, BCD,na.rm=TRUE, mean)
MCma<- aggregate(Chl ~ Season + Loc+ Site, BMP,na.rm=TRUE, mean)
BTa<- aggregate(Temp ~ Site, YSI2,na.rm=TRUE, mean)
BTse<- aggregate(Temp ~ Site, YSI2,st.err)
BOma<- aggregate(DO ~ Season + Loc+ Site, BCD,na.rm=TRUE, mean)
MOma<- aggregate(DO ~ Season + Loc+ Site, BMP,na.rm=TRUE, mean)
BCma<- aggregate(Chl ~ Season + Loc+ Site, BCD,na.rm=TRUE, mean)
MCma<- aggregate(Chl ~ Season + Loc+ Site, BMP,na.rm=TRUE, mean)


BTmse<- aggregate(Temp ~ Season + Loc + Site, BCD,st.err)
MTmse<- aggregate(Temp ~ Season + Loc+ Site, BMP,st.err)
BOmse<- aggregate(DO ~ Season + Loc+ Site, BCD,st.err)
MOmse<- aggregate(DO ~ Season + Loc+ Site, BMP,st.err)
BCmse<- aggregate(Chl ~ Season + Loc+ Site, BCD,st.err)
MCmse<- aggregate(Chl ~ Season + Loc+ Site, BMP,st.err)

BCT<-cbind(BTma,BTmse$Temp)
colnames(BCT)<-c("Season","Loc","Site","Temp","Tse")
BCO<-cbind(BOma,BOmse$DO)
colnames(BCO)<-c("Season","Loc","Site","DO","DOse")
BCCh<-cbind(BCma,BCmse$Chl)
colnames(BCCh)<-c("Season","Loc","Site","Chl","Cse")

BMT<-cbind(MTma,MTmse$Temp)
colnames(BMT)<-c("Season","Loc","Site","Temp","Tse")
BMO<-cbind(MOma,MOmse$DO)
colnames(BMO)<-c("Season","Loc","Site","DO","DOse")
BMCh<-cbind(MCma,MCmse$Chl)
colnames(BMCh)<-c("Season","Loc","Site","Chl","Cse")
Temp<-rbind(BCT,BMT)
Ox<-rbind(BCO,BMO)
Chl<-rbind(BCCh,BMCh)

#stats
t.test(Chl ~ Site, data=US)
fit <- aov(Chl ~ Loc + Season, data=BMP)
summary(fit)
TukeyHSD(fit)



library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7","#000000")


png("DO BigMill season.png",units="in",width=6,height=4, res=300)
y1<-ggplot(BMO, aes(x=factor(Season, levels=c("Winter","Spring","Summer","Fall")), 
                        y=DO, 
                        fill=factor(Loc, levels=c("DS","US"))))+
                     
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(data=BMO, aes(ymax=DO +DOse, ymin=DO - DOse), 
                               width=0.4,position=position_dodge(0.9))+
  xlab("")+
  ylab("Dissolved Oxygen (mg/L)")+
   scale_fill_manual(values=cbPalette)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="bottom",
                   axis.line = element_line(colour = "black"))
y1 + facet_grid(. ~ Site)
dev.off()
  
p1<-ggplot