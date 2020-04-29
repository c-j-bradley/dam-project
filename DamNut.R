library(readxl)
Nutdata <- read_excel("Dam nutrients.xlsx", 
                      col_types = c("text", "numeric", "numeric", 
                                    "text", "text", "text", "text", "numeric", 
                                    "text", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "text", "text"))
colnames(Nutdata)<-c("Collection","Date","PD","Season","Loc","Sites","Site","Year","ID","Time","pWt",
                     "Filtered","Wet","Dry","NetWt","DryWt","NO2","NO3","PO4","NH4",
                     "TN","Notes","Filters")
Nutdata$Month<-months(Nutdata$Date)

#only upstream and downstream sites
lc<-split(Nutdata,f=Nutdata$Site)
USn<-lc$US
DSn<-lc$DS
Nutdata2<-rbind(USn,DSn)

#separate sites for descriptive stats
st<-split(Nutdata2,f=Nutdata2$Loc)
BCDn<-st$`BC`
BMPn<-st$`BMP`

#descriptive stats
BNOma<- aggregate(NO3 ~ Season + Loc + Site, BCDn,na.rm=TRUE, mean)
MNOma<- aggregate(NO3 ~ Season + Loc+ Site, BMPn,na.rm=TRUE, mean)
BTNma<- aggregate(TN ~ Season + Loc + Site, BCDn,na.rm=TRUE, mean)
MTNma<- aggregate(TN ~ Season + Loc+ Site, BMPn,na.rm=TRUE, mean)
BN2ma<- aggregate(NO2 ~ Season + Loc + Site, BCDn,na.rm=TRUE, mean)
MN2ma<- aggregate(NO2 ~ Season + Loc+ Site, BMPn,na.rm=TRUE, mean)
BAMma<- aggregate(NH4 ~ Season + Loc + Site, BCDn,na.rm=TRUE, mean)
MAMma<- aggregate(NH4 ~ Season + Loc+ Site, BMPn,na.rm=TRUE, mean)
BPOma<- aggregate(PO4 ~ Season + Loc+ Site, BCDn,na.rm=TRUE, mean)
MPOma<- aggregate(PO4 ~ Season + Loc+ Site, BMPn,na.rm=TRUE, mean)
BNOa<-aggregate(NO3 ~ Loc+Site,BCDn,na.rm=TRUE,mean)
BN2a<-aggregate(NO2 ~ Loc+Site,BCDn,na.rm=TRUE,mean)
BTNa<-aggregate(TN ~ Loc+Site,BCDn,na.rm=TRUE,mean)
BAMa<-aggregate(NH4 ~ Loc+Site,BCDn,na.rm=TRUE,mean)
BPOa<-aggregate(PO4 ~ Loc+Site,BCDn,na.rm=TRUE,mean)
MNOa<-aggregate(NO3 ~ Loc+Site,BMPn,na.rm=TRUE,mean)
MN2a<-aggregate(NO2 ~ Loc+Site,BMPn,na.rm=TRUE,mean)
MTNa<-aggregate(TN ~ Loc+Site,BMPn,na.rm=TRUE,mean)
MAMa<-aggregate(NH4 ~ Loc+Site,BMPn,na.rm=TRUE,mean)
MPOa<-aggregate(PO4 ~ Loc+Site,BMPn,na.rm=TRUE,mean)


BNOms<- aggregate(NO3 ~ Season + Loc + Site, BCDn, st.err)
MNOms<- aggregate(NO3 ~ Season + Loc+ Site, BMPn, st.err)
BTNms<- aggregate(TN ~ Season + Loc + Site, BCDn, st.err)
MTNms<- aggregate(TN ~ Season + Loc+ Site, BMPn, st.err)
BN2ms<- aggregate(NO2 ~ Season + Loc + Site, BCDn, st.err)
MN2ms<- aggregate(NO2 ~ Season + Loc+ Site, BMPn, st.err)
BAMms<- aggregate(NH4 ~ Season + Loc + Site, BCDn, st.err)
MAMms<- aggregate(NH4 ~ Season + Loc+ Site, BMPn, st.err)
BPOms<- aggregate(PO4 ~ Season + Loc+ Site, BCDn, st.err)
MPOms<- aggregate(PO4 ~ Season + Loc+ Site, BMPn, st.err)
BNOse<-aggregate(NO3 ~ Loc+Site,BCDn,st.err)
BN2se<-aggregate(NO2 ~ Loc+Site,BCDn,st.err)
BTNse<-aggregate(TN ~ Loc+Site,BCDn,st.err)
BAMse<-aggregate(NH4 ~ Loc+Site,BCDn,st.err)
BPOse<-aggregate(PO4 ~ Loc+Site,BCDn,st.err)
MNOse<-aggregate(NO3 ~ Loc+Site,BMPn,st.err)
MN2se<-aggregate(NO2 ~ Loc+Site,BMPn,st.err)
MTNse<-aggregate(TN ~ Loc+Site,BMPn,st.err)
MAMse<-aggregate(NH4 ~ Loc+Site,BMPn,st.err)
MPOse<-aggregate(PO4 ~ Loc+Site,BMPn,st.err)

MN2ma <-rbind(MN2ma,NA)
MN2ms <-rbind(MN2ms,NA)
BNOa<-cbind("Annual",BNOa)
MNOa<-cbind("Annual",MNOa)

AnnNutBC<-cbind(BNOa,BNOse$NO3,BTNa$TN,BTNse$TN,BN2a$NO2,BN2se$NO2,BAMa$NH4,BAMse$NH4,
              BPOa$PO4,BPOse$PO4)
colnames(AnnNutBC)<-c("Season","Loc","Site","NO3","Nsd","TN","TNsd","NO2","NO2sd",
                   "NH4","NH4sd","PO4","Psd")

AnnNutBM<-cbind(MNOa,MNOse$NO3,MTNa$TN,MTNse$TN,MN2a$NO2,MN2se$NO2,MAMa$NH4,MAMse$NH4,
                MPOa$PO4,MPOse$PO4)
colnames(AnnNutBM)<-c("Season","Loc","Site","NO3","Nsd","TN","TNsd","NO2","NO2sd",
                      "NH4","NH4sd","PO4","Psd")

BCnut<-cbind(BNOma, BNOms$NO3, BTNma$TN, BTNms$TN, BN2ma$NO2, BN2ms$NO2, 
             BAMma$NH4, BAMms$NH4, BPOma$PO4, BPOms$PO4)
colnames(BCnut)<-c("Season","Loc","Site","NO3","Nsd","TN","TNsd","NO2","NO2sd",
                    "NH4","NH4sd","PO4","Psd")
BMnut<-cbind(MNOma,MNOms$NO3,MTNma$TN, MTNms$TN, MN2ma$NO2, MN2ms$NO2, 
             MAMma$NH4, MAMms$NH4, MPOma$PO4, MPOms$PO4)
colnames(BMnut)<-c("Season","Loc","Site","NO3","Nsd","TN","TNsd","NO2","NO2sd",
                   "NH4","NH4sd","PO4","Psd")
AllNut<-rbind(BCnut,BMnut,AnnNutBC,AnnNutBM)

#plots
library(ggplot2)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7","#999999", "#000000")

#nutrient boxplots
png("Big Mill PO Season.png",units="in",width=6,height=4, res=300)
p1<-ggplot(BMPn, aes(x=factor(Season, levels=c("Winter","Spring", "Summer", "Fall")), 
                        y=PO4, fill=factor(Site, levels=c("US","DS"))))+
  geom_boxplot()+
  xlab("")+
  #ylab(expression(paste("NO" [3],"+ NO" [2],"+ NH" [4], " (mg/L)")))+
  #ylab(expression(paste("NO" [3], " (mg/L)")))+
  ylab(expression(paste("PO" [4], " (mg/L)")))+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(limits=c(0,1))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="top",
                   axis.line = element_line(colour = "black"))

p1 + facet_grid(. ~ Loc)
dev.off()


#mean + error plots
pdf("NO3 Site Comp 2019.pdf",6,3.5)
y1<-ggplot(AllNut, aes(x=factor(Season, levels=c("Spring18","Summer18","Fall18",
                                                 "Winter19",
                                                 "Spring19")), 
                    y=PO4, 
                    fill=factor(Site, levels=c("BC","BMP")),
                    shape=factor(Loc, levels=c("US","DS"))))+ 
  geom_point(size=4)+ 
  scale_shape_manual(values=c(21,22))+
  geom_errorbar(data=AllNut, aes(ymin = PO4 - Psd, ymax = PO4 + Psd), width=0.2)+
  xlab("")+
  ylab("Phosphate (mg/L)")+
  scale_color_manual(values=c("#0c0c0c","#999999"))+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(limits=c(0,1))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="top",
                   axis.line = element_line(colour = "black"))
y1

dev.off()

#stats
t.test(PO4 ~ Site, data=BMPn)
fit <- aov(PO4 ~ Loc + Season, data=Nutdata2)
summary(fit)
TukeyHSD(fit)

