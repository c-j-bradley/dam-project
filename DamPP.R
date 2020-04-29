library(readxl)
library(tibble)
library(ggplot2)
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

DamNut <- read_excel("AllSamples.xlsx", sheet = "Nutrients")
DamPlk <- read_excel("AllSamples.xlsx", sheet = "PLK")
DamNut2 <- cbind(DamNut[,1:4],c("POM"),DamNut[,7],DamNut[,10:14])
DamPlk2 <- cbind(DamPlk[,1:4],DamPlk[,8],DamPlk[,14],DamPlk[,17])
DamPlk2$DWtmgm3<-as.numeric(as.character(DamPlk2$DWtmgm3))


# Primary Productivity ----------------------------------------------------
#Split into Locations

Loc<-split(DamPlk2,f=DamPlk2$Loc)
BCD<-Loc$`BC`
BMP<-Loc$`BMP`
NR<-Loc$`NRC`

DamPP<-rbind(BCD,BMP)
DamPP$DWtmgm3<-as.numeric(as.character(DamPP$DWtmgm3))
SZ<-split(DamPP,DamPP$Group)
MAC<-SZ$MAC
MES<-SZ$MES
MIC<-SZ$MIC

SZb<-split(BCD,BCD$Group)
SZm<-split(BMP,BMP$Group)



BCmacma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$MAC,na.rm=TRUE, mean)
BCmacma$Group<-c("MAC")
BCmesma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$MES,na.rm=TRUE, mean)
BCmesma$Group<-c("MES")
BCmicma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$MIC,na.rm=TRUE, mean)
BCmicmaN<-rbind(c("FA","BC","DS","NA"),BCmicma)
BCmicmaN$Group<-c("MIC")
#BCpomma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$POM,na.rm=TRUE, mean)
MPmacma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$MAC,na.rm=TRUE, mean)
MPmacma$Group<-c("MAC")
MPmesma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$MES,na.rm=TRUE, mean)
MPmesmaN<-rbind(c("FA","BMP","DS","NA"),MPmesma)
MPmesmaN$Group<-c("MES")
MPmicma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$MIC,na.rm=TRUE, mean)
MPmicma$Group<-c("MIC")
#MPpomma<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$POM,na.rm=TRUE, mean)


BCmacse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$MAC,st.err)
BCmesse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$MES,st.err)
BCmicse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$MIC,st.err)
BCmicseN<-rbind(c("FA","BC","DS","NA"),BCmicse)
#BCpomse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZb$POM,st.err)
MPmacse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$MAC,st.err)
MPmesse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$MES,st.err)
MPmesseN<-rbind(c("FA","BC","DS","NA"),MPmesse)
MPmicse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$MIC,st.err)
#MPpomse<- aggregate(DWtmgm3 ~ Time + Loc + Site, SZm$POM,st.err)

MeanPPbc<-rbind(BCmacma, BCmesma,BCmicmaN)
SEPPbc<-rbind(BCmacse, BCmesse,BCmicseN)
AvePPbc<-cbind(MeanPPbc, SEPPbc$DWtmgm3)
colnames(AvePPbc)<-c("Season","Loc","Site","PP","Group","SE")

MeanPPmp<-rbind(MPmacma, MPmesmaN,MPmicma)
SEPPmp<-rbind(MPmacse, MPmesseN,MPmicse)
AvePPmp<-cbind(MeanPPmp, SEPPmp$DWtmgm3)
colnames(AvePPmp)<-c("Season","Loc","Site","PP","Group","SE")
AvePPmp$PP<-as.numeric(as.character(AvePPmp$PP))
AvePPmp$SE<-as.numeric(as.character(AvePPmp$SE))

PPaa<- aggregate(DWtmgm3 ~ Group+ Loc + Site, DamPlk2,na.rm=TRUE, mean)
PPse<- aggregate(DWtmgm3 ~ Group+ Loc + Site, DamPlk2,st.err)
AvePP<-cbind(PPaa,PPse$DWtmgm3)
colnames(AvePP)<-c("Group","Loc","Site","PP","SE")

AveALL<-rbind(AvePPbc,AvePPmp)
AveALL$PP<-as.numeric(as.character(AveALL$PP))
AveALL$SE<-as.numeric(as.character(AveALL$SE))
AveALL<-na.omit(AveALL)


# stats -------------------------------------------------------------------
t.test(DWtmgm3 ~ Loc, data=MES)
fit <- aov(DWtmgm3 ~ Time+Site, data=BCD)
summary(fit)
TukeyHSD(fit)


# plots -------------------------------------------------------------------
library(ggplot2)
library(scales)
cbPalette <- c("#999999", "#56B4E9","#E69F00",  "#009E73",  "#CC79A7","#D55E00",
               "#0072B2", "#F0E442", "#000000")
png("Biomass Seasonally 20182019log.png",units="in",width=6,height=4, res=300)
Cp1<-ggplot(data=AvePPmp, aes(x=factor(as.factor(Season), levels=c("WI","SP","SU","FA"),
                                      labels=c("Winter","Spring","Summer","Fall")), 
                     y=PP, 
                     color=factor(Site, levels=c("US","DS"))))+
  #geom_boxplot()+
  geom_point(size=4, position=position_dodge(0.7))+
  #geom_bar(stat="identity", position=position_dodge())+
  scale_color_manual(values=c("#E69F00", "#999999" ))+
  geom_errorbar(data=AvePPmp, aes(x=factor(as.factor(Season), 
                                          levels=c("WI","SP","SU","FA"),
                                          labels=c("Winter","Spring","Summer","Fall")),
                                          ymax=PP + SE, ymin=PP - SE), 
                width=0.1, position=position_dodge(0.7))+
  ylab(expression(paste("Filtered Dry Weight (mg m" ^-3,")")))+
  xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,7))
  #scale_y_continuous(trans = log2_trans(),
                     #breaks = trans_breaks("log2", function(x) 2^x),
                     #labels = trans_format("log2", math_format(2^.x)))
  #scale_y_log10(breaks=trans_breaks("log10",function(x)10^x),
               #labels = trans_format("log10", math_format(10^.x)))
  

Cp1+ facet_grid(Group ~ Loc,scales="fixed")+
  theme_bw()+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(), legend.title = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()





# Nutrients ---------------------------------------------------------------
#Split into Locations
Loc<-split(DamNut,f=DamNut$Loc)
BCD<-Loc$`BC`
BMP<-Loc$`BMP`
NR<-Loc$`NRC`

AvBCno<- aggregate(BCD$`NO3` ~ `Time`+`Site`,BCD,na.rm=TRUE, mean)
SEBCno<- aggregate(BCD$`NO3` ~ `Time`+`Site`,BCD, st.err)

AvBMno<- aggregate(BMP$`NO3` ~ `Time`+`Site`,BMP,na.rm=TRUE, mean)
SEBMno<- aggregate(BMP$`NO3` ~ `Time`+`Site`,BMP, st.err)

AVno<-cbind(AvBCno,SEBCno[,3])
colnames(AVno)<-c("Time","Site","NO3","noSE")
AVno$Loc <- c("BC")
AVno2<-cbind(AvBMno,SEBMno[,3])
colnames(AVno2)<-c("Time","Site","NO3","noSE")
AVno2$Loc <- c("BM")
AVEno<-cbind(AVno[,1:4],AVpo[,3:5])
AVEpo<-cbind(AVno2[,1:4],AVpo2[,3:5])
AVEnut<-rbind(AVEno,AVEpo)

cbPalette <- c("#999999", "#56B4E9","#E69F00",  "#009E73",  "#CC79A7","#D55E00",
               "#0072B2", "#F0E442", "#000000")
pdf("phosphate 2018 seasonally.pdf",6,3.5)
Cp1<-ggplot(AVEnut, aes(x=factor(Site, levels=c("US","DA","DS")), 
                          y=PO4, 
                          fill=factor(Time, levels=c("SP","SU","FA")),
                          color=factor(Loc, levels=c("BC","BM"))))+ 
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_manual(values=c("#f1a115","#f5f11e"))+
  scale_fill_manual(values=cbPalette)+
  geom_errorbar(data=AVEnut, aes(ymax=PO4 + poSE, ymin=PO4 - poSE), 
                width=0.4,position=position_dodge(0.9))

Cp1+
  theme_bw()+
  ylab(expression(paste("Phosphate (mg/L)")))+
  xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,2.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(), legend.title = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()
