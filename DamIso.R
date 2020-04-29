library(readxl)
library(tibble)
library(ggplot2)

st.err <- function(x) {
  sd(x)/sqrt(length(x))
}


#DamIso18 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/SU/415+ Research/Fish Passage/Dam results CN 180619.xlsx", 
                       #sheet = "Isotopes")
FishIso <- read_excel("AllSamples.xlsx", sheet = "Fish")
NutIso <- read_excel("AllSamples.xlsx", sheet = "Nutrients")
PlkIso <- read_excel("AllSamples.xlsx", sheet = "Plankton")
CBIso <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/SU/Grad Committees/Sarah/Results SC CNCBP Sp19 190531.xlsx")
z<-split(CBIso, f=CBIso$Type)
CBzp<-rbind(z$MAC,z$MES,z$MIC)
CBzp$Location<-c("MCB")
CBzp$Loc<-c("MCB")

#clean and remove excess data
DamIsoF <- cbind(FishIso[,2],FishIso[,4:5],FishIso[,21],FishIso[,24:27])
colnames(DamIsoF) <- c("Time","Loc","Site","Type","dC","%C","dN","%N")
#with names for diversity
DamIsoF <- cbind(FishIso[,2],FishIso[,4:5],FishIso[,11],FishIso[,21],FishIso[,24:27])
colnames(DamIsoF) <- c("Time","Loc","Site","Name","Type","dC","%C","dN","%N")

DamIsoF <- na.omit(DamIsoF)
#remove Bishopville
Loc<-split(DamIsoF,f=DamIsoF$Loc)
BV<-Loc$`BV`
BC<-Loc$`BC`
BMP<-Loc$`BMP`
NR<-Loc$`NRC`
DamIsoF2 <-rbind(Loc$BC,Loc$BMP)
  
DamIsoP <- cbind(PlkIso[,2],PlkIso[,4],PlkIso[,3],PlkIso[,8],PlkIso[,20:23])
colnames(DamIsoP) <- c("Time","Loc","Site","Type","dC","%C","dN","%N")
CBIsoP <- cbind(CBzp[,6],CBzp[,14],CBzp[,4],CBzp[,3],CBzp[,7:10])
colnames(CBIsoP) <- c("Time","Loc","Site","Type","dC","%C","dN","%N")
DamIso <- rbind(DamIsoF2, DamIsoP,CBIsoP)
#Split into Locations
Loc<-split(DamIsoF,f=DamIsoF$Loc)
BV<-Loc$`BV`
BC<-Loc$`BC`
BMP<-Loc$`BMP`
NR<-Loc$`NRC`
DamIsoF2 <-
Grp<-split(FishIso,f=FishIso$Loc)
Fish<-Grp$``
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

# Mean Values per Location ------------------------------------------------
AvAllc<- aggregate(dC ~ Type + Loc + Site,DamIso,na.rm=TRUE, mean)
SDAllc<- aggregate(dC ~ Type + Loc + Site,DamIso, st.err)
AvBMn<- aggregate(dC ~ Type + Site,BMP,na.rm=TRUE, mean)
SDBMn<- aggregate(dC ~ Type + Site,BMP, st.err)
AvBCn<- aggregate(dC ~ Type + Site,BC,na.rm=TRUE, mean)
SDBCn<- aggregate(dC ~ Type + Site,BC,st.err)
AvNRn<- aggregate(dC ~ Type + Site,NR,na.rm=TRUE, mean)
SdCRn<- aggregate(dC ~ Type + Site,NR,st.err)


AvAlln<- aggregate(dN ~ Type + Loc + Site,DamIso,na.rm=TRUE, mean)
SDAlln<- aggregate(dN ~ Type + Loc + Site,DamIso, st.err)
AvBMc<- aggregate(dN ~ Type + Site,BMP,na.rm=TRUE, mean)
SDBMc<- aggregate(dN ~ Type + Site,BMP,st.err)
AvBCc<- aggregate(dN ~ Type + Site,BC,na.rm=TRUE, mean)
SDBCc<- aggregate(dN ~ Type + Site,BC, st.err)
AvNRc<- aggregate(dN ~ Type + Site,NR,na.rm=TRUE, mean)
SdCRc<- aggregate(dN ~ Type + Site,NR,st.err)

Allcn<-cbind(AvAllc,SDAllc[,4],AvAlln[,4],SDAlln[,4])
colnames(Allcn)<-c("Type","Loc","Site","dC","dCsd","dN","dNsd")
BCcn<-cbind(AvBCc,SDBCc[,3],AvBCn[,3],SDBCn[,3])
BCcn$Loc<-c("BCD")
colnames(BCcn)<-c("Type","Site","dC","dCsd","dN","dNsd","Loc")
BMcn<-cbind(AvBMc,SDBMc[,3],AvBMn[,3],SDBMn[,3])
BMcn$Loc<-c("BMP")
colnames(BMcn)<-c("Type","Site","dC","dCsd","dN","dNsd","Loc")
NRcn<-cbind(AvNRc,SDNRc[,3],AvNRn[,3],SDNRn[,3])
NRcn$Site<-c("NR")
colnames(NRcn)<-c("Group","Type","dC","dCsd","dN","dNsd","Site")

AllcnAVE<-rbind(BCcn,BMcn)
length(BC$Type)

# location stats ----------------------------------------------------------
require(graphics)
Frac<-split(BMP, f=BMP$Type)
Type<-split(BC, f=BC$Type)
BCpp<-rbind(Frac$`Mic`,Frac$`POM`)
BCpp$Frac<-c("PP")
BCmzp<-rbind(Frac$`MES`)
BCmzp$Frac<-c("MeZP")
BClzp<-rbind(Frac$`MAC`)
BClzp$Frac<-c("MaZP")
BMpp<-rbind(Frac$`Mic`,Frac$`POM`)
BMpp$Frac<-c("PP")
BMmzp<-rbind(Frac$`MES`)
BMmzp$Frac<-c("MeZP")
BMlzp<-rbind(Frac$`MAC`)
BMlzp$Frac<-c("MaZP")

Plkn<-rbind(BCpp,BCmzp,BClzp,BMpp,BMmzp,BMlzp)
LocPP<-split(Plkn, f=Plkn$Loc)
BMplkn<-LocPP$`BMP`
BCplkn<-LocPP$`BC`
Ste<-split(BMplkn,f=BMplkn$Type)
Plkn2<-rbind(LocPP$`BCD`,Ste$`DS`)

fit<-aov(dN~Site+Type,data=BC)
summary(fit)
TukeyHSD(fit)
t.test(dN~Site, data=BMP)
fit2<-aov(d15N ~Type, data=Frac$`L`)
summary(fit2)
t.test(d15N~Type, data=NR)


# MDN fish -----------------------------------------------------------
library(ggplot2)
#must run stats first
AllPP<-rbind(BCpp,BMpp)
cbPalette <- c("#56B4E9","#009E73","#E69F00", "#999999",   "#F0E442", "#f9f290",
               "#D55E00", "#d18245","#d1a37f","#0072B2", "#CC79A7","#000000")

fLoc<-split(FishMusc, f=FishMusc$Location)
sz<-split(BCD,f=BCD$Group)
sz2<-split(BMP,f=BMP$Group)
fshd<-rbind(fLoc$`BCD`,fLoc$`BMP`)
ANf<-split(fshd,fshd$Type)
fMDN<-ANf$`AN`
AvfMDN<- aggregate(d13C ~ Location+Time,fMDN,na.rm=TRUE, mean)
SEfMDN<- aggregate(d13C ~ Location+Time,fMDN, st.err)
AvBCpp<- aggregate(d13C ~ Location+Time,sz$`PP`,na.rm=TRUE, mean)
SEBCpp<- aggregate(d13C ~ Location+Time,sz$`PP`, st.err)
AvBMpp<- aggregate(d13C ~ Location+Time+Type,sz2$`PP`,na.rm=TRUE, mean)
SEBMpp<- aggregate(d13C ~ Location+Time+Type,sz2$`PP`, st.err)
fMDNa<-cbind(AvfMDN,SEfMDN$d13C)
fMDNa<-add_column(fMDNa,"Type",.after="Time")
colnames(fMDNa)<-c("Location","Time","Type","dC","dCse")
bcpMDNa<-cbind(AvBCpp,SEBCpp$d13C)
bcpMDNa<-add_column(bcpMDNa,"PP",.after="Time")
colnames(bcpMDNa)<-c("Location","Time","Type","dC","dCse")
bmpMDNa<-cbind(AvBMpp,SEBMpp$d13C)
colnames(bmpMDNa)<-c("Location","Time","Type","dC","dCse")
MDN<-rbind(fMDNa, bcpMDNa, bmpMDNa)

nAvfMDN<- aggregate(d15N ~ Location+Time,fMDN,na.rm=TRUE, mean)
nSEfMDN<- aggregate(d15N ~ Location+Time,fMDN, st.err)
nAvBCpp<- aggregate(d15N ~ Location+Time,sz$`PP`,na.rm=TRUE, mean)
nSEBCpp<- aggregate(d15N ~ Location+Time,sz$`PP`, st.err)
nAvBMpp<- aggregate(d15N ~ Location+Time+Type,sz2$`PP`,na.rm=TRUE, mean)
nSEBMpp<- aggregate(d15N ~ Location+Time+Type,sz2$`PP`, st.err)
nfMDNa<-cbind(nAvfMDN,nSEfMDN$d15N)
nfMDNa<-add_column(nfMDNa,"Type",.after="Time")
colnames(nfMDNa)<-c("Location","Time","Type","dN","dNse")
nbcpMDNa<-cbind(nAvBCpp,nSEBCpp$d15N)
nbcpMDNa<-add_column(nbcpMDNa,"PP",.after="Time")
colnames(nbcpMDNa)<-c("Location","Time","Type","dN","dNse")
nbmpMDNa<-cbind(nAvBMpp,nSEBMpp$d15N)
colnames(nbmpMDNa)<-c("Location","Time","Type","dN","dNse")
nMDN<-rbind(nfMDNa, nbcpMDNa, nbmpMDNa)
allMDN<-cbind(MDN,nMDN$dN,nMDN$dNse)
colnames(allMDN)<-c("Location","Time","Type","dC","dCse","dN","dNse")


# Plotting ----------------------------------------------------------------
pdf("CNdamL.pdf",11,8.5)
CNp1<-ggplot(BCplkn, aes(x=dC, y=dN, shape=Group,
                        color=Site))+  
  geom_point(size=5)+
  geom_errorbarh(data=allMDN, aes(xmax=dC + dCse, xmin=dC-dCse))+
  geom_errorbar(data=allMDN, aes(ymax=dN + dNse, ymin=dN - dNse))+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=c("yellow","green"))+
  scale_shape_manual(values=c(24,25,21,3,8,8,4,4,4,22,23,20))
CNp1+
  theme_bw()+
  xlab(expression(paste(delta ^13,"C")))+
  ylab(expression(paste(delta ^15,"N")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()

pdf("Seasonal Plkdc.pdf",6,3.5)
Cpp1<-ggplot(Plkn2, aes(x=factor(Frac, levels=c("PP","MeZP","MaZP")), y=d13C, 
                                color=factor(Location, levels=c("BCD","BMP")),
                          fill=Time))+
  geom_boxplot()
Cpp1 + theme_bw()+
  ylab(expression(paste(delta ^13,"C")))+
  scale_fill_manual(name="Season",values=c("#4fc27c","#4fbdc2"))+
  scale_color_manual(name="Location",values=c("black","black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(), 
        axis.line = element_line(colour = "black"))
dev.off()

pdf("Seasonal plkdn.pdf",6,3.5)
Npp1<-ggplot(Plkn2, aes(x=factor(Frac, levels=c("PP","MeZP","MaZP")), y=d15N, 
                        color=factor(Location, levels=c("BCD","BMP")),
                        fill=Time))+
  geom_boxplot()
Npp1 + theme_bw()+
  ylab(expression(paste(delta ^15,"N")))+
  scale_fill_manual(name="Season",values=c("#4fc27c","#4fbdc2"))+
  scale_color_manual(name="Location",values=c("black","black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()


# CN plots ----------------------------------------------------------------
Frac<-split(DamIso18, f=DamIso18$Group)
Fish<-rbind(Frac$`ZV`,Frac$`PV`,Frac$`SC`)
Tissue<-split(Fish,f=Fish$Site)
FishLiv<-Tissue$`L`
FishMusc<-rbind(Tissue$`M`, Tissue$`W`)

AvAllc<- aggregate(dC ~ Group + Site,AllcnAVE,na.rm=TRUE, mean)
SDAllc<- aggregate(dC ~ Group + Site,AllcnAVE, st.err)
AvAlln<- aggregate(dN ~ Group + Site,AllcnAVE,na.rm=TRUE, mean)
SDAlln<- aggregate(dN ~ Group + Site,AllcnAVE, st.err)
AllCN<-cbind(AvAllc,SDAllc[,3],AvAlln[,3],SDAlln[,3])
colnames(AllCN)<-c("Group","Site","dC","dCse","dN","dNse")


# food web plots ----------------------------------------------------------

library(ggplot2)
cbPalette <- c("#999999", "#56B4E9","#E69F00",  "#009E73",  "#CC79A7","#D55E00",
               "#0072B2", "#F0E442", "#000000")

#create mean + error data for plankton
Typ<-split(DamIso,f=DamIso$Type)
AvePP<-rbind(Typ$`MIC`,Typ$`POM`)
AvePPc<-aggregate(dC ~ Loc + Site, AvePP, na.rm=TRUE, mean)
AvePPn<-aggregate(dN ~ Loc + Site, AvePP, na.rm=TRUE, mean)
SePPc<-aggregate(dC ~ Loc + Site, AvePP,  st.err)
SePPn<-aggregate(dN ~ Loc + Site, AvePP, st.err)

AveZPc<-aggregate(dC ~ Loc + Site, Typ$`MES`, na.rm=TRUE, mean)
AveZPn<-aggregate(dN ~ Loc + Site, Typ$`MES`, na.rm=TRUE, mean)
SeZPc<-aggregate(dC ~ Loc + Site, Typ$`MES`,  st.err)
SeZPn<-aggregate(dN ~ Loc + Site, Typ$`MES`, st.err)

AveZPMc<-aggregate(dC ~ Loc + Site, Typ$`MAC`, na.rm=TRUE, mean)
AveZPMn<-aggregate(dN ~ Loc + Site, Typ$`MAC`, na.rm=TRUE, mean)
SeZPMc<-aggregate(dC ~ Loc + Site, Typ$`MAC`,  st.err)
SeZPMn<-aggregate(dN ~ Loc + Site, Typ$`MAC`, st.err)

AvePP2<-cbind(AvePPc,AvePPn[,3],SePPc[,3],SePPn[,3])
AvePP2$Type<-c("PP")
colnames(AvePP2)<-c("Loc","Site","dC","dN","dCse","dNse","Type")
AveZP2<-cbind(AveZPc,AveZPn[,3],SeZPc[,3],SeZPn[,3])
colnames(AveZP2)<-c("Loc","Site","dC","dN","dCse","dNse")
AveZP2$Type<-c("MES")
AveZPM<-cbind(AveZPMc,AveZPMn[,3],SeZPMc[,3],SeZPMn[,3])
colnames(AveZPM)<-c("Loc","Site","dC","dN","dCse","dNse")
AveZPM$Type<-c("MAC")

AvePlk<-rbind(AvePP2,AveZP2,AveZPM)
s<-split(AvePlk, f=AvePlk$Loc)
BCPK<-rbind(s$BC,s$MCB)
BMPL<-rbind(s$BMP,s$MCB)

sf<-split(DamIsoF2, f=DamIsoF2$Loc)


png("CNdamFishBM.png",units="in",width=6,height=3, res=300)
CNp1<-ggplot(BMPL, aes(x=dC, y=dN, shape=Type,
                        color=Site))+  
  geom_point(size=3)+
  geom_errorbarh(data=BMPL, aes(xmax=dC + dCse, xmin=dC-dCse))+
  geom_errorbar(data=BMPL, aes(ymax=dN + dNse, ymin=dN - dNse))+
  #scale_color_manual(values=c("#56B4E9","#E69F00","#009E73", "#56B4E9"))
  scale_color_manual(values=c("#56B4E9","#009E73","#E69F00" ))
  #scale_color_manual(values=cbPalette)
  #scale_shape_manual(values=c(21,22,23,95))
CNp1+geom_point(data=sf$BMP, aes(x=dC, y=dN, shape=Type, stroke=2,
                               fill=Site,size=5))+ 
  scale_shape_manual(values=c(24,21,0,5,1,21,23))+
  #scale_shape_manual(values=c(24,25,21,0,5,1,21,23))+
  scale_fill_manual(values=c("#56B4E9","#E69F00"))+
  scale_x_continuous(limits=c(-33,-16))+
  scale_y_continuous(limits=c(5,30))+
  theme_bw()+
  xlab(expression(paste(delta ^13,"C (‰)")))+
  ylab(expression(paste(delta ^15,"N (‰)")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()


# Box Plots ---------------------------------------------------------------
library(ggplot2)
cbPalette <- c("#999999", "#56B4E9","#E69F00",  "#009E73",  "#CC79A7","#D55E00",
               "#0072B2", "#F0E442", "#000000")
pdf("dCfish.pdf",11,8.5)
Cp1<-ggplot(Fish, aes(x=factor(Type, levels=c("CT","AN","SA","IN","R")), y=d13C, 
                        color=factor(Site, levels=c("M","L","W")), 
                      fill=factor(Site, levels=c("M","L","W"))))+  
  geom_boxplot()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)
Cp1+
  theme_bw()+
  ylab(expression(paste(delta ^13,"C")))+
  xlab(expression(paste("")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(), legend.title = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()

pdf("dNfish.pdf",11,8.5)
Np1<-ggplot(Fish, aes(x=factor(Group, levels=c("PV","SC","ZV")), y=d15N, 
                      color=factor(Site, levels=c("M","L","W")), 
                      fill=factor(Type, levels=c("CT","AN","SA","IN","R"))))+  
  geom_boxplot()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)
Np1+
  theme_bw()+
  ylab(expression(paste(delta ^15,"N")))+
  xlab(expression(paste("")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(), legend.title = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()

pdf("dNfish.pdf",11,8.5)
Np1<-ggplot(Fish, aes(x=factor(Group, levels=c("PV","SC","ZV")), y=d15N, 
                      color=factor(Site, levels=c("M","L","W")), 
                      fill=factor(Type, levels=c("CT","AN","SA","IN","R"))))+  
  geom_boxplot()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)
Np1+
  theme_bw()+
  ylab(expression(paste(delta ^15,"N")))+
  xlab(expression(paste("")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(), legend.title = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()


# Diversity ---------------------------------------------------------------
dl<-split(BMP,f=BMP$Site)
Bus<-dl$US
Bds<-dl$DS
Busfdiv<-as.data.frame(table(Bus$Name))
Busfdiv$pi<-Busfdiv$Freq/sum(Busfdiv$Freq)
Busfdiv$ln<-(log(Busfdiv$pi))*Busfdiv$pi 
Busfdiv$Site<-c("US")
Bdsfdiv<-as.data.frame(table(Bds$Name))
Bdsfdiv$pi<-Bdsfdiv$Freq/sum(Bdsfdiv$Freq)
Bdsfdiv$ln<-(log(Bdsfdiv$pi))*Bdsfdiv$pi  
Bdsfdiv$Site<-c("DS")

BMPfdiv<-rbind(Bdsfdiv,Busfdiv)


png("BMPFish Diversity.png",units="in",width=6,height=4, res=300)

p1<-ggplot(BMPfdiv,aes(x=Var1,y=Freq,fill=factor(Site, levels=c("US","DS"))))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=cbPalette)+
  xlab("")+
  ylab("Count")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="bottom",
                   axis.line = element_line(colour = "black"),
                   axis.text.x = element_text(angle = 90, hjust = 1))
p1  
dev.off()  
  
