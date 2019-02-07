library(plyr)
library(ggplot2)
library(reshape2)
theme_update(plot.title = element_text(hjust = 0.5))
###instellingen grafieken####
letleg<-1.5 #lettertype legende
br<-800 #breedte grafiek 
le<-800 #lengte grafiek
#datumbereik
startdate<-"2017-12-01 00:00:00 GMT"
stopdate<- "2018-03-28 00:00:00 GMT"
#range Permitivity
startperm<-0
stopperm<-60
#range Period
startper<-1
stopper<-2.5
#range VR
startVR<-0
stopVR<-2
#range VWC
startVWC<-0
stopVWC<-0.8
#range VWC
startEC<-0
stopEC<-1

interval<-"month"

###veld_NPHK_P1_T1####
veld_NPHK_P1_T1 <- read.csv("./data/data voor verwerking/veld/maasmechelen/S6103/S6103_NPHK_P1_T1_1.dat",header=F)
veld_NPHK_P1_T1 <- veld_NPHK_P1_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHK_P1_T1) <- as.character(unlist(veld_NPHK_P1_T1[1,])) #oude benaming kolommen losmaken
veld_NPHK_P1_T1 <- veld_NPHK_P1_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHK_P1_T1$EC_1_Avg[veld_NPHK_P1_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$Temp_1_Avg[veld_NPHK_P1_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$VWC_1_Avg[veld_NPHK_P1_T1$VWC_1_Avg=="NAN"]<-NA
veld_NPHK_P1_T1$EC_2_Avg[veld_NPHK_P1_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$Temp_2_Avg[veld_NPHK_P1_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$VWC_2_Avg[veld_NPHK_P1_T1$VWC_2_Avg=="NAN"]<-NA
veld_NPHK_P1_T1$EC_3_Avg[veld_NPHK_P1_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$Temp_3_Avg[veld_NPHK_P1_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$VWC_3_Avg[veld_NPHK_P1_T1$VWC_3_Avg=="NAN"]<-NA
veld_NPHK_P1_T1$EC_4_Avg[veld_NPHK_P1_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$Temp_4_Avg[veld_NPHK_P1_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHK_P1_T1$VWC_4_Avg[veld_NPHK_P1_T1$VWC_4_Avg=="NAN"]<-NA
veld_NPHK_P1_T1$`Druk_Avg(2)`[veld_NPHK_P1_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_NPHK_P1_T1$`Druk_Avg(1)`[veld_NPHK_P1_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_NPHK_P1_T1, file = "./file processing/veld_NPHK_P1.csv") #dataset gebruikt door plot
veld_NPHK_P1_T1<-read.csv("./file processing/veld_NPHK_P1.csv", sep = ",", header = T)

veld_NPHK_P1_T1$TIMESTAMP <- as.POSIXct(veld_NPHK_P1_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHK_P1_EC.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK1_EC<-ggplot(veld_NPHK_P1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="EC"))+
  theme(legend.position="left")+
  ggtitle("NPHK.P1.EC")
veldNPHK1_EC
dev.off()

naam<-"veld_NPHK_P1_VWC.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK1_VWC<-ggplot(veld_NPHK_P1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="VWC"))+
  theme(legend.position="left")+
  ggtitle("NPHK.P1.VWC")
  veldNPHK1_VWC
dev.off()

naam<-"veld_NPHK_P1_Temp.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK1_Temp<-ggplot(veld_NPHK_P1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="Temp"))+
  theme(legend.position="left")+
  ggtitle("NPHK.P1.Temp")
veldNPHK1_Temp
dev.off()




###veld_NPHK_P1_T2####
veld_NPHK_P1_T2 <- read.csv("./data/data voor verwerking/veld/maasmechelen/S6103/S6103_NPHK_P1_T2_2.dat",header=F)
veld_NPHK_P1_T2 <- veld_NPHK_P1_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHK_P1_T2) <- as.character(unlist(veld_NPHK_P1_T2[1,])) #oude benaming kolommen losmaken
veld_NPHK_P1_T2 <- veld_NPHK_P1_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHK_P1_T2$Perm_1_Avg[is.na(veld_NPHK_P1_T1$VWC_1_Avg)]<-NA 
veld_NPHK_P1_T2$Period_1_Avg[is.na(veld_NPHK_P1_T1$VWC_1_Avg)]<-NA 
veld_NPHK_P1_T2$VR_1_Avg[is.na(veld_NPHK_P1_T1$VWC_1_Avg)]<-NA
veld_NPHK_P1_T2$Perm_2_Avg[is.na(veld_NPHK_P1_T1$VWC_2_Avg)]<-NA 
veld_NPHK_P1_T2$Period_2_Avg[is.na(veld_NPHK_P1_T1$VWC_2_Avg)]<-NA 
veld_NPHK_P1_T2$VR_2_Avg[is.na(veld_NPHK_P1_T1$VWC_2_Avg)]<-NA
veld_NPHK_P1_T2$Perm_3_Avg[is.na(veld_NPHK_P1_T1$VWC_3_Avg)]<-NA 
veld_NPHK_P1_T2$Period_3_Avg[is.na(veld_NPHK_P1_T1$VWC_3_Avg)]<-NA 
veld_NPHK_P1_T2$VR_3_Avg[is.na(veld_NPHK_P1_T1$VWC_3_Avg)]<-NA
veld_NPHK_P1_T2$Perm_4_Avg[is.na(veld_NPHK_P1_T1$VWC_4_Avg)]<-NA 
veld_NPHK_P1_T2$Period_4_Avg[is.na(veld_NPHK_P1_T1$VWC_4_Avg)]<-NA 
veld_NPHK_P1_T2$VR_4_Avg[is.na(veld_NPHK_P1_T1$VWC_4_Avg)]<-NA
##einde controle op NAN
write.csv(veld_NPHK_P1_T2, file = "./file processing/veld_NPHK_P1_T2.csv") #dataset gebruikt door plot
veld_NPHK_P1_T2<-read.csv("./file processing/veld_NPHK_P1_T2.csv", sep = ",", header = T)


veld_NPHK_P1_T2$TIMESTAMP <- as.POSIXct(veld_NPHK_P1_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHK_P1_Perm.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK1_Perm<-ggplot(veld_NPHK_P1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="Perm"))+
  theme(legend.position="left")+
  ggtitle("NPHK.P1.Perm")
veldNPHK1_Perm
dev.off()

naam<-"veld_NPHK_P1_Period.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK1_Period<-ggplot(veld_NPHK_P1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="Period"))+
  theme(legend.position="left")+
  ggtitle("NPHK.P1.Period")
veldNPHK1_Period
dev.off()

naam<-"veld_NPHK_P1_VR.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK1_VR<-ggplot(veld_NPHK_P1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="VR"))+
  theme(legend.position="left")+
  ggtitle("NPHK.P1.VR")
veldNPHK1_VR
dev.off()


###veld_NPHK_P2_T1####
veld_NPHK_P2_T1 <- read.csv("./data/data voor verwerking/veld/maasmechelen/S6108/S6108_NPHK_PUNT2_T1_1.dat",header=F)
veld_NPHK_P2_T1 <- veld_NPHK_P2_T1[-c(1,3,4,5,6,7,8),] #verwijderen van rij 1, 3 tot 8
colnames(veld_NPHK_P2_T1) <- as.character(unlist(veld_NPHK_P2_T1[1,])) #oude benaming kolommen losmaken
veld_NPHK_P2_T1 <- veld_NPHK_P2_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHK_P2_T1$EC_1_Avg[veld_NPHK_P2_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$Temp_1_Avg[veld_NPHK_P2_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$VWC_1_Avg[veld_NPHK_P2_T1$VWC_1_Avg=="NAN"]<-NA
veld_NPHK_P2_T1$EC_2_Avg[veld_NPHK_P2_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$Temp_2_Avg[veld_NPHK_P2_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$VWC_2_Avg[veld_NPHK_P2_T1$VWC_2_Avg=="NAN"]<-NA
veld_NPHK_P2_T1$EC_3_Avg[veld_NPHK_P2_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$Temp_3_Avg[veld_NPHK_P2_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$VWC_3_Avg[veld_NPHK_P2_T1$VWC_3_Avg=="NAN"]<-NA
veld_NPHK_P2_T1$EC_4_Avg[veld_NPHK_P2_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$Temp_4_Avg[veld_NPHK_P2_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHK_P2_T1$VWC_4_Avg[veld_NPHK_P2_T1$VWC_4_Avg=="NAN"]<-NA
veld_NPHK_P2_T1$`Druk_Avg(2)`[veld_NPHK_P2_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_NPHK_P2_T1$`Druk_Avg(1)`[veld_NPHK_P2_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_NPHK_P2_T1, file = "./file processing/veld_NPHK_P2.csv") #dataset gebruikt door plot
veld_NPHK_P2_T1<-read.csv("./file processing/veld_NPHK_P2.csv", sep = ",", header = T)


veld_NPHK_P2_T1$TIMESTAMP <- as.POSIXct(veld_NPHK_P2_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHK_P2_EC.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK2_EC<-ggplot(veld_NPHK_P2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="NPHK.P2.EC")) 
veldNPHK2_EC
dev.off()

naam<-"veld_NPHK_P2_VWC.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK2_VWC<-ggplot(veld_NPHK_P2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="NPHK.P2.VWC"))
veldNPHK2_VWC
dev.off()

naam<-"veld_NPHK_P2_Temp.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK2_Temp<-ggplot(veld_NPHK_P2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="NPHK.P2.Temp"))
veldNPHK2_Temp
dev.off()

###veld_NPHK_P2_T2####
veld_NPHK_P2_T2 <- read.csv("./data/data voor verwerking/veld/maasmechelen/S6108/S6108_NPHK_PUNT2_T2_2.dat",header=F)
veld_NPHK_P2_T2 <- veld_NPHK_P2_T2[-c(1,3,4,5,6,7,8),] #verwijderen van rij 1, 3 tot 8
colnames(veld_NPHK_P2_T2) <- as.character(unlist(veld_NPHK_P2_T2[1,])) #oude benaming kolommen losmaken
veld_NPHK_P2_T2 <- veld_NPHK_P2_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHK_P2_T2$Perm_1_Avg[is.na(veld_NPHK_P2_T1$VWC_1_Avg)]<-NA 
veld_NPHK_P2_T2$Period_1_Avg[is.na(veld_NPHK_P2_T1$VWC_1_Avg)]<-NA 
veld_NPHK_P2_T2$VR_1_Avg[is.na(veld_NPHK_P2_T1$VWC_1_Avg)]<-NA


##einde controle op NAN
write.csv(veld_NPHK_P2_T2, file = "./file processing/veld_NPHK_P2_T2.csv") #dataset gebruikt door plot
veld_NPHK_P2_T2<-read.csv("./file processing/veld_NPHK_P2_T2.csv", sep = ",", header = T)


veld_NPHK_P2_T2$TIMESTAMP <- as.POSIXct(veld_NPHK_P2_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHK_P2_Perm.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK2_Perm<-ggplot(veld_NPHK_P2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="NPHK.P2.Perm")) 
veldNPHK2_Perm
dev.off()

naam<-"veld_NPHK_P2_Period.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK2_Period<-ggplot(veld_NPHK_P2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="NPHK.P2.Period")) 
veldNPHK2_Period
dev.off()

naam<-"veld_NPHK_P2_VR.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK2_VR<-ggplot(veld_NPHK_P2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="NPHK.P2.VR"))
veldNPHK2_VR
dev.off()




###veld_NPHK_P3_T1####
veld_NPHK_P3_T1 <- read.csv("../kliveg/data/data voor verwerking/veld/maasmechelen/S6110/S6110_NPHK_PUNT3_T1_1.dat",header=F)
veld_NPHK_P3_T1 <- veld_NPHK_P3_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHK_P3_T1) <- as.character(unlist(veld_NPHK_P3_T1[1,])) #oude benaming kolommen losmaken
veld_NPHK_P3_T1 <- veld_NPHK_P3_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHK_P3_T1$EC_1_Avg[veld_NPHK_P3_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHK_P3_T1$Temp_1_Avg[veld_NPHK_P3_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHK_P3_T1$VWC_1_Avg[veld_NPHK_P3_T1$VWC_1_Avg=="NAN"]<-NA
veld_NPHK_P3_T1$`Druk_Avg(2)`[veld_NPHK_P3_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_NPHK_P3_T1$`Druk_Avg(1)`[veld_NPHK_P3_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_NPHK_P3_T1, file = "./file processing/veld_NPHK_P3_T1.csv") #dataset gebruikt door plot
veld_NPHK_P3_T1<-read.csv("./file processing/veld_NPHK_P3_T1.csv", sep = ",", header = T)

veld_NPHK_P3_T1$TIMESTAMP <- as.POSIXct(veld_NPHK_P3_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHK_P3_EC.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK3_EC<-ggplot(veld_NPHK_P3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="NPHK.P3.EC"))
veldNPHK3_EC
dev.off()

naam<-"veld_NPHK_P3_VWC.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK3_VWC<-ggplot(veld_NPHK_P3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="NPHK.P3.VWC"))
veldNPHK3_VWC
dev.off()

naam<-"veld_NPHK_P3_Temp.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK3_Temp<-ggplot(veld_NPHK_P3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="NPHK.P3.Temp"))
veldNPHK3_Temp
dev.off()



###veld_NPHK_P3_T2####
veld_NPHK_P3_T2 <- read.csv("../kliveg/data/data voor verwerking/veld/maasmechelen/S6110/S6110_NPHK_PUNT3_T2_2.dat",header=F)
veld_NPHK_P3_T2 <- veld_NPHK_P3_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHK_P3_T2) <- as.character(unlist(veld_NPHK_P3_T2[1,])) #oude benaming kolommen losmaken
veld_NPHK_P3_T2 <- veld_NPHK_P3_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHK_P3_T2$Perm_1_Avg[is.na(veld_NPHK_P3_T1$VWC_1_Avg)]<-NA 
veld_NPHK_P3_T2$Period_1_Avg[is.na(veld_NPHK_P3_T1$VWC_1_Avg)]<-NA 
veld_NPHK_P3_T2$VR_1_Avg[is.na(veld_NPHK_P3_T1$VWC_1_Avg)]<-NA
veld_NPHK_P3_T2$Perm_2_Avg[is.na(veld_NPHK_P3_T1$VWC_2_Avg)]<-NA 
veld_NPHK_P3_T2$Period_2_Avg[is.na(veld_NPHK_P3_T1$VWC_2_Avg)]<-NA 
veld_NPHK_P3_T2$VR_2_Avg[is.na(veld_NPHK_P3_T1$VWC_2_Avg)]<-NA
veld_NPHK_P3_T2$Perm_3_Avg[is.na(veld_NPHK_P3_T1$VWC_3_Avg)]<-NA 
veld_NPHK_P3_T2$Period_3_Avg[is.na(veld_NPHK_P3_T1$VWC_3_Avg)]<-NA 
veld_NPHK_P3_T2$VR_3_Avg[is.na(veld_NPHK_P3_T1$VWC_3_Avg)]<-NA
veld_NPHK_P3_T2$Perm_4_Avg[is.na(veld_NPHK_P3_T1$VWC_4_Avg)]<-NA 
veld_NPHK_P3_T2$Period_4_Avg[is.na(veld_NPHK_P3_T1$VWC_4_Avg)]<-NA 
veld_NPHK_P3_T2$VR_4_Avg[is.na(veld_NPHK_P3_T1$VWC_4_Avg)]<-NA

##einde controle op NAN
write.csv(veld_NPHK_P3_T2, file = "./file processing/veld_NPHK_P3_T2.csv") #dataset gebruikt door plot
veld_NPHK_P3_T2<-read.csv("./file processing/veld_NPHK_P3_T2.csv", sep = ",", header = T)

veld_NPHK_P3_T2$TIMESTAMP <- as.POSIXct(veld_NPHK_P3_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHK_P3_Perm.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK3_Perm<-ggplot(veld_NPHK_P3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="NPHK.P3.Perm"))
veldNPHK3_Perm
dev.off()

naam<-"veld_NPHK_P3_Period.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK3_Period<-ggplot(veld_NPHK_P3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="NPHK.P3.Period"))
veldNPHK3_Period
dev.off()

naam<-"veld_NPHK_P3_VR.bmp"
bmp(file = file.path("./plot output/maasmechelen",paste(naam)), width = br, height = le)
veldNPHK3_VR<-ggplot(veld_NPHK_P3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="NPHK.P3.VR"))
veldNPHK3_VR
dev.off()







#library(wateRinfo)
#devtools::install_github("inbo/wateRinfo")
