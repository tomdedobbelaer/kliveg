library(plyr)
library(ggplot2)
library(reshape2)
###instellingen grafieken####
letleg<-1.5 #lettertype legende
br<-800 #breedte grafiek 
le<-800 #lengte grafiek
#datumbereik
startdate<-"2017-12-01 00:00:00 GMT"
stopdate<- "2018-03-28 00:00:00 GMT"
locatie<-file.path ("C:", "git", "kliveg", "plot_output")
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
stopEC<-0.25
interval<-"month"

###veld_NPHKp1_T1####
veld_NPHKp1_T1 <- read.csv("C:/Users/tom_dedobbelaer/Desktop/ruwe data loggers/velddata NPHK_P1_s6XXX_GMT/CR200 Series_NPHK_PUNT1_T1_1_2018-04-17T13-09.dat",header=F)
veld_NPHKp1_T1 <- veld_NPHKp1_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHKp1_T1) <- as.character(unlist(veld_NPHKp1_T1[1,])) #oude benaming kolommen losmaken
veld_NPHKp1_T1 <- veld_NPHKp1_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHKp1_T1$EC_1_Avg[veld_NPHKp1_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$Temp_1_Avg[veld_NPHKp1_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$VWC_1_Avg[veld_NPHKp1_T1$VWC_1_Avg=="NAN"]<-NA
veld_NPHKp1_T1$EC_2_Avg[veld_NPHKp1_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$Temp_2_Avg[veld_NPHKp1_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$VWC_2_Avg[veld_NPHKp1_T1$VWC_2_Avg=="NAN"]<-NA
veld_NPHKp1_T1$EC_3_Avg[veld_NPHKp1_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$Temp_3_Avg[veld_NPHKp1_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$VWC_3_Avg[veld_NPHKp1_T1$VWC_3_Avg=="NAN"]<-NA
veld_NPHKp1_T1$EC_4_Avg[veld_NPHKp1_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$Temp_4_Avg[veld_NPHKp1_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHKp1_T1$VWC_4_Avg[veld_NPHKp1_T1$VWC_4_Avg=="NAN"]<-NA
veld_NPHKp1_T1$`Druk_Avg(2)`[veld_NPHKp1_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_NPHKp1_T1$`Druk_Avg(1)`[veld_NPHKp1_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_NPHKp1_T1, file = "veld_NPHKp1.csv") #dataset gebruikt door plot
veld_NPHKp1_T1<-read.csv("C:/git/kliveg/veld_NPHKp1.csv", sep = ",", header = T)

attach(veld_NPHKp1_T1)
veld_NPHKp1_T1$TIMESTAMP <- as.POSIXct(veld_NPHKp1_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHKp1_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp1_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp1_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(veld_NPHKp1_T1)


###veld_NPHKp1_T2####
veld_NPHKp1_T2 <- read.csv("C:/Users/tom_dedobbelaer/Desktop/ruwe data loggers/velddata NPHK_P1_s6XXX_GMT/CR200 Series_NPHK_PUNT1_T2_2_2018-04-17T13-19.dat",header=F)
veld_NPHKp1_T2 <- veld_NPHKp1_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHKp1_T2) <- as.character(unlist(veld_NPHKp1_T2[1,])) #oude benaming kolommen losmaken
veld_NPHKp1_T2 <- veld_NPHKp1_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHKp1_T2$Perm_1_Avg[veld_NPHKp1_T2$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp1_T2$Period_1_Avg[veld_NPHKp1_T2$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp1_T2$VR_1_Avg[veld_NPHKp1_T2$VWC_1_Avg=="NAN"]<-NA

##einde controle op NAN
write.csv(veld_NPHKp1_T2, file = "veld_NPHKp1_T2.csv") #dataset gebruikt door plot
veld_NPHKp1_T2<-read.csv("C:/git/kliveg/veld_NPHKp1_T2.csv", sep = ",", header = T)

attach(veld_NPHKp1_T2)
veld_NPHKp1_T2$TIMESTAMP <- as.POSIXct(veld_NPHKp1_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHKp1_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp1_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp1_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(veld_NPHKp1_T2)


###veld_NPHKp2_T1####
veld_NPHKp2_T1 <- read.csv("C:/Users/tom_dedobbelaer/Desktop/ruwe data loggers/velddata NPHK_P2_s6XXX_GMT/CR200 Series_NPHK_PUNT2_T1_1_2018-04-17T12-44.dat",header=F)
veld_NPHKp2_T1 <- veld_NPHKp2_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHKp2_T1) <- as.character(unlist(veld_NPHKp2_T1[1,])) #oude benaming kolommen losmaken
veld_NPHKp2_T1 <- veld_NPHKp2_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHKp2_T1$EC_1_Avg[veld_NPHKp2_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$Temp_1_Avg[veld_NPHKp2_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$VWC_1_Avg[veld_NPHKp2_T1$VWC_1_Avg=="NAN"]<-NA
veld_NPHKp2_T1$EC_2_Avg[veld_NPHKp2_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$Temp_2_Avg[veld_NPHKp2_T1$VWC_2_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$VWC_2_Avg[veld_NPHKp2_T1$VWC_2_Avg=="NAN"]<-NA
veld_NPHKp2_T1$EC_3_Avg[veld_NPHKp2_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$Temp_3_Avg[veld_NPHKp2_T1$VWC_3_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$VWC_3_Avg[veld_NPHKp2_T1$VWC_3_Avg=="NAN"]<-NA
veld_NPHKp2_T1$EC_4_Avg[veld_NPHKp2_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$Temp_4_Avg[veld_NPHKp2_T1$VWC_4_Avg=="NAN"]<-NA 
veld_NPHKp2_T1$VWC_4_Avg[veld_NPHKp2_T1$VWC_4_Avg=="NAN"]<-NA
veld_NPHKp2_T1$`Druk_Avg(2)`[veld_NPHKp2_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_NPHKp2_T1$`Druk_Avg(1)`[veld_NPHKp2_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_NPHKp2_T1, file = "veld_NPHKp2.csv") #dataset gebruikt door plot
veld_NPHKp2_T1<-read.csv("C:/git/kliveg/veld_NPHKp2.csv", sep = ",", header = T)

attach(veld_NPHKp2_T1)
veld_NPHKp2_T1$TIMESTAMP <- as.POSIXct(veld_NPHKp2_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHKp2_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp2_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp2_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(veld_NPHKp2_T1)


###veld_NPHKp2_T2####
veld_NPHKp2_T2 <- read.csv("C:/Users/tom_dedobbelaer/Desktop/ruwe data loggers/velddata NPHK_P2_s6XXX_GMT/CR200 Series_NPHK_PUNT2_T2_2_2018-04-17T12-44.dat",header=F)
veld_NPHKp2_T2 <- veld_NPHKp2_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHKp2_T2) <- as.character(unlist(veld_NPHKp2_T2[1,])) #oude benaming kolommen losmaken
veld_NPHKp2_T2 <- veld_NPHKp2_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHKp2_T2$Perm_1_Avg[veld_NPHKp2_T2$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp2_T2$Period_1_Avg[veld_NPHKp2_T2$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp2_T2$VR_1_Avg[veld_NPHKp2_T2$VWC_1_Avg=="NAN"]<-NA

##einde controle op NAN
write.csv(veld_NPHKp2_T2, file = "veld_NPHKp2_T2.csv") #dataset gebruikt door plot
veld_NPHKp2_T2<-read.csv("C:/git/kliveg/veld_NPHKp2_T2.csv", sep = ",", header = T)

attach(veld_NPHKp2_T2)
veld_NPHKp2_T2$TIMESTAMP <- as.POSIXct(veld_NPHKp2_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHKp2_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp2_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp2_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P2_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P2_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(veld_NPHKp2_T2)


###veld_NPHKp3_T1####
veld_NPHKp3_T1 <- read.csv("C:/Users/tom_dedobbelaer/Desktop/ruwe data loggers/velddata NPHK_P3_s6110_GMT/CR200 Series_NPHK_PUNT3_T1_1_2018-04-17T11-06.dat",header=F)
veld_NPHKp3_T1 <- veld_NPHKp3_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHKp3_T1) <- as.character(unlist(veld_NPHKp3_T1[1,])) #oude benaming kolommen losmaken
veld_NPHKp3_T1 <- veld_NPHKp3_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHKp3_T1$EC_1_Avg[veld_NPHKp3_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp3_T1$Temp_1_Avg[veld_NPHKp3_T1$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp3_T1$VWC_1_Avg[veld_NPHKp3_T1$VWC_1_Avg=="NAN"]<-NA
veld_NPHKp3_T1$`Druk_Avg(2)`[veld_NPHKp3_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_NPHKp3_T1$`Druk_Avg(1)`[veld_NPHKp3_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_NPHKp3_T1, file = "veld_NPHKp3_T1.csv") #dataset gebruikt door plot
veld_NPHKp3_T1<-read.csv("C:/git/kliveg/veld_NPHKp3_T1.csv", sep = ",", header = T)

attach(veld_NPHKp3_T1)
veld_NPHKp3_T1$TIMESTAMP <- as.POSIXct(veld_NPHKp3_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHKp3_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp3_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp3_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(veld_NPHKp3_T1)


###veld_NPHKp3_T2####
veld_NPHKp3_T2 <- read.csv("C:/Users/tom_dedobbelaer/Desktop/ruwe data loggers/velddata NPHK_P3_s6110_GMT/CR200 Series_NPHK_PUNT3_T2_2_2018-04-17T11-06.dat",header=F)
veld_NPHKp3_T2 <- veld_NPHKp3_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_NPHKp3_T2) <- as.character(unlist(veld_NPHKp3_T2[1,])) #oude benaming kolommen losmaken
veld_NPHKp3_T2 <- veld_NPHKp3_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_NPHKp3_T2$Perm_1_Avg[veld_NPHKp3_T2$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp3_T2$Period_1_Avg[veld_NPHKp3_T2$VWC_1_Avg=="NAN"]<-NA 
veld_NPHKp3_T2$VR_1_Avg[veld_NPHKp3_T2$VWC_1_Avg=="NAN"]<-NA

##einde controle op NAN
write.csv(veld_NPHKp3_T2, file = "veld_NPHKp3_T2.csv") #dataset gebruikt door plot
veld_NPHKp3_T2<-read.csv("C:/git/kliveg/veld_NPHKp3_T2.csv", sep = ",", header = T)

attach(veld_NPHKp3_T2)
veld_NPHKp3_T2$TIMESTAMP <- as.POSIXct(veld_NPHKp3_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_NPHKp3_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp3_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_NPHKp3_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(veld_NPHKp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(veld_NPHKp3_T2)



