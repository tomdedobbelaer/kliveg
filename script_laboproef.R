library(plyr)
library(ggplot2)
library(reshape2)
library(plotly)
###instellingen grafieken####
letleg<-1.5 #lettertype legende
br<-800 #breedte grafiek 
le<-800 #lengte grafiek
#datumbereik
startdate<-"2018-03-27 00:00:00 GMT"
stopdate<- "2018-04-23 00:00:00 GMT"
locatie<-file.path ("C:", "git", "kliveg", "plot_output")
#range Permitivity
startperm<-0
stopperm<-60
#range Period
startper<-0
stopper<-5
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

###NPHKp1####
labo_NPHKp1_T1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6105/laboproef_NPHKp1_s6105_GMT/CR200 Series_NPHK_test_p1_1.dat", header=FALSE)
labo_NPHKp1_T1 <- labo_NPHKp1_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_NPHKp1_T1) <- as.character(unlist(labo_NPHKp1_T1[1,])) #oude benaming kolommen losmaken
labo_NPHKp1_T1 <- labo_NPHKp1_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
labo_NPHKp1_T1$EC_1_Avg[labo_NPHKp1_T1$VWC_1_Avg=="NAN"]<-NA 
labo_NPHKp1_T1$Temp_1_Avg[labo_NPHKp1_T1$VWC_1_Avg=="NAN"]<-NA 
labo_NPHKp1_T1$VWC_1_Avg[labo_NPHKp1_T1$VWC_1_Avg=="NAN"]<-NA
write.csv(labo_NPHKp1_T1, file = "labo_NPHKp1_T1.csv")
labo_NPHKp1_T1<-read.csv("C:/git/kliveg/labo_NPHKp1_T1.csv", sep = ",", header = T)


attach(labo_NPHKp1_T1)
labo_NPHKp1_T1$TIMESTAMP <- as.POSIXct(labo_NPHKp1_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")


naam<-"labo_NPHKp1_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
NPHK11<-ggplot(labo_NPHKp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor"))
NPHK11
dev.off()

NPHK11=ggplotly(NPHK11)
NPHK11
naam<-"labo_NPHKp1_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
 dev.off()

naam<-"labo_NPHKp1_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)


ggplot(labo_NPHKp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(5, 25))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_NPHKp1_T1)
#### ####
labo_NPHKp1_T2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6105/laboproef_NPHKp1_s6105_GMT/CR200 Series_NPHK_test_p1_2.dat", header=FALSE)
labo_NPHKp1_T2 <- labo_NPHKp1_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_NPHKp1_T2) <- as.character(unlist(labo_NPHKp1_T2[1,])) #oude benaming kolommen losmaken
labo_NPHKp1_T2 <- labo_NPHKp1_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

write.csv(labo_NPHKp1_T2, file = "labo_NPHKp1_T2.csv")
labo_NPHKp1_T2<-read.csv("C:/git/kliveg/labo_NPHKp1_T2.csv", sep = ",", header = T)


attach(labo_NPHKp1_T2)
labo_NPHKp1_T2$TIMESTAMP <- as.POSIXct(labo_NPHKp1_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_NPHKp1_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp1_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp1_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P1_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P1_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P1_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P1_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_NPHKp1_T2)

 
###NPHKp2#####
labo_NPHKp2_T1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/NPHK_test_p2_1.dat", header=FALSE)
labo_NPHKp2_T1 <- labo_NPHKp2_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_NPHKp2_T1) <- as.character(unlist(labo_NPHKp2_T1[1,])) #oude benaming kolommen losmaken
labo_NPHKp2_T1 <- labo_NPHKp2_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(labo_NPHKp2_T1, file = "labo_NPHKp2_T1.csv")
labo_NPHKp2_T1<-read.csv("C:/git/kliveg/labo_NPHKp2_T1.csv", sep = ",", header = T)

attach(labo_NPHKp2_T1)
labo_NPHKp2_T1$TIMESTAMP <- as.POSIXct(labo_NPHKp2_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")
                                
naam<-"labo_NPHKp2_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P2_H3")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp2_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P2_H3")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp2_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P2_H3")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_NPHKp2_T1)
#### ####
labo_NPHKp2_T2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/NPHK_test_p2_2.dat", header=FALSE)
labo_NPHKp2_T2 <- labo_NPHKp2_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_NPHKp2_T2) <- as.character(unlist(labo_NPHKp2_T2[1,])) #oude benaming kolommen losmaken
labo_NPHKp2_T2 <- labo_NPHKp2_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(labo_NPHKp2_T2, file = "labo_NPHKp2_T2.csv")
labo_NPHKp2_T2<-read.csv("C:/git/kliveg/labo_NPHKp2_T2.csv", sep = ",", header = T)

attach(labo_NPHKp2_T2)
labo_NPHKp2_T2$TIMESTAMP <- as.POSIXct(labo_NPHKp2_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_NPHKp2_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P2_H3")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp2_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P2_H3")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp2_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_NPHKp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P2_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P2_H2")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P2_H3")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_NPHKp2_T2)

###NPHKp3####
 labo_NPHKp3_T1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6107/laboproef nphkp3_s6107_GMT/CR200 Series_NPHK_test_p3_1.dat", header=FALSE)
labo_NPHKp3_T1 <- labo_NPHKp3_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_NPHKp3_T1) <- as.character(unlist(labo_NPHKp3_T1[1,])) #oude benaming kolommen losmaken
labo_NPHKp3_T1 <- labo_NPHKp3_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
##controle op NAN, omzetting naar NA's 
labo_NPHKp3_T1$EC_1_Avg[labo_NPHKp3_T1$VWC_1_Avg=="NAN"]<-NA 
labo_NPHKp3_T1$Temp_1_Avg[labo_NPHKp3_T1$VWC_1_Avg=="NAN"]<-NA 
labo_NPHKp3_T1$VWC_1_Avg[labo_NPHKp3_T1$VWC_1_Avg=="NAN"]<-NA
labo_NPHKp3_T1$`Druk_Avg(2)`[labo_NPHKp3_T1$`Druk_Avg(1)`=="NAN"]<-NA
labo_NPHKp3_T1$`Druk_Avg(1)`[labo_NPHKp3_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(labo_NPHKp3_T1, file = "labo_NPHKp3_T1.csv") #dataset gebruikt door plot
labo_NPHKp3_T1<-read.csv("C:/git/kliveg/labo_NPHKp3_T1.csv", sep = ",", header = T)

attach(labo_NPHKp3_T1)
labo_NPHKp3_T1$TIMESTAMP <- as.POSIXct(labo_NPHKp3_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_NPHKp3_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_NPHKp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp3_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_NPHKp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp3_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_NPHKp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_NPHKp3_T1)
#### ####
labo_NPHKp3_T2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6107/laboproef nphkp3_s6107_GMT/CR200 Series_NPHK_test_p3_2.dat", header=FALSE)
labo_NPHKp3_T2 <- labo_NPHKp3_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_NPHKp3_T2) <- as.character(unlist(labo_NPHKp3_T2[1,])) #oude benaming kolommen losmaken
labo_NPHKp3_T2 <- labo_NPHKp3_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
labo_NPHKp3_T2$Period_1_Avg[labo_NPHKp3_T2$Perm_1_Avg== 0]<-NA 
labo_NPHKp3_T2$VR_1_Avg[labo_NPHKp3_T2$Perm_1_Avg== 0]<-NA 
labo_NPHKp3_T2$Perm_1_Avg[labo_NPHKp3_T2$Perm_1_Avg== 0]<-NA
##einde controle op NAN

write.csv(labo_NPHKp3_T2, file = "labo_NPHKp3_T2.csv") #dataset gebruikt door plot
labo_NPHKp3_T2<-read.csv("C:/git/kliveg/labo_NPHKp3_T2.csv", sep = ",", header = T)

attach(labo_NPHKp3_T2)
labo_NPHKp3_T2$TIMESTAMP <- as.POSIXct(labo_NPHKp3_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_NPHKp3_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_NPHKp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp3_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_NPHKp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_NPHKp3_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_NPHKp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "NPHK_P3_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "NPHK_P3_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "NPHK_P3_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "NPHK_P3_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_NPHKp3_T2)

###DYLp1#####
labo_DYLp1_T1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6104/laboproef_dylp1_s6104_GMT/CR200 Series_DYL_test_p1_1.dat", header=FALSE)
labo_DYLp1_T1 <- labo_DYLp1_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_DYLp1_T1) <- as.character(unlist(labo_DYLp1_T1[1,])) #oude benaming kolommen losmaken
labo_DYLp1_T1 <- labo_DYLp1_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(labo_DYLp1_T1, file = "labo_DYLp1_T1.csv")
labo_DYLp1_T1<-read.table("C:/git/kliveg/labo_DYLp1_T1.csv",sep = ",", header = T)

attach(labo_DYLp1_T1)
labo_DYLp1_T1$TIMESTAMP <- as.POSIXct(labo_DYLp1_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_DYLp1_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "DYL_P1_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "DYL_P1_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "DYL_P1_H3")) +
   coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


naam<-"labo_DYLp1_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "DYL_P1_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "DYL_P1_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "DYL_P1_H3")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


naam<-"labo_DYLp1_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "DYL_P1_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "DYL_P1_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "DYL_P1_H3")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_DYLp1_T1)
#### ####
labo_DYLp1_T2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6104/laboproef_dylp1_s6104_GMT/CR200 Series_DYL_test_p1_2.dat", header=FALSE)
labo_DYLp1_T2 <- labo_DYLp1_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_DYLp1_T2) <- as.character(unlist(labo_DYLp1_T2[1,])) #oude benaming kolommen losmaken
labo_DYLp1_T2 <- labo_DYLp1_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(labo_DYLp1_T2, file = "labo_DYLp1_T2.csv")
labo_DYLp1_T2<-read.table("C:/git/kliveg/labo_DYLp1_T2.csv",sep = ",", header = T)

attach(labo_DYLp1_T2)
labo_DYLp1_T2$TIMESTAMP <- as.POSIXct(labo_DYLp1_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_DYLp1_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "DYL_P1_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "DYL_P1_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "DYL_P1_H3")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


naam<-"labo_DYLp1_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "DYL_P1_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "DYL_P1_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "DYL_P1_H3")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


naam<-"labo_DYLp1_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "DYL_P1_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "DYL_P1_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "DYL_P1_H3")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_DYLp1_T2)

###DYLp2####
labo_DYLp2_T1<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p2_1.dat", header=FALSE)
labo_DYLp2_T1b<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/CR200 Series_DYL_test_p2b_1.dat", header=FALSE)
labo_DYLp2_T1 <-labo_DYLp2_T1 [-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4
labo_DYLp2_T1b <-labo_DYLp2_T1b[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4

colnames(labo_DYLp2_T1) <- c("TIMESTAMP", "RECORD","VWC_4_Avg","EC_4_Avg","Temp_4_Avg","VWC_5_Avg","EC_5_Avg","Temp_5_Avg")
colnames(labo_DYLp2_T1b) <- c("TIMESTAMPbis", "RECORD","VWC_3_Avg","EC_3_Avg","Temp_3_Avg")
write.csv(labo_DYLp2_T1, file = "labo_DYLp2_T1.csv")
write.csv(labo_DYLp2_T1b, file = "labo_DYLp2_T1b.csv")
labo_DYLp2_T1<-read.table("C:/git/kliveg/labo_DYLp2_T1.csv",sep = ",", header = T)
labo_DYLp2_T1b<-read.table("C:/git/kliveg/labo_DYLp2_T1b.csv",sep = ",", header = T)

labo_DYLp2_T1$TIMESTAMP <- as.POSIXct(labo_DYLp2_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")
labo_DYLp2_T1b$TIMESTAMPbis <- as.POSIXct(labo_DYLp2_T1b$TIMESTAMPbis,"%Y-%m-%d %H:%M:%S",tz="GMT")

attach(labo_DYLp2_T1)
attach(labo_DYLp2_T1b)

naam<-"labo_DYLp2_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot() + 
  geom_line(aes(x=TIMESTAMP, y = EC_4_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(x=TIMESTAMP, y = EC_5_Avg, colour = "DYL_P2_H1bis")) +
  geom_line(aes(x=TIMESTAMPbis,y = EC_3_Avg, colour = "DYL_P2_H2")) +
  coord_cartesian(ylim = c(startEC,stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp2_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot() + 
  geom_line(aes(x=TIMESTAMP, y = VWC_4_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(x=TIMESTAMP, y = VWC_5_Avg, colour = "DYL_P2_H1bis")) +
  geom_line(aes(x=TIMESTAMPbis,y = VWC_3_Avg, colour = "DYL_P2_H2")) +
  coord_cartesian(ylim = c(startVWC,stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp2_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot() + 
  geom_line(aes(x=TIMESTAMP, y = Temp_4_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(x=TIMESTAMP, y = Temp_5_Avg, colour = "DYL_P2_H1bis")) +
  geom_line(aes(x=TIMESTAMPbis,y = Temp_3_Avg, colour = "DYL_P2_H2")) +
  coord_cartesian(ylim = c(0,40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_DYLp2_T1)
detach(labo_DYLp2_T1b)
#### ####

labo_DYLp2_T2<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p2_2.dat", header=FALSE)
labo_DYLp2_T2bis<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/CR200 Series_DYL_test_p2b_2.dat", header=FALSE)
labo_DYLp2_T2 <- labo_DYLp2_T2[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4
labo_DYLp2_T2bis <- labo_DYLp2_T2bis[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4

colnames(labo_DYLp2_T2) <- c("TIMESTAMP", "RECORD","Perm_4_Avg","Period_4_Avg","VR_4_Avg","Perm_5_Avg","Period_5_Avg","VR_5_Avg")
colnames(labo_DYLp2_T2bis) <- c("TIMESTAMPbis", "RECORD","Perm_3_Avg","Period_3_Avg","VR_3_Avg")
write.csv(labo_DYLp2_T2, file = "labo_DYLp2_T2.csv")
write.csv(labo_DYLp2_T2bis, file = "labo_DYLp2_T2bis.csv")
labo_DYLp2_T2<-read.table("C:/git/kliveg/labo_DYLp2_T2.csv",sep = ",", header = T)
labo_DYLp2_T2bis<-read.table("C:/git/kliveg/labo_DYLp2_T2bis.csv",sep = ",", header = T)

labo_DYLp2_T2$TIMESTAMP <- as.POSIXct(labo_DYLp2_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")
labo_DYLp2_T2bis$TIMESTAMPbis <- as.POSIXct(labo_DYLp2_T2bis$TIMESTAMPbis,"%Y-%m-%d %H:%M:%S",tz="GMT")

attach(labo_DYLp2_T2)
attach(labo_DYLp2_T2bis)

naam<-"labo_DYLp2_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot() + 
  geom_line(aes(x=TIMESTAMP, y = Period_4_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(x=TIMESTAMP, y = Period_5_Avg, colour = "DYL_P2_H1bis")) +
  geom_line(aes(x=TIMESTAMPbis,y = Period_3_Avg, colour = "DYL_P2_H2")) +
  coord_cartesian(ylim = c(startper,stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp2_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot() + 
  geom_line(aes(x=TIMESTAMP, y = Perm_4_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(x=TIMESTAMP, y = Perm_5_Avg, colour = "DYL_P2_H1bis")) +
  geom_line(aes(x=TIMESTAMPbis,y = Perm_3_Avg, colour = "DYL_P2_H2")) +
  coord_cartesian(ylim = c(startperm,stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp2_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot() + 
  geom_line(aes(x=TIMESTAMP, y = VR_4_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(x=TIMESTAMP, y = VR_5_Avg, colour = "DYL_P2_H1bis")) +
  geom_line(aes(x=TIMESTAMPbis,y = VR_3_Avg, colour = "DYL_P2_H2")) +
  coord_cartesian(ylim = c(startVR,stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

detach(labo_DYLp2_T2)
detach(labo_DYLp2_T2bis)

###DYLp3####
labo_DYLp3_T1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p3_1.dat", header=FALSE)
labo_DYLp3_T1 <- labo_DYLp3_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_DYLp3_T1) <- as.character(unlist(labo_DYLp3_T1[1,])) #oude benaming kolommen losmaken
labo_DYLp3_T1 <- labo_DYLp3_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(labo_DYLp3_T1, file = "labo_DYLp3_T1.csv")
labo_DYLp3_T1<-read.table("C:/git/kliveg/labo_DYLp3_T1.csv",sep = ",", header = T)

attach(labo_DYLp3_T1)
labo_DYLp3_T1$TIMESTAMP <- as.POSIXct(labo_DYLp3_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")
naam<-"labo_DYLp3_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "DYL_P3_H3")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp3_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_DYLp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "DYL_P3_H3")) +
  coord_cartesian(ylim = c(startVWC,stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp3_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_DYLp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "DYL_P3_H3")) +
  coord_cartesian(ylim = c(0,40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


detach(labo_DYLp3_T1)



#### ####
labo_DYLp3_T2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p3_2.dat", header=FALSE)
labo_DYLp3_T2 <- labo_DYLp3_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(labo_DYLp3_T2) <- as.character(unlist(labo_DYLp3_T2[1,])) #oude benaming kolommen losmaken
labo_DYLp3_T2 <- labo_DYLp3_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(labo_DYLp3_T2, file = "labo_DYLp3_T2.csv")
labo_DYLp3_T2<-read.table("C:/git/kliveg/labo_DYLp3_T2.csv",sep = ",", header = T)


attach(labo_DYLp3_T2)
labo_DYLp3_T2$TIMESTAMP <- as.POSIXct(labo_DYLp3_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"labo_DYLp3_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)

ggplot(labo_DYLp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "DYL_P3_H3")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp3_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_DYLp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "DYL_P3_H3")) +
  coord_cartesian(ylim = c(startperm,stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"labo_DYLp3_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
ggplot(labo_DYLp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "DYL_P3_H3")) +
  coord_cartesian(ylim = c(startVR,stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


detach(labo_DYLp3_T2)






