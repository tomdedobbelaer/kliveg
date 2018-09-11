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

###veld_DYLp1_T1####
veld_DYLp1_T1 <- read.csv("./data/data voor verwerking/veld/LOGGER S6112/S6112_DYL_PUNT1_T1.dat",header=F)
veld_DYLp1_T1 <- veld_DYLp1_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_DYLp1_T1) <- as.character(unlist(veld_DYLp1_T1[1,])) #oude benaming kolommen losmaken
veld_DYLp1_T1 <- veld_DYLp1_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_DYLp1_T1$EC_1_Avg[veld_DYLp1_T1$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp1_T1$Temp_1_Avg[veld_DYLp1_T1$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp1_T1$VWC_1_Avg[veld_DYLp1_T1$VWC_1_Avg=="NAN"]<-NA
veld_DYLp1_T1$EC_2_Avg[veld_DYLp1_T1$VWC_2_Avg=="NAN"]<-NA 
veld_DYLp1_T1$Temp_2_Avg[veld_DYLp1_T1$VWC_2_Avg=="NAN"]<-NA 
veld_DYLp1_T1$VWC_2_Avg[veld_DYLp1_T1$VWC_2_Avg=="NAN"]<-NA
veld_DYLp1_T1$EC_3_Avg[veld_DYLp1_T1$VWC_3_Avg=="NAN"]<-NA 
veld_DYLp1_T1$Temp_3_Avg[veld_DYLp1_T1$VWC_3_Avg=="NAN"]<-NA 
veld_DYLp1_T1$VWC_3_Avg[veld_DYLp1_T1$VWC_3_Avg=="NAN"]<-NA
veld_DYLp1_T1$EC_4_Avg[veld_DYLp1_T1$VWC_4_Avg=="NAN"]<-NA 
veld_DYLp1_T1$Temp_4_Avg[veld_DYLp1_T1$VWC_4_Avg=="NAN"]<-NA 
veld_DYLp1_T1$VWC_4_Avg[veld_DYLp1_T1$VWC_4_Avg=="NAN"]<-NA
veld_DYLp1_T1$`Druk_Avg(2)`[veld_DYLp1_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_DYLp1_T1$`Druk_Avg(1)`[veld_DYLp1_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_DYLp1_T1, file = "./file processing/veld_DYLp1.csv") #dataset gebruikt door plot
veld_DYLp1_T1<-read.csv("./file processing/veld_DYLp1.csv", sep = ",", header = T)

veld_DYLp1_T1$TIMESTAMP <- as.POSIXct(veld_DYLp1_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_DYLp1_EC.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp1_VWC.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp1_Temp.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp1_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()




###veld_DYLp1_T2####
veld_DYLp1_T2 <- read.csv("./data/data voor verwerking/veld/LOGGER S6112/S6112_DYL_PUNT1_T2.dat",header=F)
veld_DYLp1_T2 <- veld_DYLp1_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_DYLp1_T2) <- as.character(unlist(veld_DYLp1_T2[1,])) #oude benaming kolommen losmaken
veld_DYLp1_T2 <- veld_DYLp1_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_DYLp1_T2$Perm_1_Avg[veld_DYLp1_T2$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp1_T2$Period_1_Avg[veld_DYLp1_T2$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp1_T2$VR_1_Avg[veld_DYLp1_T2$VWC_1_Avg=="NAN"]<-NA

##einde controle op NAN
write.csv(veld_DYLp1_T2, file = "./file processing/veld_DYLp1_T2.csv") #dataset gebruikt door plot
veld_DYLp1_T2<-read.csv("./file processing/veld_DYLp1_T2.csv", sep = ",", header = T)


veld_DYLp1_T2$TIMESTAMP <- as.POSIXct(veld_DYLp1_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_DYLp1_Perm.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp1_Period.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp1_VR.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp1_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()


###veld_DYLp2_T1####
veld_DYLp2_T1 <- read.csv("./data/data voor verwerking/veld/LOGGER S6108/S6106_DYL_PUNT2_T1.dat",header=F)
veld_DYLp2_T1 <- veld_DYLp2_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_DYLp2_T1) <- as.character(unlist(veld_DYLp2_T1[1,])) #oude benaming kolommen losmaken
veld_DYLp2_T1 <- veld_DYLp2_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_DYLp2_T1$EC_1_Avg[veld_DYLp2_T1$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp2_T1$Temp_1_Avg[veld_DYLp2_T1$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp2_T1$VWC_1_Avg[veld_DYLp2_T1$VWC_1_Avg=="NAN"]<-NA
veld_DYLp2_T1$EC_2_Avg[veld_DYLp2_T1$VWC_2_Avg=="NAN"]<-NA 
veld_DYLp2_T1$Temp_2_Avg[veld_DYLp2_T1$VWC_2_Avg=="NAN"]<-NA 
veld_DYLp2_T1$VWC_2_Avg[veld_DYLp2_T1$VWC_2_Avg=="NAN"]<-NA
veld_DYLp2_T1$EC_3_Avg[veld_DYLp2_T1$VWC_3_Avg=="NAN"]<-NA 
veld_DYLp2_T1$Temp_3_Avg[veld_DYLp2_T1$VWC_3_Avg=="NAN"]<-NA 
veld_DYLp2_T1$VWC_3_Avg[veld_DYLp2_T1$VWC_3_Avg=="NAN"]<-NA
veld_DYLp2_T1$EC_4_Avg[veld_DYLp2_T1$VWC_4_Avg=="NAN"]<-NA 
veld_DYLp2_T1$Temp_4_Avg[veld_DYLp2_T1$VWC_4_Avg=="NAN"]<-NA 
veld_DYLp2_T1$VWC_4_Avg[veld_DYLp2_T1$VWC_4_Avg=="NAN"]<-NA
veld_DYLp2_T1$`Druk_Avg(2)`[veld_DYLp2_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_DYLp2_T1$`Druk_Avg(1)`[veld_DYLp2_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_DYLp2_T1, file = "./file processing/veld_DYLp2.csv") #dataset gebruikt door plot
veld_DYLp2_T1<-read.csv("./file processing/veld_DYLp2.csv", sep = ",", header = T)


veld_DYLp2_T1$TIMESTAMP <- as.POSIXct(veld_DYLp2_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_DYLp2_EC.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp2_VWC.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp2_Temp.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp2_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

###veld_DYLp2_T2####
veld_DYLp2_T2 <- read.csv("./data/data voor verwerking/veld/LOGGER S6112/S6112_DYL_PUNT1_T2.dat",header=F)
veld_DYLp2_T2 <- veld_DYLp2_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_DYLp2_T2) <- as.character(unlist(veld_DYLp2_T2[1,])) #oude benaming kolommen losmaken
veld_DYLp2_T2 <- veld_DYLp2_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_DYLp2_T2$Perm_1_Avg[veld_DYLp2_T2$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp2_T2$Period_1_Avg[veld_DYLp2_T2$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp2_T2$VR_1_Avg[veld_DYLp2_T2$VWC_1_Avg=="NAN"]<-NA

##einde controle op NAN
write.csv(veld_DYLp2_T2, file = "./file processing/veld_DYLp2_T2.csv") #dataset gebruikt door plot
veld_DYLp2_T2<-read.csv("./file processing/veld_DYLp2_T2.csv", sep = ",", header = T)


veld_DYLp2_T2$TIMESTAMP <- as.POSIXct(veld_DYLp2_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_DYLp2_Perm.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp2_Period.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp2_VR.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp2_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "DYL_P2_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "DYL_P2_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "DYL_P2_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "DYL_P2_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()




###veld_DYLp3_T1####
veld_DYLp3_T1 <- read.csv("../kliveg/data/data voor verwerking/veld/LOGGER S6098/S6098_DYL_PUNT3_T1.dat",header=F)
veld_DYLp3_T1 <- veld_DYLp3_T1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_DYLp3_T1) <- as.character(unlist(veld_DYLp3_T1[1,])) #oude benaming kolommen losmaken
veld_DYLp3_T1 <- veld_DYLp3_T1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_DYLp3_T1$EC_1_Avg[veld_DYLp3_T1$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp3_T1$Temp_1_Avg[veld_DYLp3_T1$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp3_T1$VWC_1_Avg[veld_DYLp3_T1$VWC_1_Avg=="NAN"]<-NA
veld_DYLp3_T1$`Druk_Avg(2)`[veld_DYLp3_T1$`Druk_Avg(1)`=="NAN"]<-NA
veld_DYLp3_T1$`Druk_Avg(1)`[veld_DYLp3_T1$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(veld_DYLp3_T1, file = "./file processing/veld_DYLp3_T1.csv") #dataset gebruikt door plot
veld_DYLp3_T1<-read.csv("./file processing/veld_DYLp3_T1.csv", sep = ",", header = T)

veld_DYLp3_T1$TIMESTAMP <- as.POSIXct(veld_DYLp3_T1$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_DYLp3_EC.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = EC_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = EC_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = EC_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = EC_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startEC, stopEC))+
  xlab("date") + ylab("EC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp3_VWC.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = VWC_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = VWC_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = VWC_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = VWC_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startVWC, stopVWC))+
  xlab("date") + ylab("VWC") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp3_Temp.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp3_T1, aes(TIMESTAMP)) + 
  geom_line(aes(y = Temp_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Temp_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Temp_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = Temp_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(0, 40))+
  xlab("date") + ylab("Temp") +
  guides(color=guide_legend(title="sensor")) 
dev.off()



###veld_DYLp3_T2####
veld_DYLp3_T2 <- read.csv("../kliveg/data/data voor verwerking/veld/LOGGER S6098/S6098_DYL_PUNT3_T2.dat",header=F)
veld_DYLp3_T2 <- veld_DYLp3_T2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(veld_DYLp3_T2) <- as.character(unlist(veld_DYLp3_T2[1,])) #oude benaming kolommen losmaken
veld_DYLp3_T2 <- veld_DYLp3_T2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
veld_DYLp3_T2$Perm_1_Avg[veld_DYLp3_T2$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp3_T2$Period_1_Avg[veld_DYLp3_T2$VWC_1_Avg=="NAN"]<-NA 
veld_DYLp3_T2$VR_1_Avg[veld_DYLp3_T2$VWC_1_Avg=="NAN"]<-NA

##einde controle op NAN
write.csv(veld_DYLp3_T2, file = "./file processing/veld_DYLp3_T2.csv") #dataset gebruikt door plot
veld_DYLp3_T2<-read.csv("./file processing/veld_DYLp3_T2.csv", sep = ",", header = T)

veld_DYLp3_T2$TIMESTAMP <- as.POSIXct(veld_DYLp3_T2$TIMESTAMP,"%Y-%m-%d %H:%M:%S",tz="GMT")

naam<-"veld_DYLp3_Perm.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Perm_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Perm_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Perm_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = Perm_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startperm, stopperm))+
  xlab("date") + ylab("Perm") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp3_Period.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = Period_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = Period_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = Period_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = Period_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startper, stopper))+
  xlab("date") + ylab("Period") +
  guides(color=guide_legend(title="sensor")) 
dev.off()

naam<-"veld_DYLp3_VR.bmp"
bmp(file = file.path("./plot output/dijlevallei",paste(naam)), width = br, height = le)
ggplot(veld_DYLp3_T2, aes(TIMESTAMP)) + 
  geom_line(aes(y = VR_1_Avg, colour = "DYL_P3_H1")) + 
  geom_line(aes(y = VR_2_Avg, colour = "DYL_P3_H2")) +
  geom_line(aes(y = VR_3_Avg, colour = "DYL_P3_H3")) +
  geom_line(aes(y = VR_4_Avg, colour = "DYL_P3_H4")) +
  coord_cartesian(ylim = c(startVR, stopVR))+
  xlab("date") + ylab("VR") +
  guides(color=guide_legend(title="sensor")) 
dev.off()







library(wateRinfo)
devtools::install_github("inbo/wateRinfo")
