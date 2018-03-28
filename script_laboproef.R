library(plyr)
###instellingen grafieken####
letleg<-1.5 #lettertype legende
br<-800 #breedte grafiek 
le<-800 #lengte grafiek
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

#grafieken voor VWC/Temp/EC####
###NPHKp1####
NPHKp1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6105/laboproef_NPHKp1_s6105_GMT/CR200 Series_NPHK_test_p1_1_2018-03-21T13-49.dat", header=FALSE)
aNPHKp1 <- NPHKp1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp1) <- as.character(unlist(aNPHKp1[1,])) #oude benaming kolommen losmaken
aNPHKp1 <- aNPHKp1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

write.csv(aNPHKp1, file = "NPHKp1.csv")
NPHKp1a<-read.csv("C:/git/kliveg/NPHKp1.csv", sep = ",", header = T)


attach(NPHKp1a)
naam<-"NPHKp1_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,EC_1_Avg,ylab='EC',xlab='date',ylim=range(startEC,stopEC))
lines(TIMESTAMP,EC_2_Avg,col="green")
lines(TIMESTAMP,EC_3_Avg,col="blue")
lines(TIMESTAMP,EC_4_Avg,col="red")
legend("topright",c("NPHK_P1_H1", "NPHK_P1_H2", "NPHK_P1_H3","NPHK_P1_H4"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp1_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VWC_1_Avg,ylab='VWC',xlab='date',ylim=range(startVWC,stopVWC))
lines(TIMESTAMP,VWC_2_Avg,col="green")
lines(TIMESTAMP,VWC_3_Avg,col="blue")
lines(TIMESTAMP,VWC_4_Avg,col="red")
legend("topright",c("NPHK_P1_H1", "NPHK_P1_H2", "NPHK_P1_H3","NPHK_P1_H4"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp1_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Temp_1_Avg,ylab='Temp °C',xlab='date',ylim=range(10,25))
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_3_Avg,col="blue")
lines(TIMESTAMP,Temp_4_Avg,col="red")
legend("topright",c("NPHK_P1_H1", "NPHK_P1_H2", "NPHK_P1_H3","NPHK_P1_H4"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

detach(NPHKp1a)

###NPHKp2#####
NPHKp2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/NPHK_test_p2_1_2018-03-21T13-40.dat", header=FALSE)
aNPHKp2 <- NPHKp2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp2) <- as.character(unlist(aNPHKp2[1,])) #oude benaming kolommen losmaken
aNPHKp2 <- aNPHKp2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aNPHKp2, file = "NPHKp2.csv")
NPHKp2a<-read.csv("C:/git/kliveg/NPHKp2.csv", sep = ",", header = T)

attach(NPHKp2a)
naam<-"NPHKp2_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,EC_1_Avg,ylab='EC',xlab='date',ylim=range(startEC,stopEC))
lines(TIMESTAMP,EC_2_Avg,col="green")
lines(TIMESTAMP,EC_4_Avg,col="blue")
legend("topright",c("NPHK_p2_H1", "NPHK_p2_H2", "NPHK_p2_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"NPHKp2_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VWC_1_Avg,ylab='VWC',xlab='date',ylim=range(startVWC,stopVWC))
lines(TIMESTAMP,VWC_2_Avg,col="green")
lines(TIMESTAMP,VWC_4_Avg,col="blue")
legend("topright",c("NPHK_P2_H1", "NPHK_P2_H2", "NPHK_P2_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"NPHKp2_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Temp_1_Avg,ylab='Temp °C',xlab='date',ylim=range(10,25))
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_4_Avg,col="blue")
legend("topright",c("NPHK_P2_H1", "NPHK_P2_H2", "NPHK_P2_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

detach(NPHKp2a)

###NPHKp3####
NPHKp3 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6107/laboproef nphkp3_s6107_GMT/CR200 Series_NPHK_test_p3_1_2018-03-21T13-27.dat", header=FALSE)
aNPHKp3 <- NPHKp3[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp3) <- as.character(unlist(aNPHKp3[1,])) #oude benaming kolommen losmaken
aNPHKp3 <- aNPHKp3[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
##controle op NAN, omzetting naar NA's 
aNPHKp3$EC_1_Avg[aNPHKp3$VWC_1_Avg=="NAN"]<-NA 
aNPHKp3$Temp_1_Avg[aNPHKp3$VWC_1_Avg=="NAN"]<-NA 
aNPHKp3$VWC_1_Avg[aNPHKp3$VWC_1_Avg=="NAN"]<-NA
aNPHKp3$`Druk_Avg(2)`[aNPHKp3$`Druk_Avg(1)`=="NAN"]<-NA
aNPHKp3$`Druk_Avg(1)`[aNPHKp3$`Druk_Avg(1)`=="NAN"]<-NA
##einde controle op NAN
write.csv(aNPHKp3, file = "NPHKp3.csv") #dataset gebruikt door plot
NPHKp3a<-read.csv("C:/git/kliveg/NPHKp3.csv", sep = ",", header = T)

attach(NPHKp3a)
naam<-"NPHKp3_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,EC_1_Avg,ylab='EC',xlab='date',ylim=range(startEC,stopEC))
lines(TIMESTAMP,EC_2_Avg,col="green")
lines(TIMESTAMP,EC_3_Avg,col="blue")
lines(TIMESTAMP,EC_4_Avg,col="red")
legend("topright",c("NPHK_P3_H1", "NPHK_P3_H1bis", "NPHK_P3_H2","NPHK_P3_H3"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp3_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VWC_1_Avg,ylab='VWC',xlab='date',ylim=range(startVWC,stopVWC))
lines(TIMESTAMP,VWC_2_Avg,col="green")
lines(TIMESTAMP,VWC_3_Avg,col="blue")
lines(TIMESTAMP,VWC_4_Avg,col="red")
legend("topright",c("NPHK_P3_H1", "NPHK_P3_H1bis", "NPHK_P3_H2","NPHK_P3_H3"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp3_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Temp_1_Avg,ylab='Temp °C',xlab='date',ylim=range(10,25))
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_3_Avg,col="blue")
lines(TIMESTAMP,Temp_4_Avg,col="red")
legend("topright",c("NPHK_P3_H1", "NPHK_P3_H1bis", "NPHK_P3_H2","NPHK_P3_H3"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

detach(NPHKp3a)

###DYLp1#####
DYLp1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6104/laboproef_dylp1_s6104_GMT/CR200 Series_DYL_test_p1_1_2018-03-21T13-09.dat", header=FALSE)
aDYLp1 <- DYLp1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aDYLp1) <- as.character(unlist(aDYLp1[1,])) #oude benaming kolommen losmaken
aDYLp1 <- aDYLp1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aDYLp1, file = "dylp1.csv")
DYLp1a<-read.table("C:/git/kliveg/dylp1.csv",sep = ",", header = T)

attach(DYLp1a)
naam<-"DYLp1_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,EC_1_Avg, ylim=range(startEC,stopEC),ylab='EC',xlab='date')
lines(TIMESTAMP,EC_2_Avg,col="green")
lines(TIMESTAMP,EC_3_Avg,col="blue")
legend("topright",c("DYL_P1_H1", "DYL_P1_H2", "DYL_P1_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

attach(DYLp1a)
naam<-"DYLp1_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VWC_1_Avg, ylim=range(startVWC,stopVWC),ylab='VWC',xlab='date')
lines(TIMESTAMP,VWC_2_Avg,col="green")
lines(TIMESTAMP,VWC_3_Avg,col="blue")
legend("topright",c("DYL_P1_H1", "DYL_P1_H2", "DYL_P1_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp1_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Temp_1_Avg,ylab='Temp (°C)',xlab='date', ylim=range(10, 25))
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_3_Avg,col="blue")
legend("topright",c("DYL_P1_H1", "DYL_P1_H2", "DYL_P1_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

detach(DYLp1a)

###DYLp2####

DYLp2<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p2_1_2018-03-21T14-10.dat", header=FALSE)
DYLp2bis<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/CR200 Series_DYL_test_p2b_1_2018-03-21T13-40.dat", header=FALSE)
a_DYLp2 <- DYLp2[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4
a_DYLp2bis <- DYLp2bis[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4

colnames(a_DYLp2) <- c("TIMESTAMP", "RECORD","VWC_4_Avg","EC_4_Avg","Temp_4_Avg","VWC_5_Avg","EC_5_Avg","Temp_5_Avg")
colnames(a_DYLp2bis) <- c("TIMESTAMPbis", "RECORD","VWC_3_Avg","EC_3_Avg","Temp_3_Avg")
write.csv(a_DYLp2, file = "dylp2.csv")
write.csv(a_DYLp2bis, file = "dylp2b.csv")
a_dylp2<-read.table("C:/git/kliveg/dylp2.csv",sep = ",", header = T)
a_dylp2b<-read.table("C:/git/kliveg/dylp2b.csv",sep = ",", header = T)

attach(a_dylp2)
attach(a_dylp2b)
naam<-"DYLp2_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,EC_4_Avg, ylim=range(startEC,stopEC),ylab='EC',xlab='date')
lines(TIMESTAMP,EC_5_Avg,col="green")
lines(TIMESTAMPbis,EC_3_Avg,col="blue")
legend("topright",c("DYL_P2_H1", "DYL_P2_H1bis", "DYL_P2_H2"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp2_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VWC_4_Avg, ylim=range(startVWC,stopVWC),ylab='VWC',xlab='date')
lines(TIMESTAMP,VWC_5_Avg,col="green")
lines(TIMESTAMPbis,VWC_3_Avg,col="blue")
legend("topright",c("DYL_P2_H1", "DYL_P2_H1bis", "DYL_P2_H2"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp2_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Temp_4_Avg,ylab='Temp (°C)',xlab='date', ylim=range(10, 25))
lines(TIMESTAMP,Temp_5_Avg,col="green")
lines(TIMESTAMPbis,Temp_3_Avg,col="blue")
legend("topright",c("DYL_P2_H1", "DYL_P2_H1bis", "DYL_P2_H2"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()
detach(a_dylp2)
detach(a_dylp2b)

###DYLp3####
DYLp3 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p3_1_2018-03-21T14-10.dat", header=FALSE)
aDYLp3 <- DYLp3[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aDYLp3) <- as.character(unlist(aDYLp3[1,])) #oude benaming kolommen losmaken
aDYLp3 <- aDYLp3[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aDYLp3, file = "dylp3.csv")
dylp3a<-read.table("C:/git/kliveg/dylp3.csv",sep = ",", header = T)

attach(dylp3a)
naam<-"DYLp3_EC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,EC_1_Avg, ylim=range(startEC,stopEC),ylab='EC',xlab='date')
lines(TIMESTAMP,EC_2_Avg,col="green")
lines(TIMESTAMP,EC_3_Avg,col="blue")
legend("topright",c("DYL_P3_H1", "DYL_P3_H2", "DYL_P3_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp3_VWC.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VWC_1_Avg, ylim=range(startVWC,stopVWC),ylab='VWC',xlab='date')
lines(TIMESTAMP,VWC_2_Avg,col="green")
lines(TIMESTAMP,VWC_3_Avg,col="blue")
legend("topright",c("DYL_P3_H1", "DYL_P3_H2", "DYL_P3_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp3_Temp.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Temp_1_Avg,ylab='Temp (°C)',xlab='date', ylim=range(0, 20))
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_3_Avg,col="blue")
legend("topright",c("DYL_P3_H1", "DYL_P3_H2", "DYL_P3_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

detach(dylp3a)



#grafieken voor Period/VR/Perm####
###NPHKp1####
NPHKp1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6105/laboproef_NPHKp1_s6105_GMT/CR200 Series_NPHK_test_p1_2_2018-03-21T13-49.dat", header=FALSE)
aNPHKp1 <- NPHKp1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp1) <- as.character(unlist(aNPHKp1[1,])) #oude benaming kolommen losmaken
aNPHKp1 <- aNPHKp1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

write.csv(aNPHKp1, file = "NPHKp1.csv")
NPHKp1a<-read.csv("C:/git/kliveg/NPHKp1.csv", sep = ",", header = T)


attach(NPHKp1a)
naam<-"NPHKp1_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Period_1_Avg,ylab='Period',xlab='date',ylim=range(startper,stopper))
lines(TIMESTAMP,Period_2_Avg,col="green")
lines(TIMESTAMP,Period_3_Avg,col="blue")
lines(TIMESTAMP,Period_4_Avg,col="red")
legend("topright",c("NPHK_P1_H1", "NPHK_P1_H2", "NPHK_P1_H3","NPHK_P1_H4"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp1_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Perm_1_Avg,ylab='Perm',xlab='date',ylim=range(startperm,stopperm))
lines(TIMESTAMP,Perm_2_Avg,col="green")
lines(TIMESTAMP,Perm_3_Avg,col="blue")
lines(TIMESTAMP,Perm_4_Avg,col="red")
legend("topright",c("NPHK_P1_H1", "NPHK_P1_H2", "NPHK_P1_H3","NPHK_P1_H4"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp1_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VR_1_Avg,ylab='VR',xlab='date',ylim=range(startVR,stopVR))
lines(TIMESTAMP,VR_2_Avg,col="green")
lines(TIMESTAMP,VR_3_Avg,col="blue")
lines(TIMESTAMP,VR_4_Avg,col="red")
legend("topright",c("NPHK_P1_H1", "NPHK_P1_H2", "NPHK_P1_H3","NPHK_P1_H4"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

detach(NPHKp1a)

###NPHKp2#####
NPHKp2 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/NPHK_test_p2_2_2018-03-21T13-40.dat", header=FALSE)
aNPHKp2 <- NPHKp2[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp2) <- as.character(unlist(aNPHKp2[1,])) #oude benaming kolommen losmaken
aNPHKp2 <- aNPHKp2[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aNPHKp2, file = "NPHKp2.csv")
NPHKp2a<-read.csv("C:/git/kliveg/NPHKp2.csv", sep = ",", header = T)

attach(NPHKp2a)
naam<-"NPHKp2_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Period_1_Avg,ylab='Period',xlab='date',ylim=range(startper,stopper))
lines(TIMESTAMP,Period_2_Avg,col="green")
lines(TIMESTAMP,Period_4_Avg,col="blue")
legend("topright",c("NPHK_p2_H1", "NPHK_p2_H2", "NPHK_p2_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"NPHKp2_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Perm_1_Avg,ylab='Perm',xlab='date',ylim=range(startperm,stopperm))
lines(TIMESTAMP,Perm_2_Avg,col="green")
lines(TIMESTAMP,Perm_4_Avg,col="blue")
legend("topright",c("NPHK_P2_H1", "NPHK_P2_H2", "NPHK_P2_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"NPHKp2_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VR_1_Avg,ylab='VR',xlab='date',ylim=range(startVR,stopVR))
lines(TIMESTAMP,VR_2_Avg,col="green")
lines(TIMESTAMP,VR_4_Avg,col="blue")
legend("topright",c("NPHK_P2_H1", "NPHK_P2_H2", "NPHK_P2_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

detach(NPHKp2a)

###NPHKp3####
NPHKp3 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6107/laboproef nphkp3_s6107_GMT/CR200 Series_NPHK_test_p3_2_2018-03-21T13-27.dat", header=FALSE)
aNPHKp3 <- NPHKp3[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp3) <- as.character(unlist(aNPHKp3[1,])) #oude benaming kolommen losmaken
aNPHKp3 <- aNPHKp3[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)

##controle op NAN, omzetting naar NA's 
aNPHKp3$Period_1_Avg[aNPHKp3$Perm_1_Avg== 0]<-NA 
aNPHKp3$VR_1_Avg[aNPHKp3$Perm_1_Avg== 0]<-NA 
aNPHKp3$Perm_1_Avg[aNPHKp3$Perm_1_Avg== 0]<-NA
##einde controle op NAN

write.csv(aNPHKp3, file = "NPHKp3.csv") #dataset gebruikt door plot
NPHKp3a<-read.csv("C:/git/kliveg/NPHKp3.csv", sep = ",", header = T)

attach(NPHKp3a)
naam<-"NPHKp3_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Period_1_Avg,ylab='Period',xlab='date',ylim=range(startper,stopper))
lines(TIMESTAMP,Period_2_Avg,col="green")
lines(TIMESTAMP,Period_3_Avg,col="blue")
lines(TIMESTAMP,Period_4_Avg,col="red")
legend("topright",c("NPHK_P3_H1", "NPHK_P3_H1bis", "NPHK_P3_H2","NPHK_P3_H3"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp3_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Perm_1_Avg,ylab='Perm',xlab='date',ylim=range(startperm,stopperm))
lines(TIMESTAMP,Perm_2_Avg,col="green")
lines(TIMESTAMP,Perm_3_Avg,col="blue")
lines(TIMESTAMP,Perm_4_Avg,col="red")
legend("topright",c("NPHK_P3_H1", "NPHK_P3_H1bis", "NPHK_P3_H2","NPHK_P3_H3"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

naam<-"NPHKp3_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VR_1_Avg,ylab='VR',xlab='date',ylim=range(startVR,stopVR))
lines(TIMESTAMP,VR_2_Avg,col="green")
lines(TIMESTAMP,VR_3_Avg,col="blue")
lines(TIMESTAMP,VR_4_Avg,col="red")
legend("topright",c("NPHK_P3_H1", "NPHK_P3_H1bis", "NPHK_P3_H2","NPHK_P3_H3"), pch=15,text.col=c("black", "green", "blue","red"),col=c("black", "green", "blue","red"), cex=letleg)
dev.off()

detach(NPHKp3a)

###DYLp1#####
DYLp1 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6104/laboproef_dylp1_s6104_GMT/CR200 Series_DYL_test_p1_2_2018-03-21T13-09.dat", header=FALSE)
aDYLp1 <- DYLp1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aDYLp1) <- as.character(unlist(aDYLp1[1,])) #oude benaming kolommen losmaken
aDYLp1 <- aDYLp1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aDYLp1, file = "dylp1.csv")
DYLp1a<-read.table("C:/git/kliveg/dylp1.csv",sep = ",", header = T)

attach(DYLp1a)
naam<-"DYLp1_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Period_1_Avg, ylim=range(startper,stopper),ylab='Period',xlab='date')
lines(TIMESTAMP,Period_2_Avg,col="green")
lines(TIMESTAMP,Period_3_Avg,col="blue")
legend("topright",c("DYL_P1_H1", "DYL_P1_H2", "DYL_P1_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp1_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Perm_1_Avg, ylim=range(startperm,stopperm),ylab='Perm',xlab='date')
lines(TIMESTAMP,Perm_2_Avg,col="green")
lines(TIMESTAMP,Perm_3_Avg,col="blue")
legend("topright",c("DYL_P1_H1", "DYL_P1_H2", "DYL_P1_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp1_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VR_1_Avg,ylab='VR',xlab='date', ylim=range(startVR,stopVR))
lines(TIMESTAMP,VR_2_Avg,col="green")
lines(TIMESTAMP,VR_3_Avg,col="blue")
legend("topright",c("DYL_P1_H1", "DYL_P1_H2", "DYL_P1_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

detach(DYLp1a)

###DYLp2####

DYLp2<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p2_2_2018-03-21T14-10.dat", header=FALSE)
DYLp2bis<- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6111/laboproef_dylp2-nphk2_s6111_GMT/CR200 Series_DYL_test_p2b_2_2018-03-21T13-40.dat", header=FALSE)
a_DYLp2 <- DYLp2[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4
a_DYLp2bis <- DYLp2bis[-c(1,2,3,4),] #verwijderen van rij 1, 3 en 4

colnames(a_DYLp2) <- c("TIMESTAMP", "RECORD","Perm_4_Avg","Period_4_Avg","VR_4_Avg","Perm_5_Avg","Period_5_Avg","VR_5_Avg")
colnames(a_DYLp2bis) <- c("TIMESTAMPbis", "RECORD","Perm_3_Avg","Period_3_Avg","VR_3_Avg")
write.csv(a_DYLp2, file = "dylp2.csv")
write.csv(a_DYLp2bis, file = "dylp2b.csv")
a_dylp2<-read.table("C:/git/kliveg/dylp2.csv",sep = ",", header = T)
a_dylp2b<-read.table("C:/git/kliveg/dylp2b.csv",sep = ",", header = T)

attach(a_dylp2)
attach(a_dylp2b)
naam<-"DYLp2_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Period_4_Avg, ylim=range(startper,stopper),ylab='Period',xlab='date')
lines(TIMESTAMP,Period_5_Avg,col="green")
lines(TIMESTAMPbis,Period_3_Avg,col="blue")
legend("topright",c("DYL_P2_H1", "DYL_P2_H1bis", "DYL_P2_H2"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp2_Perm.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Perm_4_Avg, ylim=range(startperm,stopperm),ylab='Perm',xlab='date')
lines(TIMESTAMP,Perm_5_Avg,col="green")
lines(TIMESTAMPbis,Perm_3_Avg,col="blue")
legend("topright",c("DYL_P2_H1", "DYL_P2_H1bis", "DYL_P2_H2"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp2_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VR_4_Avg,ylab='VR',xlab='date', ylim=range(startVR,stopVR))
lines(TIMESTAMP,VR_5_Avg,col="green")
lines(TIMESTAMPbis,VR_3_Avg,col="blue")
legend("topright",c("DYL_P2_H1", "DYL_P2_H1bis", "DYL_P2_H2"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()
detach(a_dylp2)
detach(a_dylp2b)

###DYLp3####
DYLp3 <- read.csv("C:/git/kliveg/GMT gecorrigeerde bestanden/LOGGER S6109/laboproef_dylp2_dylp3_s6109_GMT/CR200 Series_DYL_test_p3_2_2018-03-21T14-10.dat", header=FALSE)
aDYLp3 <- DYLp3[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aDYLp3) <- as.character(unlist(aDYLp3[1,])) #oude benaming kolommen losmaken
aDYLp3 <- aDYLp3[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aDYLp3, file = "dylp3.csv")
dylp3a<-read.table("C:/git/kliveg/dylp3.csv",sep = ",", header = T)


attach(dylp3a)
naam<-"DYLp3_Period.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,Period_1_Avg, ylim=range(startper,stopper),ylab='Period',xlab='date')
lines(TIMESTAMP,Period_2_Avg,col="green")
lines(TIMESTAMP,Period_3_Avg,col="blue")
legend("topright",c("DYL_P3_H1", "DYL_P3_H2", "DYL_P3_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

naam<-"DYLp3_VR.bmp"
bmp(file = file.path(locatie,paste(naam)), width = br, height = le)
plot(TIMESTAMP,VR_1_Avg,ylab='VR',xlab='date', ylim=range(startVR,stopVR))
lines(TIMESTAMP,VR_2_Avg,col="green")
lines(TIMESTAMP,VR_3_Avg,col="blue")
legend("topright",c("DYL_P3_H1", "DYL_P3_H2", "DYL_P3_H3"), pch=15,text.col=c("black", "green", "blue"),col=c("black", "green", "blue"), cex=letleg)
dev.off()

detach(dylp3a)
