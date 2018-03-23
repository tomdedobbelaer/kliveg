NPHKp1 <- read.csv("C:/git/kliveg/raw data/LOGGER S6105/laboproef nphkp1_s6105_GMT/CR200 Series_NPHK_test_p1_1_2018-03-21T13-49.dat", header=FALSE)
View(NPHKp1)
aNPHKp1 <- NPHKp1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aNPHKp1) <- as.character(unlist(aNPHKp1[1,])) #oude benaming kolommen losmaken
aNPHKp1 <- aNPHKp1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aNPHKp1, file = "start3.csv")
a<-read.table(file.choose(),sep = ",", header = T)
attach(a)
plot(TIMESTAMP,VWC_1_Avg,col="red")
lines(TIMESTAMP,VWC_2_Avg,col="green")
lines(TIMESTAMP,VWC_3_Avg,col="blue")
lines(TIMESTAMP,VWC_4_Avg,col="yellow")

plot(TIMESTAMP,Temp_1_Avg,col="red")
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_3_Avg,col="blue")
lines(TIMESTAMP,Temp_4_Avg,col="yellow")

DYLp1 <- read.csv("C:/git/kliveg/raw data/LOGGER S6104/laboproef_dylp1_s6104_GMT/CR200 Series_DYL_test_p1_1_2018-03-21T13-09.dat", header=FALSE)
View(DYLp1)
aDYLp3 <- DYLp1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aDYLp1) <- as.character(unlist(aDYLp3[1,])) #oude benaming kolommen losmaken
aDYLp3 <- aDYLp1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aDYLp1, file = "dylp1.csv")
out_DYLp1<-read.table(file.choose(),sep = ",", header = T)
attach(out_DYLp1)
detach(a)


plot(TIMESTAMP,VWC_1_Avg,col="red")
lines(TIMESTAMP,VWC_2_Avg,col="green")
plot(TIMESTAMP,VWC_3_Avg,col="blue")
lines(TIMESTAMP,VWC_4_Avg,col="yellow")

plot(TIMESTAMP,Temp_1_Avg,col="red")
lines(TIMESTAMP,Temp_2_Avg,col="green")
lines(TIMESTAMP,Temp_3_Avg,col="blue")
lines(TIMESTAMP,Temp_4_Avg,col="yellow")
