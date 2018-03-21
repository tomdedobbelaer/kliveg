DYL1 <- read.csv("C:/git/kliveg/raw data/LOGGER S6107/laboproef nphkp3_s6107_GMT/CR200 Series_NPHK_test_p3_1_2018-03-21T13-27.dat", header=FALSE)
View(DYL1)
aDYL1 <- DYL1[-c(1,3,4),] #verwijderen van rij 1, 3 en 4
colnames(aDYL1) <- as.character(unlist(aDYL1[1,])) #oude benaming kolommen losmaken
aDYL1 <- aDYL1[-1, ] # verwijder de eerste rij (header en eerste rij zijn anders hetzelfde)
write.csv(aDYL1, file = "start3.csv")
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
