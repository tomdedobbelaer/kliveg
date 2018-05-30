##Define variables
attach(labo_DYLp3_T1)
attach(labo_DYLp3_T2)
Temp<-Temp_1_Avg
Perm<-Perm_1_Avg
EC<-EC_1_Avg
VWC<-VWC_1_Avg

plot(EC_pore_water()~TIMESTAMP)

EC_sat_extract <- function(Bulk,Dsoli){
  perm_soil_pore_water<-(80.3-0.37*(Temp-20))
  WCS<-(1-Bulk/Dsoli)
  EC_saturation_extract<-((perm_soil_pore_water*VWC*EC)/(WCS*(Perm-4.1)))
  ECpw<-(EC_saturation_extract*(WCS/VWC))
  return(EC_saturation_extract)}


EC_pore_water <- function(){
  perm_soil_pore_water<-(80.3-0.37*(Temp-20))
  EC_pw<-((perm_soil_pore_water*EC)/(Perm-4.1))
  return(EC_pw)}

plot(EC_pore_water()~TIMESTAMP)

plot()
Temp
Perm
perm_soil_pore_water<-(80.3-0.37*(Temp-20))
Temp1<-Temp_1_Avg
Temp1
assign(paste("Temp.", 1,"Avg", sep = "",T))
Temp
Temp<-gsub('"',n,paste("Temp",1, "Avg", sep="_"))
Temp

s = "\"asd  asdds  asdd\""

puts s
"asd  asdds  asdd"

puts s.gsub(/\A"|"\z/,'')
asd  asdds  asdd
