library(RODBC)
ch <- odbcConnect("watina_PRD")
sqlTables(ch)
sqlColumns(ch, "vw_Meetpunt")
sqlQuery(ch, paste("SELECT MeetpuntCode, X,Y FROM vw_Meetpunt" ,
                   "WHERE GebiedNaam = ABE ORDER BY Meetpuntcode"))
dbListTables(ch)
close(ch)
