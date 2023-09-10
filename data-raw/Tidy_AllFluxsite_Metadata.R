############################################
##tidy the Metadata for ICOS EC sites
############################################
library("readxl")
library(dplyr)

#------------------------------
#(1) load the data
#------------------------------
load.path<-"./data-raw/Data_from_All_Fluxsite/"
Allsites.MetaInfo<-read_xlsx(paste0(load.path,"All_Fluxsites.xlsx"))
names(Allsites.MetaInfo)

#------------------------------
#(2)re-tidy the info:
#------------------------------
##tidy the longtitude and latitude:
Allsites.MetaInfo<-as.data.frame(Allsites.MetaInfo)
str(Allsites.MetaInfo)
Allsites.MetaInfo[,2:3]<-apply(Allsites.MetaInfo[,2:3],2,as.numeric)
Allsites.MetaInfo<-Allsites.MetaInfo%>%
    mutate(Latitude=round(Latitude,4),
           Longtitude=round(Longtitude,4))

#---------------
#(3)save the data:
#---------------
save.path<-"./data/"
save(Allsites.MetaInfo,file = paste0(save.path,"Allsites_MetaInfo.RDA"))
