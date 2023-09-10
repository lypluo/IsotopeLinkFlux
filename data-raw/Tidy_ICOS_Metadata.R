############################################
##tidy the Metadata for ICOS EC sites
############################################
library("readxl")
library(dplyr)

#------------------------------
#(1) load the data
#------------------------------
load.path<-"./data-raw/Data_from_ICOS/"
EC.MetaInfo<-read_xlsx(paste0(load.path,"ICOS_forest_stations_Meta.xlsx"))
names(EC.MetaInfo)

#------------------------------
#(2)re-tidy the info:
#------------------------------
##tidy the longtitude and latitude:
geo.pos<-strsplit(EC.MetaInfo$Position," ")
pos<-c()
for (i in 1:length(geo.pos)) {
  temp<-geo.pos[[i]]
  pos<-rbind(pos,temp)
}
pos<-apply(pos, 2,as.numeric)
pos.final<-as.data.frame(pos)
names(pos.final)<-c("Longtitude","Latitude")

##
EC.MetaInfo<-EC.MetaInfo %>%
  mutate(Longtitude=pos.final$Longtitude,
         Latitude=pos.final$Latitude,
         Position=NULL)
##reassign variables new names
New_names<-c("ICOSID","Sitename","Country","PInames","PFT","Elevation",
             "StationClass","LabelDate","Weblink","Longtitude","Latitude")
names(EC.MetaInfo)<-New_names

#---------------
#(3)save the data:
#---------------
save.path<-"./data/"
save(EC.MetaInfo,file = paste0(save.path,"EC_MetaInfo.RDA"))
