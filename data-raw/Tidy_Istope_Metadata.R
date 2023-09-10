############################################
##tidy the data and put the data in a proper format that could be easily processed
############################################
library("readxl")

#----------
#(1)load the data:
#----------
load.path<-"./data-raw/Data_from_DrAn/"
Iso.dataInfo<-read_xlsx(paste0(load.path,"2023-001_ISONET-Project-Members_18O_ISONET-Sites-Information.xlsx"))
names(Iso.dataInfo)
#------------------
#(2)start to tidy the data:
#------------------
#remove the "Data supplementary material to" in order to well tidy the data:
pos<-match("Data supplementary material to",names(Iso.dataInfo))
Iso.dataInfo<-Iso.dataInfo[,-pos]
#a.remove the observation when there is no "Authors" Info:
Iso.dataInfo_tidy<-Iso.dataInfo[!is.na(Iso.dataInfo$Authors),]
#b.remove duplite variable:"Data sampled"-->pos:23
Iso.dataInfo_tidy<-Iso.dataInfo_tidy[,-23]
#rename the variables to keep the variable names concise:
New_names<-c("Authors","FundingInfo","Sitecode","AlternativeCode","Sitename",
             "Country","Latitude","Longtitude","Altitude","Aspect","Slope",
             "Contact","ORCID","Email","Species","MAT","SampleDate","DataFormats",
             "SamplePeriod","NumLivingTrees","NumAncientWoodSample","NumCores",
             "SampleProcedure","SoilType","ForestStructure","ForestType",
             "MAP","PET","Period_MAPsmallerPET","BAItrend","KoppenClimateType","Notes")
names(Iso.dataInfo_tidy)<-New_names

#---------------
#(3)save the data:
#---------------
save.path<-"./data/"
save(Iso.dataInfo_tidy,file = paste0(save.path,"Iso.dataInfo_tidy.RDA"))
