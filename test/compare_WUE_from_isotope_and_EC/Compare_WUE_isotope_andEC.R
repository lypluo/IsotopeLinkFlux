##--------------------------------------
#Aim: comparing the WUE from istope and EC
##--------------------------------------
#background: I first select several EC sites that with avaiable data,
#and we would like to first evaluate the difference between iWUE from isotope and EC
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
#------------
#(1)load the data:
#------------
#load the isotope data shared by Dr. An:
df.iso<-readxl::read_xlsx(path=paste0("./data-raw/Data_from_DrAn/WUE_from_selected_sites_20231113.xlsx"))
unique(df.iso[,c("Latitude","Longitude")])
#round longitude and latitude to 2 digits
df.iso<-df.iso %>%
  mutate(Latitude=round(Latitude,2),
         Longitude=round(Longitude,2))
#adding the LinkID to indicate which EC site that links to the isotope measurements:
df.iso$LinkID<-rep(NA,nrow(df.iso))
df.iso<-df.iso %>%
  mutate(LinkID = case_when(
    c(Latitude==46.68 & Longitude==10.17)~"CH-Dav",
    c(Latitude==51.51 & Longitude==9.78)~"DE-Hai",
    c(Latitude==60.00 & Longitude==23.08)~"FR-Let",
    c(Latitude==48.38 & Longitude==2.67)~"FR-Fon",
    c(Latitude==46.05 & Longitude==8.77)~"IT-Isp",
    c(Latitude==46.35 & Longitude==8.6)~"FR-Fon"))



#load the selected EC fluxes sites
#-----
#EC site Meteinfo:
#-----
load("./data/EC_MetaInfo.RDA")
#subset the selected sites:
sel.sites<-c("CH-Dav","DE-Hai","FI-Let","FR-Fon","IT-Isp","IT-Ren")
EC.MetaInfo.sel<-EC.MetaInfo %>%
  filter(ICOSID %in% sel.sites)%>%
  select(ICOSID,Sitename,PFT,Longtitude,Latitude)
#load the EC data from select EC sites:
load("./data/Daily_flux_data.RDA")

#-------------------------
#(2)calculating the iWUE from EC sites
#--------------------------
#refering the formulas in paper:
#iWUE=GPP/Gs=GPP/ET*(VPD/Pa)
#https://nph.onlinelibrary.wiley.com/doi/full/10.1111/nph.18649
df.EC<-df_all_sel_daily
df.EC<-df.EC%>%
  mutate(LinkID=sitename,
         Year=year(Date),
         Month=month(Date),
         #defining Growing season from April-September
         GS=ifelse(Month>=4 & Month<=9,"yes","no")
         )%>%
  mutate(iWUE_EC=GPP_NT_mean/ET_mean*VPD_day_mean/PA_mean)

##aggregate the annually iWUE from the EC:
df.EC_annual<-df.EC %>%
  group_by(sitename,Year)%>%
  dplyr::summarise(iWUE_EC_year=mean(iWUE_EC,na.rm=T),
                   #mean iWUE if the data are in growing season
                   iWUE_EC_GS=mean(iWUE_EC[GS=="yes"],na.rm=T))%>%
  mutate(LinkID=sitename)

#----------------------
#(3)compare WUE from two different sources:
#----------------------
df.iso_sel<-df.iso%>%
  select(Year,Latitude,Longitude,iWUE.micromol.mol,LinkID)%>%
  mutate(iWUE_iso=iWUE.micromol.mol,
         iWUE.micromol.mol=NULL)%>%
  mutate(iWUE_iso=ifelse(iWUE_iso=="NA",NA,iWUE_iso))
#merge the data:
df.merge<-left_join(df.EC_annual,df.iso_sel)
#plotting
df.merge %>%
  filter(!is.na(iWUE_iso))%>%
  mutate(iWUE_iso=as.numeric(iWUE_iso))%>%
  ggplot(aes(x=iWUE_iso,y=iWUE_EC_GS,col=LinkID))+
  ylim(0,200)+
  geom_point()

