##########################################
##Aim:plot the map of sites
##########################################
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(mapdata)
library(rworldmap)
library(rworldxtra)
library(colorRamps)
library(graphics)
library(jpeg)
#I.load the map
data(coastsCoarse)
#prepration for map
# newmap <- getMap(resolution = "high")[getMap()$ADMIN!='Antarctica',]
newmap <- getMap(resolution = "high")

#---------------------------
#(1)load the sites Info
#---------------------------
sites.path<-"./data/"
load(paste0(sites.path,"Iso.dataInfo_tidy.RDA"))
load(paste0(sites.path,"EC_metaInfo.RDA"))
load(paste0(sites.path,"Allsites_metaInfo.RDA"))
#only using the coordinates to tidy this two datasets:
df.EC<-EC.MetaInfo %>%
  select(c(ICOSID,Sitename,Country,Latitude,Longtitude))%>%
  mutate(ID=ICOSID,ICOSID=NULL,
         Datasource=rep("ICOS-Fluxes",nrow(EC.MetaInfo)))
df.ISO<-Iso.dataInfo_tidy %>%
  select(c(Sitecode,Sitename,Country,Latitude,Longtitude))%>%
  mutate(Latitude=as.numeric(Latitude),
         Longtitude=as.numeric(Longtitude))%>%
  mutate(ID=Sitecode,Sitecode=NULL,
         Datasource=rep("ISONET-Isotope",nrow(Iso.dataInfo_tidy)))
df.Allsites.EC<-Allsites.MetaInfo %>%
  mutate(ID=FluxID,FluxID=NULL,Datasource=rep("Allsites-Fluxes",nrow(Allsites.MetaInfo)))
#---------------------------
#(2)merge data sources
#---------------------------
df.final<-rbind(df.ISO,df.EC)
#bind_rows
df.final<-bind_rows(df.final,df.Allsites.EC)
##set the factors:
df.final$Datasource<-factor(df.final$Datasource,
 levels = c("Allsites-Fluxes","ICOS-Fluxes","ISONET-Isotope"))

#---------------------------------------------
#(3) plotting
#---------------------------------------------
library(RColorBrewer)
library(grDevices)
############
#1. map theme
############
#can refer:http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
##update in 2022,Oct-->using Beni' code (but revised it) to create a empty global map
source("./R/plot_map_simpl_rev_YP_update.R")
lonmin=-35
lonmax=65
latmin=30
latmax=75  ##the latmax should slight >75 as the setting in the function
gg<-plot_map_simpl(lonmin,lonmax,latmin,80)
#---------------------------------------------
# 2. add sites information
#---------------------------------------------
library(ggrepel)  #add the site labels
barwidth = 1.5
barheight = 2

#--------------------------------------------
#figure format:color for different data sources-->
#--------------------------------------------
p_final_1<-gg+
  geom_point(data=df.final,aes(x=Longtitude,y=Latitude,col=Datasource),size=4,pch=16)+
  # scale_color_gradientn(expression ("GPP bias (g m"^-2*"d"^-1*")"),
  #                       colours = c("blue", "white", "red"),
  #                       values = c(0, 0.5, 1))+
  # annotate(geom = "text",x=-160,y=75,label="a",size=6)+
  theme(legend.title = element_text(size=16),
        legend.text = element_text(size=12))+
  scale_color_manual(values = c("ICOS-Fluxes"="red",
                               "ISONET-Isotope"="steelblue2",
                               "Allsites-Fluxes"=adjustcolor("gold",0.3)))
# geom_point(data=final_coord_sites%>% filter(event_perc==0),
#            aes(x=lon,y=lat),size=2,pch=16,col="black")+
# geom_text(data = final_coord_sites %>% filter(sitename=="DK-Sor"),
#           aes(x = lon+8,
#               y = lat + 0.1*event_perc*barheight,
#               label = paste0(event_perc*100," %")
#           ),
#           size = 3,col="orange")+ #only indicate the sites identified with non-overestimation(0%)
# geom_label_repel(data=final_coord_sites,
#                  aes(x=lon,y=lat,label = sitename),col="blue",
#                  label_size=NA,alpha=0.8,label.padding = .1,
#                  max.overlaps = 50,label.size = 0.1,
#                  arrow = arrow(ends = "first",length = unit(0.05,"inch")),
#                  size = 2.8)
#save the plots
save.path<-"./manuscript/figures/"
ggsave(file=paste0(save.path,"Figure1_sites_distribution_fluxandisotope.png"),
       p_final_1,dev="png",width = 8,height=7)
#------------------------
#3. add further information for isotope site-->Dr. An provide information on Oct, 2023
#------------------------
site.info_add<-readxl::read_excel(paste0("./data-raw/Data_from_DrAn/isotope_site_information.xlsx"))
names(site.info_add)<-c("Longtitude","Latitude")
#
lonmin=-180
lonmax=180
latmin=-45
latmax=75  ##the latmax should slight >75 as the setting in the function
gg<-plot_map_simpl(lonmin,lonmax,latmin,80)

p_final_2_world<-gg+
  geom_point(data = site.info_add,aes(x=Longtitude,y=Latitude),col="purple")
p_final_2_europe_Allsites<-p_final_1+
  geom_point(data = site.info_add,aes(x=Longtitude,y=Latitude),
             col="purple")+
  geom_label_repel(data=df.final[df.final$Datasource=="Allsites-Fluxes",],
             aes(x=Longtitude,y=Latitude,label = ID),col="blue",
             label_size=NA,alpha=0.8,label.padding = .1,
             max.overlaps = 50,label.size = 0.1,
             arrow = arrow(ends = "first",length = unit(0.05,"inch")),
             size = 2.8)
ggsave(file=paste0(save.path,"Figure1_add_sites_distribution_fluxandisotope_Allsites-Fluxes-text.png"),
       p_final_2_europe,dev="png",width = 18,height=15)
p_final_2_europe_ICOS<-p_final_1+
  geom_point(data = site.info_add,aes(x=Longtitude,y=Latitude),
             col="purple")+
  geom_label_repel(data=df.final[df.final$Datasource=="ICOS-Fluxes",],
                   aes(x=Longtitude,y=Latitude,label = ID),col="blue",
                   label_size=NA,alpha=0.8,label.padding = .1,
                   max.overlaps = 50,label.size = 0.1,
                   arrow = arrow(ends = "first",length = unit(0.05,"inch")),
                   size = 2.8)
ggsave(file=paste0(save.path,"Figure1_add_sites_distribution_fluxandisotope_ICOS-Fluxes-text.png"),
       p_final_2_europe_ICOS,dev="png",width = 18,height=15)

##################back up code#############################
#merge two plots:
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
#refer the links:
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# p_out<-grid.arrange(p_final,p_bias_PFTs,widths=c(5.8,3),heights=5:1)
