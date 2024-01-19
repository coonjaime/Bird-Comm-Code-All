#BIRD COMMUNITY ANALYSIS - ALL YEARS OMG #
#.....................................................................----
#1. METADATA                                                          ####
#.....................................................................----

#Authors of this code: Jaime Coon.

#Abbreviations defined:
##235 - Kellerton 235
##BSH - Besh Offset
##DUN - Dunn Ranch
##GIL - Gilleland
##KLN - Kellerton North
##KLL - Kell (Prairie Chicken Area)
##KLT - Kellerton Tauke
##LTR - Lee Trail Road
##RC2 - Richardson 2 (Richardson Hay)
##RCH2007 - Richardson from 2007-2013
##RCH2014 - Richardson from 2014-Present
##RNR - Ringgold North Richardson
##RIS - Ringgold South
##RIN - Ringgold North
##RIE - Ringgold East
##PAW - Pawnee Prairie
##PYN - Pyland North
##PYS - Pyland South
##PYW - Pyland West

#.....................................................................----
#2. SETUP                                                          ####
#.....................................................................----
setwd("~/Library/CloudStorage/Dropbox/_Manuscripts/BirdCommCode/Bird-Comm-Code-All")
#install.packages('easypackages')
library('easypackages')
packages('tidyverse','janitor','glmmTMB','ggeffects','TMB','AICcmodavg','readxl','writexl','patchwork')

#ggplot themes
theme_bar_noleg <- function () { 
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        # axis.title.y=element_blank(),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("white"),
        panel.grid=element_blank(),
        legend.position="none",
        legend.text=element_text(size=12, color="black"))}

theme_bar_yaxisnoleg <- function () { 
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="none",
        legend.text=element_text(size=12, color="black"))}

theme_bar_leg <- function () { 
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="right",
        legend.title=element_text(face="bold", size=12),
        legend.text=element_text(size=12, color="black"))}


#.....................................................................----
#3. IMPORTING AND CLEANING DATA                                       ####
#.....................................................................----
#Area surveyd by pasture_patch_year
Patch_Area=read_csv("Patch_Info_1.12.2024.csv")%>%
  clean_names()%>%
  select(area_surveyed,past_pat_year,transect_length)

#Reading in and cleaning the Bird  data - BY TRANSECT VISIT
Trans_Visit=read_xlsx("BirdSurveys.xlsx")%>%
  clean_names()%>%
  mutate(date=ymd(date))%>%
  mutate(julian_date=yday(date))%>%
  mutate(year=lubridate::year(date))%>%
  mutate(start_time=hour(start_time) + (minute(start_time)/60))%>%
  mutate(end_time=hour(end_time) + (minute(end_time)/60))%>%
  mutate(pasture = pasture)%>%
  mutate(pasture=case_when(year>2013 & pasture=="Richardson" ~ "Richardson2014",
                           .default = as.character(pasture)))%>%
  mutate(pasture_id=recode(pasture,
                           "Kellerton 235"                  = "235",
                           "Besh"                           = "BSH",
                           "BESH"                           = "BSH",
                           "Dunn Ranch"                     = "DUN",
                           "Frank North"                    = "FRN",
                           "Frank South"                    = "FRS",
                           "Gilleland"                      = "GIL",
                           "Jerome"                         = "JER",
                           "Kellerton North"                = "KLN",
                           "Kellerton Tauke"                = "KLT",
                           "Lee Trail Rd"                   = "LTR",
                           "Pawnee Prairie"                 = "PAW",
                           "Pyland North"                   = "PYN",
                           "PYLAND North"                   = "PYN",
                           "Pyland South"                   = "PYS",
                           "Pyland West"                    = "PYW",
                           "Richardson"                     = "RCH2007",
                           "Richardson2007"                 = "RCH2007",
                           "Richardson2014"                 = "RCH2014",
                           "Richardson 2"                   = "RC2",
                           "Ringgold East"                  = "RIE",
                           "Ringgold North"                 = "RIN",
                           "RINGGOLD North"                 = "RIN",
                           "Ringgold North Richardson"      = "RNR",
                           "RINGGOLD NORTH Richardson"      = "RNR",
                           "Ringgold South"                 = "RIS",
                           "RINGGOLD South"                 = "RIS",
                           "Sterner"                        = "STE"))%>%
  mutate(patch_id=recode(patch,
                         "Center"               = "C",
                         "East"                 = "E",
                         "North"                = "N",
                         "North1"               = "N",
                         "North2"               = "N",
                         "Northeast"            = "NE",
                         "South"                = "S",
                         "South1"               = "S",
                         "South2"               = "S",
                         "Southeast"            = "SE",
                         "West"                 = "W"))%>%
  
  unite('past_pat_year_visit',pasture_id,patch_id,year,visit,sep="_",remove=FALSE)%>%
  unite('past_pat_year',pasture_id,patch_id,year,sep="_",remove=FALSE)%>%
  filter(!visit>5)%>%
  filter(!past_pat_year=="PYN_E_2017")%>% #removing patches that don't exist (typos)
  filter(!past_pat_year=="PYS_C_2018")%>%
  filter(!past_pat_year=="PYW_W_2007")%>%
  filter(!past_pat_year=="PYW_W_2008")%>%
  filter(!past_pat_year=="PYW_W_2018")%>%
  filter(!past_pat_year=="RIN_N_2007")%>%
  filter(!past_pat_year=="RIN_N_2015")%>%
  filter(!past_pat_year=="RIS_C_NA")%>%
  filter(!past_pat_year=="RIS_N_NA")%>%
  filter(!past_pat_year=="RIS_S_NA")%>%
  filter(!past_pat_year=="STE_C_2017")%>%
  filter(!past_pat_year=="STE_E_2018")%>%
  filter(!past_pat_year=="GIL_W_2022")

#Create a list of patch-visit information from the transect information above
Patch_Visit_Info=Trans_Visit%>%
  group_by(past_pat_year_visit,past_pat_year)%>%
  summarise_at(vars(observer,start_time,clouds,wind, precipitation,cattle_interference,julian_date,year,pasture_id,patch_id,visit),first)%>%
  left_join(Patch_Area,by="past_pat_year")%>%
  filter(!area_surveyed=="NA")

#Extract only the visit numbers -> for use in right joins
#All_Patch_Visits_Surveyed=as.tibble(unique(BirdTrans_Data$past_pat_year_visit))%>%
#  rename("past_pat_year_visit"="value")

#All Bird Data, for use in other pipes - 
All_Birds_By_Visit=read_xlsx("BirdDataSubTable.xlsx")%>%
  clean_names()%>%
  mutate(sex=recode(sex,
                    "1"               = "Male",
                    "2"               = "Female",
                    "3"               = "Male & Female",
                    "4"               = "Unknown"))
  
All_Birds_By_Visit_Filtered=All_Birds_By_Visit%>%
  mutate(species=recode(species,
                        "BEUI"="BEVI",
                        "AMGR"="AMGO",
                        "BEVL"="BEVI",
                        "BRCO"="BHCO",
                        "RNPH"="RNEP",
                        "GPCH"="GRPC",
                        "REBL"="RWBL",
                        "BNSW"="BANS",
                        "BRSW"="BARS",
                        "ROGR"="RBGR",
                        "FISK"="FISP",
                        "EAKE"="EAKI",
                        "UNME"="EAME",
                        "CAGO"="CANG"))%>%
  filter(!species=="UNDU")%>%
  filter(!species=="UNEM")%>%
  filter(!species=="UNFL")%>%
  filter(!species=="UNSP")%>%
  filter(!species=="UNOR")%>%
  filter(!species=="COSP")%>%
  filter(!species=="UNSW")%>%
  filter(!species=="UNMI")%>%
  filter(!species=="UNMF")%>%
  filter(!species=="UNKN")%>%
  filter(!sex=='0') # filter out juveniles (?)
summary(as.factor(All_Birds_By_Visit_Filtered$species))

#totals all birds seen per patch per visit
Tot_Abund_By_Visit=All_Birds_By_Visit%>%
  left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
  select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year)%>% #remove unnecessary information
  right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%  
  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!sex=="Juvenile")%>%
  group_by(past_pat_year_visit)%>%
  summarise_at(vars(group_size),sum)%>%
  rename("Tot_Abund"="group_size")

#totals all OBLIGATE birds seen per patch per visit
Tot_OAbund_By_Visit=All_Birds_By_Visit_Filtered%>%
  left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
  select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year)%>% #remove unnecessary information
  right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%  
  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!sex=="Juvenile")%>%
  mutate(species=as.factor(as.character(species)))%>%
  filter(  species=="GRSP"| species=="HESP"| species=="DICK"| 
             species=="SEWR"| species=="EAME"| species=="WEME"| 
             species=="UNME"| species=="UPSA"| species=="BOBO"|
             species=="GRPC"| species=="LASP"| species=="VESP"|species=="NOHA")%>%
  group_by(past_pat_year_visit)%>%
  summarise_at(vars(group_size),sum)%>%
  rename("Obl_Abund"="group_size")


Tot_OFAbund_By_Visit=All_Birds_By_Visit_Filtered%>%
  left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
  select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year)%>% #remove unnecessary information
  right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%  
  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!sex=="Juvenile")%>%
  mutate(species=as.factor(as.character(species)))%>%
  filter(  species=="GRSP"| species=="HESP"| species=="DICK"| 
             species=="SEWR"| species=="EAME"| species=="WEME"| 
             species=="UNME"| species=="UPSA"| species=="BOBO"|
             species=="GRPC"| species=="LASP"| species=="VESP"|species=="NOHA"|
             
             species=="RWBL"| species=="FISP"| species=="BHCO"|
             species=="COYE"| species=="EAKI"| species=="MODO"| 
             species=="NOBO"| species=="LOSH"| species=="EABL"|
             species=="KILL"| species=="HOLA"| species=="TUVU"| 
             species=="AMKE"| species=="TUVU"| species=="BARS")%>%
  group_by(past_pat_year_visit)%>%
  summarise_at(vars(group_size),sum)%>%
  rename("Obl_Fac_Abund"="group_size")


#Richness
Tot_Rich_By_Visit=All_Birds_By_Visit_Filtered%>%
  left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
  select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year)%>% #remove unnecessary information
  right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!group_size==0)%>%
  filter(!sex=="Juvenile")%>%
  mutate(species=as.factor(as.character(species)))%>%
  group_by(species,past_pat_year_visit)%>%
  count()%>%
  group_by(past_pat_year_visit)%>%
  count()%>%
  rename("Tot_Rich"="n")

#obligate richness
O_Rich_By_Visit=All_Birds_By_Visit_Filtered%>%
  left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
  select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year)%>% #remove unnecessary information
  right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!group_size==0)%>%
  filter(!sex=="Juvenile")%>%
  mutate(species=as.factor(as.character(species)))%>%
  filter(  species=="GRSP"| species=="HESP"| species=="DICK"| 
           species=="SEWR"| species=="EAME"| species=="WEME"| 
           species=="UNME"| species=="UPSA"| species=="BOBO"|species=="SAVS"|
           species=="GRPC"| species=="LASP"| species=="VESP"|species=="NOHA")%>%
  group_by(species,past_pat_year_visit)%>%
  count()%>%
  group_by(past_pat_year_visit)%>%
  count()%>%
  rename("Obl_Rich"="n")

#obligate facultative richness
OF_Rich_By_Visit=All_Birds_By_Visit_Filtered%>%
  left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
  select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year)%>% #remove unnecessary information
  right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!group_size==0)%>%
  filter(!sex=="Juvenile")%>%
  mutate(species=as.factor(as.character(species)))%>%
  filter(    species=="GRSP"| species=="HESP"| species=="DICK"| 
             species=="SEWR"| species=="EAME"| species=="WEME"| 
             species=="UNME"| species=="UPSA"| species=="BOBO"|
             species=="GRPC"| species=="LASP"| species=="VESP"|species=="NOHA"|
             
             species=="RWBL"| species=="FISP"| species=="BHCO"|
             species=="COYE"| species=="EAKI"| species=="MODO"| 
             species=="NOBO"| species=="LOSH"| species=="EABL"|
             species=="KILL"| species=="HOLA"| species=="TUVU"| 
             species=="AMKE"| species=="TUVU"| species=="BARS")%>%
  group_by(species,past_pat_year_visit)%>%
  count()%>%
  group_by(past_pat_year_visit)%>%
  count()%>%
  rename("Obl_Fac_Rich"="n")

#.....................................................................----
#4. MAXIMUM ABUNDANCE                                       ####
#.....................................................................----

#Code for max abundance, x="species", y<maximum group_size#
Max_Abund_Clean <- function(x,y) { 
  All_Birds_By_Visit_Filtered%>%
    left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
    select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year,-patch_id,-pasture_id)%>% #remove unnecessary information
    right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%
    filter(species==x)%>%
    filter(group_size<y)%>%
    group_by(past_pat_year_visit,pasture_id,patch_id,year)%>%
    summarise_at(vars(group_size),sum)%>%
    ungroup()%>%
    group_by(pasture_id,patch_id,year)%>%
    summarise_at(vars(group_size),max)%>%
    unite('past_pat_year',pasture_id,patch_id,year,sep="_",remove=FALSE)%>%
    ungroup()%>%
    select(-pasture_id,-patch_id,-year)
  }

Obl_List=c("BOBO","DICK","EAME","GRSP","HESP","SEWR","NOHA","GRPC","VESP","LASP")%>%
  as_tibble()%>%
  rename(Species=value)%>%
  mutate(Obl_Fac="Obligate")

#obligate max abundance
Max_BOBO=Max_Abund_Clean("BOBO",4)%>%
  rename("Max_BOBO"="group_size")

Max_DICK=Max_Abund_Clean("DICK",4)%>%
  rename("Max_DICK"="group_size")

Max_EAME=Max_Abund_Clean("EAME",4)%>%
  rename("Max_EAME"="group_size")

Max_GRSP=Max_Abund_Clean("GRSP",4)%>%
  rename("Max_GRSP"="group_size")

Max_HESP=Max_Abund_Clean("HESP",4)%>%
  rename("Max_HESP"="group_size")

Max_SEWR=Max_Abund_Clean("SEWR",4)%>%
  rename("Max_SEWR"="group_size")

Max_NOHA=Max_Abund_Clean("NOHA",4)%>%
  rename("Max_NOHA"="group_size")

Max_GRPC=Max_Abund_Clean("GRPC",500)%>%
  rename("Max_GRPC"="group_size")

Max_VESP=Max_Abund_Clean("VESP",4)%>%
  rename("Max_VESP"="group_size")

Max_UPSA=Max_Abund_Clean("UPSA",4)%>%
  rename("Max_UPSA"="group_size")

Max_LASP=Max_Abund_Clean("UPSA",4)%>%
  rename("Max_LASP"="group_size")

Fac_List=c("BHCO","RWBL","COYE","FISP","BARS","EAKI","MODO","NOBO","LOSH","EABL","KILL","HOLA","TUVU","AMKE")%>%
  as_tibble()%>%
  rename(Species=value)%>%
  mutate(Obl_Fac="Facultative")

#facultative max abundance
Max_BHCO=Max_Abund_Clean("BHCO",4)%>%
  rename("Max_BHCO"="group_size")

Max_RWBL=Max_Abund_Clean("RWBL",4)%>%
  rename("Max_RWBL"="group_size")

Max_FISP=Max_Abund_Clean("FISP",4)%>%
  rename("Max_FISP"="group_size")

Max_COYE=Max_Abund_Clean("COYE",4)%>%
  rename("Max_COYE"="group_size")

Max_EAKI=Max_Abund_Clean("EAKI",4)%>%
  rename("Max_EAKI"="group_size")

Max_MODO=Max_Abund_Clean("MODO",4)%>%
  rename("Max_MODO"="group_size")

Max_NOBO=Max_Abund_Clean("NOBO",4)%>%
  rename("Max_NOBO"="group_size")

Max_LOSH=Max_Abund_Clean("LOSH",4)%>%
  rename("Max_LOSH"="group_size")

Max_EABL=Max_Abund_Clean("EABL",4)%>%
  rename("Max_EABL"="group_size")

Max_KILL=Max_Abund_Clean("KILL",4)%>%
  rename("Max_KILL"="group_size")

Max_HOLA=Max_Abund_Clean("HOLA",4)%>%
  rename("Max_HOLA"="group_size")

Max_TUVU=Max_Abund_Clean("TUVU",4)%>%
  rename("Max_TUVU"="group_size")

Max_AMKE=Max_Abund_Clean("AMKE",4)%>%
  rename("Max_AMKE"="group_size")

Max_BARS=Max_Abund_Clean("BARS",4)%>%
  rename("Max_BARS"="group_size")

#Other shrubland birds
Shru_List=c("BEVI","WIFL","GRCA","BRTH","OROR","AMRO","NOMO")%>%
  as_tibble()%>%
  rename(Species=value)%>%
  mutate(Obl_Fac="Other Shrubland")

Max_WIFL=Max_Abund_Clean("WIFL",4)%>%
  rename("Max_WIFL"="group_size")

Max_BEVI=Max_Abund_Clean("BEVI",4)%>%
  rename("Max_BEVI"="group_size")

Max_GRCA=Max_Abund_Clean("GRCA",4)%>%
  rename("Max_GRCA"="group_size")

Max_OROR=Max_Abund_Clean("OROR",4)%>%
  rename("Max_OROR"="group_size")

Max_OROR=Max_Abund_Clean("AMRO",4)%>%
  rename("Max_OROR"="group_size")

Max_NOMO=Max_Abund_Clean("NOMO",4)%>%
  rename("Max_NOMO"="group_size")

Max_BRTH=Max_Abund_Clean("BRTH",4)%>%
  rename("Max_BRTH"="group_size")

Max_AMRO=Max_Abund_Clean("AMRO",4)%>%
  rename("Max_AMRO"="group_size")

#.....................................................................----
#5. BIRDS BY VISIT FOR N-MIX                                      ####
#.....................................................................----

names(All_Birds_By_Visit_Filtered)
#Code for N-mixture models 

Visit_Data_Wide=Patch_Visit_Info%>%
  filter(visit<6)%>%
  filter(!visit==0)%>%
  filter(!visit==10)%>%
  filter(!year=="NA")%>%
  mutate(clouds=recode(clouds,
    "Clear"     = .13,
    "P. Cloudy" = .38,
    "M. Cloudy" = .63,
    "Overcast" = .88))%>%
  mutate(wind=recode(wind,
                       "0-1"     = 1,
                       "1-3"     = 2,
                       "4-7"     = 3,
                       "8-12"    = 4,
                       "13-16"   = 5))%>%
  group_by(pasture_id,patch_id,year,area_surveyed,transect_length)%>%
  pivot_wider(
    id_cols = c(pasture_id,patch_id,year,area_surveyed,transect_length),
    names_from = visit,
    values_from = c(clouds, wind, julian_date,start_time,observer))%>%
  ungroup()%>%
  mutate(across(clouds_1:start_time_5, ~coalesce(.x, round(mean(get(cur_column()), na.rm = TRUE),2))))%>%
  unite('past_pat_year',pasture_id,patch_id,year,sep="_",remove=FALSE)

NMix_Clean <- function(x,y) { 
  All_Birds_By_Visit_Filtered%>%
    left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
    select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year,-patch_id,-pasture_id,-visit)%>% #remove unnecessary information
    filter(species==x)%>%
    filter(group_size<y)%>%
    group_by(past_pat_year_visit)%>%
    summarise_at(vars(group_size),sum)%>%
    right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%
    filter(!visit==0)%>%
    filter(!visit==10)%>%
    mutate(group_size = replace_na(group_size, 0))%>%
    pivot_wider(
      id_cols = c(pasture_id,patch_id,year),
      names_from = visit,
      values_from = group_size)%>%
    unite('past_pat_year',pasture_id,patch_id,year,sep="_",remove=FALSE)%>%
    unite('past_pat',pasture_id,patch_id,sep="_",remove=TRUE)%>%
    select(-year)%>%
    left_join(Visit_Data_Wide,by="past_pat_year")}

#obligate N mixture cleaning
NMix_BOBO=NMix_Clean("BOBO",4)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("BOBO_1"="1",
         "BOBO_2"="2",
         "BOBO_3"="3",
         "BOBO_4"="4",
         "BOBO_5"="5")

NMix_DICK=NMix_Clean("DICK",4)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("DICK_1"="1",
         "DICK_2"="2",
         "DICK_3"="3",
         "DICK_4"="4",
         "DICK_5"="5")

NMix_EAME=NMix_Clean("EAME",4)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("EAME_1"="1",
         "EAME_2"="2",
         "EAME_3"="3",
         "EAME_4"="4",
         "EAME_5"="5")

NMix_GRSP=NMix_Clean("GRSP",4)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("GRSP_1"="1",
         "GRSP_2"="2",
         "GRSP_3"="3",
         "GRSP_4"="4",
         "GRSP_5"="5")

NMix_HESP=NMix_Clean("HESP",4)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("HESP_1"="1",
         "HESP_2"="2",
         "HESP_3"="3",
         "HESP_4"="4",
         "HESP_5"="5")

NMix_SEWR=NMix_Clean("SEWR",4)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("SEWR_1"="1",
         "SEWR_2"="2",
         "SEWR_3"="3",
         "SEWR_4"="4",
         "SEWR_5"="5")

#facultative n mixture cleaning
NMix_BHCO=NMix_Clean("BHCO",500)%>%
  select(past_pat_year,past_pat,pasture_id,patch_id,year,"1","2","3","4","5",clouds_1:observer_5,area_surveyed,transect_length)%>%
  rename("BHCO_1"="1",
         "BHCO_2"="2",
         "BHCO_3"="3",
         "BHCO_4"="4",
         "BHCO_5"="5")

#.....................................................................----
#6. EXPORTING & SAVING DATA                                    ####
#.....................................................................----

#obligate facultative list

Obl_Fac_List=rbind(Obl_List,Fac_List,Shru_List)
write.csv(Obl_Fac_List, "Obl_Fac_List.csv")


#abundance and richness####
Abund_Rich_Final=Tot_Abund_By_Visit%>%
  left_join(Tot_OAbund_By_Visit,  by="past_pat_year_visit")%>%
  left_join(Tot_OFAbund_By_Visit, by="past_pat_year_visit")%>%
  left_join(Tot_Rich_By_Visit,    by="past_pat_year_visit")%>%
  left_join(O_Rich_By_Visit,      by="past_pat_year_visit")%>%
  left_join(OF_Rich_By_Visit,     by="past_pat_year_visit")%>%
  left_join(Patch_Visit_Info,     by="past_pat_year_visit")
  
write.csv(Abund_Rich_Final, "Abund_Rich_Final.csv")

#maximum abundance####
Max_Abund_Final=Patch_Visit_Info%>%
  ungroup()%>%
  select(  past_pat_year,year,pasture_id,area_surveyed,transect_length)%>%
  group_by(past_pat_year,year,pasture_id,area_surveyed,transect_length)%>%
  count()%>%
  select(-n)%>%
  left_join(Max_BOBO,          by="past_pat_year")%>%
  left_join(Max_DICK,          by="past_pat_year")%>%
  left_join(Max_EAME,          by="past_pat_year")%>%
  left_join(Max_GRSP,          by="past_pat_year")%>%
  left_join(Max_HESP,          by="past_pat_year")%>%
  left_join(Max_SEWR,          by="past_pat_year")%>%
  #left_join(Max_NOHA,          by="past_pat_year")%>%
  left_join(Max_GRPC,          by="past_pat_year")%>%
  left_join(Max_VESP,          by="past_pat_year")%>%
  left_join(Max_LASP,          by="past_pat_year")%>%
  left_join(Max_UPSA,          by="past_pat_year")%>%
  
  left_join(Max_BHCO,          by="past_pat_year")%>%
  left_join(Max_RWBL,          by="past_pat_year")%>%
  left_join(Max_COYE,          by="past_pat_year")%>%
  left_join(Max_FISP,          by="past_pat_year")%>%
  left_join(Max_BARS,          by="past_pat_year")%>%
  left_join(Max_EAKI,          by="past_pat_year")%>%
  left_join(Max_MODO,          by="past_pat_year")%>%
  left_join(Max_NOBO,          by="past_pat_year")%>%
  left_join(Max_LOSH,          by="past_pat_year")%>%
  left_join(Max_EABL,          by="past_pat_year")%>%
  left_join(Max_KILL,          by="past_pat_year")%>%
  left_join(Max_HOLA,          by="past_pat_year")%>%
  left_join(Max_TUVU,          by="past_pat_year")%>%
  left_join(Max_AMKE,          by="past_pat_year")%>%
  
  left_join(Max_BEVI,          by="past_pat_year")%>%
  left_join(Max_WIFL,          by="past_pat_year")%>%
  left_join(Max_GRCA,          by="past_pat_year")%>%
  left_join(Max_BRTH,          by="past_pat_year")%>%
  left_join(Max_OROR,          by="past_pat_year")%>%
  left_join(Max_AMRO,          by="past_pat_year")%>%
  left_join(Max_NOMO,          by="past_pat_year")%>%
  mutate(across(Max_BOBO:Max_NOMO, ~replace_na(.x, 0)))

write.csv(Max_Abund_Final, "Max_Abund_Final_Patch.csv")

#N Mixture####
write.csv(NMix_BOBO, "NMix_BOBO.csv")
write.csv(NMix_DICK, "NMix_DICK.csv")
write.csv(NMix_EAME, "NMix_EAME.csv")
write.csv(NMix_GRSP, "NMix_GRSP.csv")
write.csv(NMix_HESP, "NMix_HESP.csv")
write.csv(NMix_SEWR, "NMix_SEWR.csv")
write.csv(NMix_BHCO, "NMix_BHCO.csv")

#.....................................................................----
#7. DATA FOR DNR REPORT - MAX ABUND PASTURE SCALE                                ####
#.....................................................................----


#Code for max abundance, x="species", y<maximum group_size#
Max_Abund_Clean_Pasture <- function(x,y) { 
  All_Birds_By_Visit_Filtered%>%
    left_join(Trans_Visit,by="bird_trans_id")%>% #first link to get the information on each transect
    select(-observer,-start_time,-clouds,wind, -precipitation,-cattle_interference,-julian_date,-year,-patch_id,-pasture_id,-visit)%>% #remove unnecessary information
    right_join(Patch_Visit_Info,by="past_pat_year_visit")%>%
    filter(species==x)%>%
    filter(group_size<y)%>%
    group_by(pasture_id,year,visit)%>%
    summarise_at(vars(group_size),sum)%>%
    ungroup()%>%
    group_by(pasture_id,year)%>%
    summarise_at(vars(group_size),max)%>%
    unite('past_year',pasture_id,year,sep="_",remove=FALSE)%>%
    ungroup()%>%
    select(-pasture_id,-year)}

Obl_List=c("BOBO","DICK","EAME","GRSP","HESP","SEWR","NOHA","GRPC","VESP","LASP","UPSA")%>%
  as_tibble()%>%
  rename(Species=value)%>%
  mutate(Obl_Fac="Obligate")

#obligate max abundance
Max_BOBO_Pasture=Max_Abund_Clean_Pasture("BOBO",4)%>%
  rename("Max_BOBO"="group_size")

Max_DICK_Pasture=Max_Abund_Clean_Pasture("DICK",4)%>%
  rename("Max_DICK"="group_size")

Max_EAME_Pasture=Max_Abund_Clean_Pasture("EAME",4)%>%
  rename("Max_EAME"="group_size")

Max_GRSP_Pasture=Max_Abund_Clean_Pasture("GRSP",4)%>%
  rename("Max_GRSP"="group_size")

Max_HESP_Pasture=Max_Abund_Clean_Pasture("HESP",4)%>%
  rename("Max_HESP"="group_size")

Max_SEWR_Pasture=Max_Abund_Clean_Pasture("SEWR",4)%>%
  rename("Max_SEWR"="group_size")

Max_NOHA_Pasture=Max_Abund_Clean_Pasture("NOHA",4)%>%
  rename("Max_NOHA"="group_size")

Max_GRPC_Pasture=Max_Abund_Clean_Pasture("GRPC",500)%>%
  rename("Max_GRPC"="group_size")

Max_VESP_Pasture=Max_Abund_Clean_Pasture("VESP",4)%>%
  rename("Max_VESP"="group_size")

Max_LASP_Pasture=Max_Abund_Clean_Pasture("LASP",4)%>%
  rename("Max_LASP"="group_size")

Max_UPSA_Pasture=Max_Abund_Clean_Pasture("UPSA",4)%>%
  rename("Max_UPSA"="group_size")

Fac_List=c("BHCO","RWBL","COYE","FISP","BARS","EAKI","MODO","NOBO","LOSH","EABL","KILL","HOLA","TUVU","AMKE")%>%
  as_tibble()%>%
  rename(Species=value)%>%
  mutate(Obl_Fac="Facultative")

#facultative max abundance
Max_BHCO_Pasture=Max_Abund_Clean_Pasture("BHCO",4)%>%
  rename("Max_BHCO"="group_size")

Max_RWBL_Pasture=Max_Abund_Clean_Pasture("RWBL",4)%>%
  rename("Max_RWBL"="group_size")

Max_FISP_Pasture=Max_Abund_Clean_Pasture("FISP",4)%>%
  rename("Max_FISP"="group_size")

Max_COYE_Pasture=Max_Abund_Clean_Pasture("COYE",4)%>%
  rename("Max_COYE"="group_size")

Max_EAKI_Pasture=Max_Abund_Clean_Pasture("EAKI",4)%>%
  rename("Max_EAKI"="group_size")

Max_MODO_Pasture=Max_Abund_Clean_Pasture("MODO",4)%>%
  rename("Max_MODO"="group_size")

Max_NOBO_Pasture=Max_Abund_Clean_Pasture("NOBO",4)%>%
  rename("Max_NOBO"="group_size")

Max_LOSH_Pasture=Max_Abund_Clean_Pasture("LOSH",4)%>%
  rename("Max_LOSH"="group_size")

Max_EABL_Pasture=Max_Abund_Clean_Pasture("EABL",4)%>%
  rename("Max_EABL"="group_size")

Max_KILL_Pasture=Max_Abund_Clean_Pasture("KILL",4)%>%
  rename("Max_KILL"="group_size")

Max_HOLA_Pasture=Max_Abund_Clean_Pasture("HOLA",4)%>%
  rename("Max_HOLA"="group_size")

Max_TUVU_Pasture=Max_Abund_Clean_Pasture("TUVU",4)%>%
  rename("Max_TUVU"="group_size")

Max_AMKE_Pasture=Max_Abund_Clean_Pasture("AMKE",4)%>%
  rename("Max_AMKE"="group_size")

Max_BARS_Pasture=Max_Abund_Clean_Pasture("BARS",4)%>%
  rename("Max_BARS"="group_size")

#Other shrubland birds
Shru_List=c("BEVI","WIFL","GRCA","BRTH","OROR","AMRO","NOMO")%>%
  as_tibble()%>%
  rename(Species=value)%>%
  mutate(Obl_Fac="Other Shrubland")

Max_WIFL_Pasture=Max_Abund_Clean_Pasture("WIFL",4)%>%
  rename("Max_WIFL"="group_size")

Max_BEVI_Pasture=Max_Abund_Clean_Pasture("BEVI",4)%>%
  rename("Max_BEVI"="group_size")

Max_GRCA_Pasture=Max_Abund_Clean_Pasture("GRCA",4)%>%
  rename("Max_GRCA"="group_size")

Max_OROR_Pasture=Max_Abund_Clean_Pasture("OROR",4)%>%
  rename("Max_OROR"="group_size")

Max_OROR_Pasture=Max_Abund_Clean_Pasture("AMRO",4)%>%
  rename("Max_OROR"="group_size")

Max_NOMO_Pasture=Max_Abund_Clean_Pasture("NOMO",4)%>%
  rename("Max_NOMO"="group_size")

Max_BRTH_Pasture=Max_Abund_Clean_Pasture("BRTH",4)%>%
  rename("Max_BRTH"="group_size")

Max_AMRO_Pasture=Max_Abund_Clean_Pasture("AMRO",4)%>%
  rename("Max_AMRO"="group_size")

#savingmaximum abundance by pasture####
Max_Abund_Final_Pasture=Patch_Visit_Info%>%
  ungroup()%>%
  filter(visit==1)%>%
  select(year,pasture_id,transect_length,area_surveyed)%>%
  group_by(year,pasture_id)%>%
  summarise_at(vars(area_surveyed,transect_length),sum)%>%
  unite('past_year',pasture_id,year,sep="_",remove=FALSE)%>%
  left_join(Max_BOBO_Pasture,          by="past_year")%>%
  left_join(Max_DICK_Pasture,          by="past_year")%>%
  left_join(Max_EAME_Pasture,          by="past_year")%>%
  left_join(Max_GRSP_Pasture,          by="past_year")%>%
  left_join(Max_HESP_Pasture,          by="past_year")%>%
  left_join(Max_SEWR_Pasture,          by="past_year")%>%
# left_join(Max_NOHA_Pasture,          by="past_year")%>%
  left_join(Max_GRPC_Pasture,          by="past_year")%>%
  left_join(Max_VESP_Pasture,          by="past_year")%>%
  left_join(Max_LASP_Pasture,          by="past_year")%>%
  left_join(Max_UPSA_Pasture,          by="past_year")%>%
  
  left_join(Max_BHCO_Pasture,          by="past_year")%>%
  left_join(Max_RWBL_Pasture,          by="past_year")%>%
  left_join(Max_COYE_Pasture,          by="past_year")%>%
  left_join(Max_FISP_Pasture,          by="past_year")%>%
  left_join(Max_BARS_Pasture,          by="past_year")%>%
  left_join(Max_EAKI_Pasture,          by="past_year")%>%
  left_join(Max_MODO_Pasture,          by="past_year")%>%
  left_join(Max_NOBO_Pasture,          by="past_year")%>%
  left_join(Max_LOSH_Pasture,          by="past_year")%>%
  left_join(Max_EABL_Pasture,          by="past_year")%>%
  left_join(Max_KILL_Pasture,          by="past_year")%>%
  left_join(Max_HOLA_Pasture,          by="past_year")%>%
  left_join(Max_TUVU_Pasture,          by="past_year")%>%
  left_join(Max_AMKE_Pasture,          by="past_year")%>%
 
  left_join(Max_BEVI_Pasture,          by="past_year")%>%
  left_join(Max_WIFL_Pasture,          by="past_year")%>%
  left_join(Max_GRCA_Pasture,          by="past_year")%>%
  left_join(Max_BRTH_Pasture,          by="past_year")%>%
  left_join(Max_OROR_Pasture,          by="past_year")%>%
  left_join(Max_AMRO_Pasture,          by="past_year")%>%
  left_join(Max_NOMO_Pasture,          by="past_year")%>%
  mutate(across(Max_BOBO:Max_NOMO, ~replace_na(.x, 0)))%>%
  filter(!year=="NA")%>%
  filter(year>2007)%>%
  pivot_longer(cols=starts_with("Max"),
               names_to="Species",
               values_to="Max_Abund",
               names_prefix="Max_")%>%
  mutate(Max_Abund_perHa=round(Max_Abund/area_surveyed,2))%>%
  unite("past_year_species",pasture_id,year,Species,sep="_",remove=FALSE)%>%
  pivot_wider(
    id_cols = c(year,Species),
    names_from = pasture_id,
    values_from = Max_Abund_perHa)%>%
  left_join(Obl_Fac_List,by="Species")%>%
  select(year,Species,KLN, PYN, LTR, PYS, PYW, "235",RCH2014,RIS,RIN,GIL,BSH,KLT,RNR,Obl_Fac)

write.csv(Max_Abund_Final_Pasture, "Max_Abund_Final_Pasture_DNR_allyears.csv")

Patch_Info=Patch_Visit_Info%>%
  group_by(pasture_id,patch_id,year)%>%
  count()

write.csv(Patch_Info, "Patch_Info.csv")

