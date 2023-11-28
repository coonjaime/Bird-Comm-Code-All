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
                         "North1"               = "N1",
                         "North2"               = "N2",
                         "Northeast"            = "NE",
                         "South"                = "S",
                         "South1"               = "S1",
                         "South2"               = "S2",
                         "Southeast"            = "SE",
                         "West"                 = "W"))%>%
  
  unite('past_pat_year_visit',pasture_id,patch_id,year,visit,sep="_",remove=FALSE)%>%
  filter(!visit>5)

#Extract information about each visit - BY PATCH VISIT
Patch_Visit_Info=Trans_Visit%>%
  group_by(past_pat_year_visit)%>%
  summarise_at(vars(observer,start_time,clouds,wind, precipitation,cattle_interference,julian_date,year),first)

#Bird_Comm_Clean <- function(x) { }

#Extract only the visit numbers -> for use in right joins
#All_Patch_Visits_Surveyed=as.tibble(unique(BirdTrans_Data$past_pat_year_visit))%>%
#  rename("past_pat_year_visit"="value")

#All Bird Data, for use in other pipes
All_Birds_By_Visit=read_xlsx("BirdDataSubTable.xlsx")%>%
  clean_names()%>%
  left_join(BirdTrans_Data,by="bird_trans_id")%>%
  mutate(sex=recode(sex,
                    "1"               = "Male",
                    "2"               = "Female",
                    "3"               = "Male & Female",
                    "4"               = "Unknown"))%>%
  filter(!sex=='0')%>%
  right_join(All_Patch_Visits_Surveyed,by="past_pat_year_visit")%>%  
  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!sex=="Juvenile")%>%
  group_by(past_pat_year_visit,visit,observer,pasture_id,patch_id,year,julian_date)%>%
  summarise_at(vars(group_size),sum)

TotalRichBirds=read_xlsx("BirdDataSubTable.xlsx")%>%
  clean_names()%>%
  left_join(BirdTrans_Data,by="bird_trans_id")%>%
  mutate(sex=recode(sex,
                    "1"               = "Male",
                    "2"               = "Female",
                    "3"               = "Male & Female",
                    "4"               = "Unknown"))%>%
  filter(!sex=='0')%>%
  right_join(All_Patch_Visits_Surveyed,by="past_pat_year_visit")%>%  #filter(!group_size==0)%>%
  #filter(sex=="Male")%>%
  filter(!sex=="Juvenile")%>%
  mutate(species=as.factor(as.character(species)))%>%
  group_by(species,past_pat_year_visit,visit,observer,pasture_id,patch_id,year,julian_date)%>%
  count()%>%
  rename("past_pat_year_visit"="value")





TotalAbundBirds=read_xlsx("BirdDataSubTable.xlsx")%>%
  clean_names()%>%
  left_join(BirdTrans_Data,by="bird_trans_id")%>%
  mutate(sex=recode(sex,
                    "1"               = "Male",
                    "2"               = "Female",
                    "3"               = "Male & Female",
                    "4"               = "Unknown"))%>%
  filter(!sex=='0')%>%
  right_join(All_Patch_Visits_Surveyed,by="past_pat_year_visit")

#filter(!group_size==0)%>%
#filter(sex=="Male")%>%
filter(!sex=="Juvenile")%>%
  group_by(species,past_pat_year_visit,visit,observer,pasture_id,patch_id,year,julian_date)%>%
  summarise_at(vars(group_size),sum)


pivot_wider(id_cols(past_pat_year_visit,visit,observer,pasture_id,patch_id,year,julian_date),
            names_from=c(),
            values_from=c())
#}

BHCO_Data=Bird_Comm_Clean("BHCO")


group_by(pasture,patch,visit)
pivot_longer()
str(BirdComm_Data)

#   left_join(BirdTrans_Data,by="bird_trans_id")%>%

pivot_longer(
  cols = !country,
  names_to = c("event", "year"),
  names_pattern = "([A-Za-z]+)(\\d+)",
  values_to = "score"
)

pivot_wider(
  names_from = "n", 
  names_prefix = "twin_", 
  values_from = "name"
)
