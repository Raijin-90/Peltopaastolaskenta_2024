library(tidyverse);library(here);library(gt);library(usefun)

#Tasokorjaus skipataan. Päästösummien laskennassa huomioidaan jo päästöjen ja viljelyalojen laskemisen kategorioittain ja sitten yhdistämisen kokonaisaloiksi ja -päästöiksi
#Muutettu laskentatapa 23.8.24/hv: 
#N2O poistetaan tyystin analyysistä. 


#Päästö- ja pinta-aladatojen sisäänotto, CO2eq-muunto N2O:lle
library(readxl)
CO2_gtk <- read_excel(here("Output/Yksinkertaistettu_intensiteettilaskenta", "CO2_gtk.xlsx"))
CO2_gtk<-CO2_gtk %>% select(1,4,5) 
Päästödata_gtk<-CO2_gtk
rm(CO2_gtk)

Päästödata_gtk<-Päästödata_gtk%>% mutate(Paastokerroin_tn_CO2eq_ha = CO2_yhteensa/Ala_yhteensa)

colnames(Päästödata_gtk)<-c("Tuotantosuuntaryhmä","Ala_yhteensä_gtk","CO2eq_tn_gtk", "Paastokerroin_tn_CO2eq_ha_gtk")


#Viljavuusdatan versiot samoista kertoimista

CO2_viljav <- read_excel(here("Output/Yksinkertaistettu_intensiteettilaskenta", "CO2_viljav.xlsx"))
CO2_viljav<-CO2_viljav %>% select(1,4,5) 

Päästödata_viljav<-CO2_viljav

Päästödata_viljav<-Päästödata_viljav %>% mutate(Paastokerroin_tn_CO2eq_ha = CO2_yhteensa/Ala_yhteensa)

colnames(Päästödata_viljav)<-c("Tuotantosuuntaryhmä","Ala_yhteensä_viljav","CO2eq_tn_viljav","Paastokerroin_tn_CO2eq_ha_viljav")

#Kahden datan kerrointen yhdistäminen

Jakaumataulu<-merge(Päästödata_viljav, Päästödata_gtk, by="Tuotantosuuntaryhmä")

#ETOL nimien englanninnos
Jakaumataulu<-Jakaumataulu %>% mutate(Tuotantosuuntaryhmä = case_when(Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
                                                           Tuotantosuuntaryhmä == "Hevostilat" ~ "Horse farms",
                                                           Tuotantosuuntaryhmä == "Hunajatuotanto, muu eläimen hoito" ~ "Honey production",
                                                           Tuotantosuuntaryhmä == "Kauran, ohran, rukiin ja vehnän viljely" ~ "Oat, barley, rye and wheat",
                                                           Tuotantosuuntaryhmä == "Kuminan ja muiden maustekasvien viljely" ~ "Cumin & other spice crops",
                                                           Tuotantosuuntaryhmä == "Lammas- ja vuohitilat" ~ "Sheep & goat farms",
                                                           Tuotantosuuntaryhmä == "Maitotilat" ~ "Dairy farms",
                                                           Tuotantosuuntaryhmä == "Mallasohran viljely" ~ "Malt barley",
                                                           Tuotantosuuntaryhmä == "Marjojen viljely" ~ "Berries",
                                                           Tuotantosuuntaryhmä == "Muiden vihannesten ja juuresten viljely" ~ "Other vegetables & root vegetables",
                                                           Tuotantosuuntaryhmä =="Munatilat" ~ "Egg-laying poultry",
                                                           Tuotantosuuntaryhmä =="Muut nautakarjatilat" ~ "Other cattle farms",
                                                           Tuotantosuuntaryhmä == "Peltoherneen ja härkäpavun viljely" ~ "Field pea & faba bean",
                                                           Tuotantosuuntaryhmä == "Perunan viljely" ~ "Potato",
                                                           Tuotantosuuntaryhmä == "Rehukasvien viljely" ~ "Fodder crops & pastures",
                                                           Tuotantosuuntaryhmä == "Rypsin ja rapsin viljely" ~ "Rapeseed & Turnip rape",
                                                           Tuotantosuuntaryhmä == "Siipikarjatilat" ~ "Poultry",
                                                           Tuotantosuuntaryhmä == "Sikatilat" ~ "Pig farms",
                                                           Tuotantosuuntaryhmä == "Sokerijuurikkaan viljely" ~ "Sugar beet",
                                                           Tuotantosuuntaryhmä == "Tarhaherneen viljely" ~ "Garden pea",
                                                           Tuotantosuuntaryhmä == "Tattarin ja kinoan viljely" ~ "Buckwheat & Quinoa",
                                                           Tuotantosuuntaryhmä == "Turkistarhaus" ~ "Fur farms",
                                                           Tuotantosuuntaryhmä == "Yrttien viljely avomaalla" ~ "Open-ground herbs",
                                                           Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax",
                                                           Tuotantosuuntaryhmä == "Total" ~ "Total"))

#Ryhmittelyavaimet
Animal_farms<-c("Horse farms",
                "Honey production",
                "Sheep & goat farms",
                "Dairy farms",
                "Egg-laying poultry",
                "Other cattle farms",
                "Poultry",
                "Pig farms",
                "Fur farms")

Animal_farms<-sort(Animal_farms)

Crop_cultivation<-outersect(Jakaumataulu$Tuotantosuuntaryhmä, Animal_farms)
Crop_cultivation<-sort(Crop_cultivation)

library(gt)
library(RColorBrewer)
#aakkostus
Jakaumataulu<-arrange(Jakaumataulu, Tuotantosuuntaryhmä)
Jakaumataulu$Ryhmittely<-NA
Jakaumataulu$Ryhmittely[Jakaumataulu$Tuotantosuuntaryhmä %in% Crop_cultivation]<-"Crop cultivation"
Jakaumataulu$Ryhmittely[Jakaumataulu$Tuotantosuuntaryhmä %in% Animal_farms]<-"Animal_farms" #Piilotettava muuttuja jolla targetoidaan värjäys

Jakaumataulu %>% summarise(CO2_summa_viljav = sum(CO2eq_tn_viljav)/1000000,
                           Viljavuushehtaarit = sum(Ala_yhteensä_viljav),
                           CO2_summa_gtk = sum(CO2eq_tn_gtk)/1000000,
                           GTK_hehtaarit = sum(Ala_yhteensä_gtk))


Hajonnat_tauluun<-Jakaumataulu %>% group_by(Ryhmittely) %>% summarise(Hajontaluku_gtk_kerroin = sd(Paastokerroin_tn_CO2eq_ha_gtk),
                                                                      Hajontaluku_viljav_kerroin = sd(Paastokerroin_tn_CO2eq_ha_viljav)) 




Hajonnat_koko_data<-Jakaumataulu %>% summarise(Keskiarvo_gtk=round(mean(Paastokerroin_tn_CO2eq_ha_gtk),2),
                           Keskiarvo_viljavuus = round(mean(Paastokerroin_tn_CO2eq_ha_viljav),2),
  Hajonta_gtk = round(sd(Paastokerroin_tn_CO2eq_ha_gtk),2),
                           Hajonta_viljav =round(sd(Paastokerroin_tn_CO2eq_ha_viljav),2))



gt(Jakaumataulu) %>%
  sub_missing() %>%
  fmt_number(columns = c(2:3,5:6), decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = c(4,7), decimals = 1, sep_mark = " ") %>%
  tab_row_group(label = "Crop cultivation", rows = Tuotantosuuntaryhmä %in% Crop_cultivation) %>%
  tab_row_group(label = "Animal farms", rows = Tuotantosuuntaryhmä %in% Animal_farms ) %>%
  row_group_order(groups = c("Crop cultivation", "Animal farms")) %>%
summary_rows(groups = matches("Crop cultivation"), columns =c(4,7) , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark =" ")) %>%
summary_rows(groups = matches("Animal farms"), columns =c(4,7) , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark =" ")) %>% 
  tab_spanner("Geospatial data", columns = contains("gtk")) %>%
  tab_spanner("Soil fertility data", columns = contains("viljav")) %>%
 tab_style(
    style=cell_text(weight="bold"),
    locations=cells_column_labels()) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_summary()) %>% 
  tab_style(
  style=cell_text(weight="bold"),
  locations=cells_stub_summary()) %>% 
  cols_align(align="left",1) %>%
  cols_align(align="center",2:length(Jakaumataulu)) %>%
  cols_label(Tuotantosuuntaryhmä ="Farm type") %>%
  cols_label(Ala_yhteensä_viljav ="Hectares") %>%
  cols_label(CO2eq_tn_viljav =html("tn CO<sub>2</sub>-eq")) %>%
  cols_label(`Paastokerroin_tn_CO2eq_ha_viljav` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>")) %>%
  cols_label(Ala_yhteensä_gtk ="Hectares") %>%
  cols_label(CO2eq_tn_gtk =html("tn CO<sub>2</sub>-eq")) %>%
  cols_label(`Paastokerroin_tn_CO2eq_ha_gtk` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>"))   %>%
  cols_hide(Ryhmittely) %>% 
  data_color(columns=c(4, 7),
             rows = Ryhmittely == "Crop cultivation",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F) %>%
  data_color(columns=c(4, 7),
             rows = Ryhmittely == "Animal_farms",  
             method="bin",
             bins=3,
             palette="Oranges",
             reverse=F) 



  


sum(Jakaumataulu$CO2eq_tn_gtk)
sum(Jakaumataulu$CO2eq_tn_viljav)

sum(Jakaumataulu$Ala_yhteensä_gtk)
sum(Jakaumataulu$Ala_yhteensä_viljav)

library(openxlsx)
