library(tidyverse);library(here);library(gt);library(usefun)

#Tasokorjaus skipataan. Päästösummien laskennassa huomioidaan jo päästöjen ja viljelyalojen laskemisen kategorioittain ja sitten yhdistämisen kokonaisaloiksi ja -päästöiksi

#Päästö- ja pinta-aladatojen sisäänotto, CO2eq-muunto N2O:lle
library(readxl)
CO2_gtk <- read_excel(here("Output/Yksinkertaistettu_intensiteettilaskenta", "CO2_gtk.xlsx"))
CO2_gtk<-CO2_gtk %>% select(1,4,5) 

N2O_gtk<-read_excel(here("Output/Yksinkertaistettu_intensiteettilaskenta", "N2O_gtk.xlsx"))
N2O_gtk <-N2O_gtk %>% mutate(N2O_tn_yht=N2O_kg_yhteensa/1000) #N2o kilot tonneiksi
N2O_gtk <- N2O_gtk %>% mutate(N2O_CO2eq_muunto_tn=N2O_tn_yht* 265) #Muunnetaan N2o-tonnit CO2-ekvivalenteiksi kertomalla GWP-kertoimella 265 (AR5-kerroin) 
N2O_gtk <- N2O_gtk %>% select(1,7)  


Päästödata_gtk<-merge(CO2_gtk, N2O_gtk, by="Tuotantosuuntaryhmä")

Päästödata_gtk<-Päästödata_gtk %>% mutate(CO2eq_tn = CO2_yhteensa+N2O_CO2eq_muunto_tn)

Päästödata_gtk<-Päästödata_gtk %>% select(1,2,5) %>% mutate(Paastokerroin_tn_CO2eq_ha = CO2eq_tn/Ala_yhteensa)

colnames(Päästödata_gtk)<-c("Tuotantosuuntaryhmä","Ala_yhteensä_gtk","CO2eq_tn_gtk", "Paastokerroin_tn_CO2eq_ha_gtk")


#Viljavuusdatan versiot samoista kertoimista

CO2_viljav <- read_excel(here("Output/Yksinkertaistettu_intensiteettilaskenta", "CO2_viljav.xlsx"))
CO2_viljav<-CO2_viljav %>% select(1,4,5) 

N2O_viljav<-read_excel(here("Output/Yksinkertaistettu_intensiteettilaskenta", "N2O_viljav.xlsx"))
N2O_viljav <-N2O_viljav %>% mutate(N2O_tn_yht=N2O_kg_yhteensa/1000) #N2o kilot tonneiksi
N2O_viljav <- N2O_viljav %>% mutate(N2O_CO2eq_muunto_tn=N2O_tn_yht* 265) #Muunnetaan N2o-tonnit CO2-ekvivalenteiksi kertomalla GWP-kertoimella 265 (AR5-kerroin) 
N2O_viljav <- N2O_viljav %>% select(1,7)  


Päästödata_viljav<-merge(CO2_viljav, N2O_viljav, by="Tuotantosuuntaryhmä")

Päästödata_viljav<-Päästödata_viljav %>% mutate(CO2eq_tn = CO2_yhteensa+N2O_CO2eq_muunto_tn)

Päästödata_viljav<-Päästödata_viljav %>% select(1,2,5) %>% mutate(Paastokerroin_tn_CO2eq_ha = CO2eq_tn/Ala_yhteensa)


sum(Päästödata_viljav$Ala_yhteensa)

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


Cropcult<-Jakaumataulu %>% filter(Ryhmittely == "Crop cultivation")

Cows_et_al<-Jakaumataulu %>% filter(Ryhmittely == "Animal_farms")


gt(Cropcult) %>%
  sub_missing() %>%
  fmt_number(columns = c(2:3,5:6), decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = c(4,7), decimals = 1, sep_mark = " ") %>%
  tab_row_group(label = "Crop cultivation", rows = Tuotantosuuntaryhmä %in% Crop_cultivation) %>%
  row_group_order(groups = c("Crop cultivation")) %>%
  summary_rows(groups = matches("Crop cultivation"), columns =c(4,7) , fns = list(
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
  cols_label(CO2eq_tn_viljav =html("tn CO<sub>2</sub>-eq<sup>-1</sup>")) %>%
  cols_label(`Paastokerroin_tn_CO2eq_ha_viljav` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>")) %>%
  cols_label(Ala_yhteensä_gtk ="Hectares") %>%
  cols_label(CO2eq_tn_gtk =html("tn CO<sub>2</sub>-eq<sup>-1</sup>")) %>%
  cols_label(`Paastokerroin_tn_CO2eq_ha_gtk` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>"))   %>%
  cols_hide(Ryhmittely) %>% 
  data_color(columns=c(4, 7),
             rows = Ryhmittely == "Crop cultivation",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F)


gt(Cows_et_al) %>%
  sub_missing() %>%
  fmt_number(columns = c(2:3,5:6), decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = c(4,7), decimals = 1, sep_mark = " ") %>%
  tab_row_group(label = "Animal farms", rows = Tuotantosuuntaryhmä %in% Animal_farms) %>%
  row_group_order(groups = c("Animal farms")) %>%
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
  cols_label(CO2eq_tn_viljav =html("tn CO<sub>2</sub>-eq<sup>-1</sup>")) %>%
  cols_label(`Paastokerroin_tn_CO2eq_ha_viljav` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>")) %>%
  cols_label(Ala_yhteensä_gtk ="Hectares") %>%
  cols_label(CO2eq_tn_gtk =html("tn CO<sub>2</sub>-eq<sup>-1</sup>")) %>%
  cols_label(`Paastokerroin_tn_CO2eq_ha_gtk` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>"))   %>%
  cols_hide(Ryhmittely) %>% 
  data_color(columns=c(4, 7),
             rows = Ryhmittely == "Animal_farms",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F)



sum(Jakaumataulu$CO2eq_tn_gtk)
sum(Jakaumataulu$CO2eq_tn_viljav)


