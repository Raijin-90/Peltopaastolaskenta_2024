# GTK:n data ####
#Muutos: poistetaan N2O laskennasta

library(tidyverse);library(here);library(openxlsx)
#RAIVAUS ####
#Raivauksen päästömassan erittely tuotantosuunnille

#Data sisään

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(here("Data",
                  "Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
             sheet = "Tuotantosuunnat ryhmittäin",
             col_types = c("text",
                           "text")
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")


library(readxl)
Raivauspaasto_CO2_gtk <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauspaasto_CO2_gtk.xlsx")

#Tuotantosuuntien kirjoitusasut samaksi, ts-ryhmät kiinni

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivauspaasto_CO2_gtk$Tuotantosuunta))

Raivauspaasto_CO2_gtk<-Raivauspaasto_CO2_gtk %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                            Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                            Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                            Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                            Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                            Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                            Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                            Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                            .default = Tuotantosuunta))

Raivauspaasto_CO2_gtk<-merge(Raivauspaasto_CO2_gtk, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivauspaasto_CO2_gtk$Tuotantosuunta))



#Aggregointi

CO2_koonti<-Raivauspaasto_CO2_gtk %>% group_by(Tuotantosuuntaryhmä, Tuoteryhmä) %>% summarise(CO2_elop_raivaus = sum(CO2_elop),
                                                                                              CO2_min_raivaus = sum(CO2_mineral))
CO2_koonti<-CO2_koonti %>% mutate(CO2_raivaus_yht = CO2_elop_raivaus+CO2_min_raivaus)

rm(Raivauspaasto_CO2_gtk)




#Englannistetaan tsryhmien nimet
Tulos<-CO2_koonti

Tulos<-Tulos %>% mutate(Tuotantosuuntaryhmä = case_when(Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
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
                                                        Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax"))
                                                       



write.xlsx(Tulos, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauksen_paastot_gtk.xlsx"), overwrite = T)

rm(list=ls())

# RAIVAAMATON ALA ####
# Sama operaatio, mutta raivaamattomille

Raivaamaton_paasto_CO2_gtk <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamaton_paasto_CO2_gtk.xlsx")

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(here("Data",
                  "Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
             sheet = "Tuotantosuunnat ryhmittäin",
             col_types = c("text",
                           "text")
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")



#Tuotantosuuntien kirjoitusasut samaksi, ts-ryhmät kiinni

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivaamaton_paasto_CO2_gtk$Tuotantosuunta))

Raivaamaton_paasto_CO2_gtk<-Raivaamaton_paasto_CO2_gtk %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                   Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                   Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                   Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                   Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                   Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                                   Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                                   Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                                   .default = Tuotantosuunta))

Raivaamaton_paasto_CO2_gtk<-merge(Raivaamaton_paasto_CO2_gtk, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivaamaton_paasto_CO2_gtk$Tuotantosuunta))

#Aggregointi

CO2_koonti<-Raivaamaton_paasto_CO2_gtk %>% 
  group_by(Tuotantosuuntaryhmä, Tuoteryhmä) %>% 
  summarise(CO2_elop = sum(CO2_elop),
            CO2_mineral = sum(CO2_mineral),
            CO2_yht = CO2_elop+CO2_mineral)

rm(Raivaamaton_paasto_CO2_gtk)



#Englannistetaan tsryhmien nimet

Tulos<-CO2_koonti
rm(CO2_koonti)
Tulos<-Tulos %>% mutate(Tuotantosuuntaryhmä = case_when(Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
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
                                                               Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax"))


write.xlsx(Tulos, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamattomat_paastot_gtk.xlsx"), overwrite = T)

rm(list=ls())

#Yhdistetään samaksi annexiksi

library(readxl)
Raivaamaton <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamattomat_paastot_gtk.xlsx")

Raivaus <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauksen_paastot_gtk.xlsx")

colnames(Raivaamaton)[3:5]<-c("CO2eq_raivaamaton_min","CO2eq_raivaamaton_elop","CO2eq_raivaamaton_summa")



Annex<-merge(Raivaamaton, Raivaus, by=c("Tuotantosuuntaryhmä","Tuoteryhmä"), all=T)


Annex<-Annex %>% mutate(CO2eq_summa = CO2eq_raivaamaton_summa+CO2_raivaus_yht)





sum(Annex$CO2eq_summa)

write.xlsx(Annex, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Annex_Paastot_tuotantosuunta_raivatut_erikseen_gtk_2608.xlsx"), overwrite = T)

rm(list=ls())
gc()

# Viljavuusdata ####

library(tidyverse);library(here);library(openxlsx)
#RAIVAUS ####
#Raivauksen päästömassan erittely tuotantosuunnille

#Data sisään

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(here("Data",
                  "Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
             sheet = "Tuotantosuunnat ryhmittäin",
             col_types = c("text",
                           "text")
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")


library(readxl)
Raivauspaasto_CO2_gtk <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauspaasto_CO2_viljav.xlsx")

#Tuotantosuuntien kirjoitusasut samaksi, ts-ryhmät kiinni

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivauspaasto_CO2_gtk$Tuotantosuunta))

Raivauspaasto_CO2_gtk<-Raivauspaasto_CO2_gtk %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                   Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                   Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                   Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                   Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                   Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                                   Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                                   Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                                   .default = Tuotantosuunta))

Raivauspaasto_CO2_gtk<-merge(Raivauspaasto_CO2_gtk, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivauspaasto_CO2_gtk$Tuotantosuunta))


#Aggregointi

Raivauspaasto_CO2_gtk[is.na(Raivauspaasto_CO2_gtk)]<-0

CO2_koonti<-Raivauspaasto_CO2_gtk %>% group_by(Tuotantosuuntaryhmä, Tuoteryhmä) %>% summarise(CO2_elop_raivaus = sum(CO2_elop),
                                                                                              CO2_min_raivaus = sum(CO2_mineral))
CO2_koonti<-CO2_koonti %>% mutate(CO2_raivaus_yht = CO2_elop_raivaus+CO2_min_raivaus)

rm(Raivauspaasto_CO2_gtk)

#Englannistetaan tsryhmien nimet

Tulos<-CO2_koonti

Tulos<-Tulos %>% mutate(Tuotantosuuntaryhmä = case_when(Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
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
                                                               Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax"))



Tulos[is.na(Tulos)]<-0


write.xlsx(Tulos, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauksen_paastot_viljav.xlsx"), overwrite = T)

rm(list=ls())

# RAIVAAMATON ALA ####
# Sama operaatio, mutta raivaamattomille

Raivaamaton_paasto_CO2_gtk <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamaton_paasto_CO2_viljav.xlsx")

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(here("Data",
                  "Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
             sheet = "Tuotantosuunnat ryhmittäin",
             col_types = c("text",
                           "text")
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")



#Tuotantosuuntien kirjoitusasut samaksi, ts-ryhmät kiinni

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivaamaton_paasto_CO2_gtk$Tuotantosuunta))

Raivaamaton_paasto_CO2_gtk<-Raivaamaton_paasto_CO2_gtk %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                             Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                             Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                             Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                             Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                             Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                                             Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                                             Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                                             .default = Tuotantosuunta))

Raivaamaton_paasto_CO2_gtk<-merge(Raivaamaton_paasto_CO2_gtk, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))

filter(Tuotantosuuntaryhmat, !(Tuotantosuuntaryhmat$Tuotantosuunta %in% Raivaamaton_paasto_CO2_gtk$Tuotantosuunta))

#Aggregointi

CO2_koonti<-Raivaamaton_paasto_CO2_gtk %>% 
  group_by(Tuotantosuuntaryhmä, Tuoteryhmä) %>% 
  summarise(CO2_elop = sum(CO2_elop),
            CO2_mineral = sum(CO2_mineral))

rm(Raivaamaton_paasto_CO2_gtk)

#Englannistetaan tsryhmien nimet

Tulos<-CO2_koonti
rm(CO2_koonti)
Tulos<-Tulos %>% mutate(Tuotantosuuntaryhmä = case_when(Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
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
                                                               Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax"))

Tulos<-Tulos %>% mutate(CO2_raivaamaton_yhteensa = CO2_elop+CO2_mineral)


write.xlsx(Tulos, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamattomat_paastot_viljav.xlsx"), overwrite = T)

rm(list=ls())

#Yhdistetään samaksi annexiksi

library(readxl)
Raivaamaton <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamattomat_paastot_viljav.xlsx")

Raivaus <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauksen_paastot_viljav.xlsx")

colnames(Raivaamaton)[3:5]<-c("CO2eq_raivaamaton_min","CO2eq_raivaamaton_elop","CO2eq_raivaamaton_summa")



Annex<-merge(Raivaamaton, Raivaus, by=c("Tuotantosuuntaryhmä","Tuoteryhmä"), all=T)

Annex<-Annex %>% mutate(CO2eq_summa = CO2eq_raivaamaton_summa+CO2_raivaus_yht)

sum(Annex$CO2eq_summa)

write.xlsx(Annex, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Annex_Paastot_tuotantosuunta_raivatut_erikseen_viljav_260824.xlsx"), overwrite = T)

rm(list=ls())

