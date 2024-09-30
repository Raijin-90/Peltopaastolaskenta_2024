# Tuotantosuuntaluokkien päivitys peltoaineistoon 20.3.2024
# HV

# JS modifioi tuotantosuuntien luokittumista eläintilojen osalta (muutettu kynnykset, kuinka monta eläintä pitää olla, jotta tilasta tulee eläintila)
# Nämä tuotantosuunnat liitettävä nyt lähtöaineistoon, jonka tuotantosuunnat vaihtuvat uusiin tilakoodin mukaan. Lähtödataa ei muilta osin muuteta.


library(tidyverse);library(here);library(readxl)

#Sisäänluenta  ####
#Peltolohkojen tiedot

library(readr)
Yhdistetty_peltodata_raivaukset_rehuvilja <- read_delim("Data/Data_vanhoilla_tuotantosuunnilla_20.3.2024/Yhdistetty_peltodata_raivaukset_rehuvilja_vanhat_tuotantosuunnat.csv", 
                                                                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                                                               trim_ws = TRUE)







Korjatut_tuotantosuunnat_20032024 <- read_excel("Data/Korjatut_tuotantosuunnat_20032024.xlsx")
colnames(Korjatut_tuotantosuunnat_20032024)[1]<-"MAATILA_TUNNUS"

#Ei haluta muuttaa olemassaolevan data framen dimensioita, jotta myöhemmät laskentavaiheet eivät häiriinny. 
#Lisätään uusi tuotantosuunta uutena muuttujana. 

Uusi_data<-merge(Yhdistetty_peltodata_raivaukset_rehuvilja, Korjatut_tuotantosuunnat_20032024, by=c("MAATILA_TUNNUS"))

#Määritellään vanhan muuttujan "Tuotantosuunta" arvojen paikalle uuden "Tuotantosuunta_muutos" arvot

Uusi_data<-Uusi_data %>% mutate(Tuotantosuunta = Tuotantosuunta_muutos) 

#Poistetaan lisätty muuttuja "Tuotantosuunta_muutos". Sen arvot on nyt siirretty muuttujaan Tuotantosuunta. 
#Myöhemmät laskennat käyttävät sitä muuttujaa (ja/tai sen sijainti-indeksiä)

Uusi_data$Tuotantosuunta_muutos <-NULL

#Kirjoitetaan Uusi_data vanhan tiedoston nimellä. Aikaisempi versio siirretty kansioon Data/Data_vanhoilla_tuotantosuunnilla varmuuskopioksi.   

write.csv2(Uusi_data, file=here("Data", "Yhdistetty_peltodata_raivaukset_rehuvilja.csv"), row.names = F)



rm(list=ls())
gc()
