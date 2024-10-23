# HERKKYYSTARKASTELU PÄÄSTÖKERTOIMILLE GTK DATAN POHJALTA ####

#Muutos 23.8.2024: N2O poistetaan laskennasta. 

#Huomaa: tätä skriptiä varten tarvitset päästötonnitiedot. Ne tallennetaan osana skriptiä Paastot_CO2
#Aja ensin haluamasi datan tasokorjaus ja päästölaskenta, jos sinne on tullut muutoksia.  jotta voit noutaa tarvitsemasi päästötonnitiedot. 

# Tehdään herkkyystarkastelu tietyille tuotteille.
# Porkkana, kaura, tarhaherne, kevätrypsi & syysrypsi
# Vihannes, vilja, palkokasvi, öljykasvityypit edustettuna
# Kuinka erilaiset kertoimet saadaan, kun käytetäänkin sato- tai hehtaarituotosarvoina jotain muuta arvoa, tietyltä vuodelta, kuin keskimääräistä? 

#Lajivektori. Kaikki Croplandia
Kasvilajit<-c("1400",
              "4110",
              "5101",
              "5106",
              "4120")

library(here);library(tidyverse);library(usefun);library(readxl)

#PÄÄSTÖTONNIEN NOUTO ####

#Päästötonneihin ei tule muutoksia. Päästökertoimet ovat samat.
#Skriptissä Paastot_CO2 lasketaan päästötonnit ja tallennetaan erikseen tätä varten. 
#Noudetaan nämä. Raivauksen ja raivatun päästöt summataan jo päästölaskentaskriptissä. 

library(readxl)
CO2 <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_gtk.xlsx", 
                                     sheet = "Herkkyystarkastelu_paastot")

CO2<-CO2 %>% pivot_longer(cols = 6:length(CO2), names_to = "Tuotantosuunta" ,values_to = "CO2_tn")

CO2<-CO2 %>% group_by(Kasvinimi) %>% summarise(CO2_tonnia = sum(CO2_tn))

#Summataan päästöt

Paastot<-CO2


gc()

rm(CO2)

# VILJELYPINTA-ALOJEN NOUTO ####

#Noudetaan tasokorjaus-skriptissä välituloksena tallennettavat, tasokorjatut alat.  Niitä ei tarvitse laskea uusiksi. 
#Tässä yhteydessä grasslandeja ei tarvita, kaikki tuotteet ovat croplandia 
#Summataan raivattu ja raivaamaton ala. 

library(readxl)
Cropland_ala_gtk <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_gtk.xlsx", 
                                     sheet = "Herkkyystarkastelu_alat")
sum(Cropland_ala_gtk$Hehtaaria_yhteensa_elop)+sum(Cropland_ala_gtk$Hehtaaria_yhteensa_min)


Cropland_ala_gtk<- Cropland_ala_gtk %>% filter(Kasvikoodi %in% Kasvilajit)

Cropland_ala_gtk <- Cropland_ala_gtk %>%
  mutate(Cropland_yhteensa_ha = Hehtaaria_yhteensa_min + Hehtaaria_yhteensa_elop) %>%
  select(Kasvikoodi, Kasvinimi, Tuotantosuunta, Cropland_yhteensa_ha) %>%
  group_by(Kasvikoodi, Kasvinimi) %>%
  summarise(Cropland_yhteensa_ha = sum(Cropland_yhteensa_ha)) 
  
# HEHTAARISATOTIEDON LIITTÄMINEN PINTA-ALAAN ####


#Näihin tulee nyt liittää useampia eri hehtaarisadon arvoja, joista kustakin saadaan toisistaan eriävä satototaali ja lopulta erisuuri kerroin
#"Vakioasetuksilla" pohjana olevat satomäärät ja euromäärät tulevat keskisadolla ja -hehtaarituotoksella



#Yhdistetään satokertoimet viljelyaloihin

library(readxl)
Satodata <- read_excel("Data/Historialliset_sadot_hinnat_herkkyystarkastelu.xlsx", 
                       sheet = "Hehtaarisadot", skip = 2)
colnames(Satodata) <-
  c(
    "Kasvikoodi",
    "Kasvinimi",
    "Satokerroin_2016",
    "Satokerroin_2017",
    "Satokerroin_2018",
    "Satokerroin_2019"
  )

Cropland_ala_gtk_sato<-merge(Cropland_ala_gtk, Satodata, by=c("Kasvikoodi","Kasvinimi"))


rm(Satodata)



# HEHTAARITUOTOSTIEDON YHDISTÄMINEN PINTA-ALAAN

#Samanlainen yhdistäminen hintadatoille

library(readxl)

#Hintadatat ovat yksikössä eur/ha
Hintadata <- read_excel("Data/Historialliset_sadot_hinnat_herkkyystarkastelu.xlsx", 
                        sheet = "Hehtaarituotokset", skip = 1)


Cropland_ala_gtk_hinta<-merge(Cropland_ala_gtk, Hintadata, by=c("Kasvikoodi","Kasvinimi"))
rm(Hintadata)


# PÄÄSTÖ- JA SATOSUMMIEN LASKENTA ERI VUOSIEN KERTOIMISTA

#Lasketaan päästö- ja satoskenaariot eri vuosien kertoimia käyttäen: oma satoarvo eri vuoden kertoimista
#Euroarvot: kertoimet eur/ha, kerrotaan alalla, lopputuloksena peltoalan arvo euroissa 

Cropland_ala_gtk_hinta<-Cropland_ala_gtk_hinta %>% mutate(
  Peltoalan_euroarvo_2016 = `2016`*Cropland_yhteensa_ha       ,
  Peltoalan_euroarvo_2017 = `2017`*Cropland_yhteensa_ha       ,
  Peltoalan_euroarvo_2018 = `2018`*Cropland_yhteensa_ha       , 
  Peltoalan_euroarvo_2019 = `2019`*Cropland_yhteensa_ha       ) 


#Satokertoimet t/ha, kerrotaan hehtaareilla, tulos sadon määrä tonneina.   
Cropland_ala_gtk_sato<-Cropland_ala_gtk_sato %>% mutate(
  Satotonnit_2016 = Satokerroin_2016*Cropland_yhteensa_ha       ,
  Satotonnit_2017 = Satokerroin_2017*Cropland_yhteensa_ha       ,
  Satotonnit_2018 = Satokerroin_2018*Cropland_yhteensa_ha       , 
  Satotonnit_2019 = Satokerroin_2019*Cropland_yhteensa_ha       ) 



# YHDISTETÄÄN SATOTONNI- JA EUROMÄÄRÄTIETO PÄÄSTÖIHIN ####

#Vihdoin ja viimein yhdistetään tieto satotonneista ja euromääristä eri vuosien arvoilla tietoon päästötonneista


#Eurokertoimet
Paastokertoimet_euro<-merge(Cropland_ala_gtk_hinta, Paastot, by=c("Kasvinimi"))
Paastokertoimet_euro<-Paastokertoimet_euro %>% select(-3:-7)
#Eurot tuhansiin euroihin
Paastokertoimet_euro[3:6]<-apply(Paastokertoimet_euro[3:6], 2, function(x){x/1000})


#Satokertoimet
Paastokertoimet_sato<-merge(Cropland_ala_gtk_sato, Paastot, by=c("Kasvinimi"))

Paastokertoimet_sato<-Paastokertoimet_sato %>% select(-3:-7)

sum(Paastokertoimet_sato$CO2_tonnia)
sum(Paastokertoimet_euro$CO2_tonnia)

rm(Paastot, Cropland_ala_gtk, Cropland_ala_gtk_hinta, Cropland_ala_gtk_sato)

#LASKETAAN VAIHTELEVA PÄÄSTÖKERROIN KUSTAKIN SATOARVOSTA JA EUROARVOSTA

#Lasketaan kerroin kustakin

Paastokertoimet_euro<-Paastokertoimet_euro  %>% mutate(Paastokerroin_t_keur_16 = CO2_tonnia/Peltoalan_euroarvo_2016,
                                                             Paastokerroin_t_keur_17= CO2_tonnia/Peltoalan_euroarvo_2017,
                                                             Paastokerroin_t_keur_18 = CO2_tonnia/Peltoalan_euroarvo_2018,
                                                             Paastokerroin_t_keur_19 = CO2_tonnia/Peltoalan_euroarvo_2019)



Paastokertoimet_sato<-Paastokertoimet_sato %>%  mutate(Paastokerroin_t_t_16 = CO2_tonnia/Satotonnit_2016,
                                                          Paastokerroin_t_t_17= CO2_tonnia/Satotonnit_2017,
                                                          Paastokerroin_t_t_18 = CO2_tonnia/Satotonnit_2018,
                                                          Paastokerroin_t_t_19 = CO2_tonnia/Satotonnit_2019)


# Tallennetaan "verrokkikertoimet"

library(openxlsx)
Verrokit<-createWorkbook()
addWorksheet(Verrokit, "Verrokkikertoimet_sato")
writeData(Verrokit, "Verrokkikertoimet_sato", Paastokertoimet_sato)
addWorksheet(Verrokit, "Verrokkikertoimet_euro")
writeData(Verrokit, "Verrokkikertoimet_euro", Paastokertoimet_euro)
saveWorkbook(Verrokit, file=here("Output/Herkkyystarkastelu/Yhdistetty_RAC/Herkkystarkastelu_verrokkikertoimet_gtk.xlsx"), overwrite = T)



#Peruskerrointen tuottaminen
#Lasketuista, keskimääräiseen satoo ja hehtaarituotoksen ("Perusarvoihin") pohjautuvista sato- ja euromääristä sekä päästöistä lasketaan "vakiokerroin", 
#johon voidaan verrata eri vuosien satoihin/tuotoksiin perustuvia vastaavia ("Verrokki") kertoimia. 

#Nämä tuotetaan sato- ja peltoalan euroarvo-skripteissä. 
library(readxl)
Hinnat_perusarvoilla <- read_excel(here("Output/Herkkyystarkastelu/Yhdistetty_RAC/Hinnat_perusarvoill_RAC.xlsx"), 
                                   col_types = c("numeric"))

Hinnat_perusarvoilla$Perusarvo_tuhatta_euroa<-Hinnat_perusarvoilla$Hinta_EUR/1000
Hinnat_perusarvoilla$Hinta_EUR<-NULL

#Yhdistetään päästötiedot


Peruskerroin_hinta<-merge(Paastokertoimet_euro, Hinnat_perusarvoilla, by=c("Kasvikoodi"), all=T)


#Päästöt per euro-kertoimen laskenta

Peruskerroin_hinta$Paastokerroin_perushinnalla_t_keur<-Peruskerroin_hinta$CO2_tonnia /Peruskerroin_hinta$Perusarvo_tuhatta_euroa

#Satopohjaiset kertoimet

Sadot_perusarvoilla <- read_excel(here("Output/Herkkyystarkastelu/Yhdistetty_RAC/Sato_perusarvoilla.xlsx"))
Sadot_perusarvoilla<-Sadot_perusarvoilla %>% group_by(Kasvikoodi,Kasvinimi) %>% summarise(Satotonnit_perusarvoilla = sum(Satotonnia)) 
                                       


Peruskerroin_sato<-merge(Paastokertoimet_sato, Sadot_perusarvoilla, by=c("Kasvikoodi","Kasvinimi"), all=T)

Peruskerroin_sato$Kerroin_t_t_perusarvoista<-Peruskerroin_sato$CO2_tonnia/Peruskerroin_sato$Satotonnit_perusarvoilla


#Koostetaan perusarvoista tuotetut kertoimet
Peruskertoimet<-createWorkbook()
addWorksheet(Peruskertoimet, "Satopohjaiset")
writeData(Peruskertoimet, "Satopohjaiset", Peruskerroin_sato)

addWorksheet(Peruskertoimet, "Europohjaiset")
writeData(Peruskertoimet, "Europohjaiset", Peruskerroin_hinta)

saveWorkbook(Peruskertoimet, file=here("Output/Herkkyystarkastelu/Yhdistetty_RAC/Peruskertoimet_gtk.xlsx"),overwrite = T)

print("Herkkyystarkastelun vaatimat kertoimet tuotettu gtk datasta.")


rm(list=ls())

gc()

# Nyt on laskettu vaihtoehtoisia kertoimia, joita saadaan näille tuotteille kun sato- tai hehtaarituotoskerroin vaihtelee. 
# Näissä ei huomioida tuotantosuuntaa, ja maalajien päästö ja sato aggregoidaan. 
# Sato- ja hintaskripti tuottaa tällaiset satoluvut myös "perus" eli keskiarvosatoa ja tuotosta käyttäen. Lasketaan niistä ja päästöistä vielä yksi "peruskerroin" joihin näitä verrokkeja vertaillaan. 




