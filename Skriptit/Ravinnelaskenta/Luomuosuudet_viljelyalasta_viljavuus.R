#Luomuosuuksien laskenta kustakin kasvista viljavuusdatan pohjalta. 

#Huomaa: laskentatapa joudutaan tekemään eri tavalla kuin GTK-aineistossa. 
#Siellä on mahdollista suoraan verrata Peruslohkokoodi-kasvulohkokoodi - yhdistelmää datassa luomulohkojen luetteloon. Jos koodiyhdistelmä löytyy luomulistasta, lohko on luomua.
#P-lukudatassa tämä ei onnistu, koska Peruslohkotunnus-kasvulohkotunnus yhdistelmä ei ole oikein. 
#Paikkatiedossa kaikki peruslohkoon kuuluvat kasvulohkot niputtuvat saman, yhden peruslohkon alle omiksi riveikseen, koska koordinaatteja ei ole kohdistettu kasvulohkoittain oikein. 


#Koska luomustatusta ei voida määritellä per lohko, vaan ainoastaan per tila, niin lohkokohtaista luomun vaihetietoa ei tähän saada huomioitua. 

#Voidaan kuitenkin erotella tilakoodilla luomutilat, ja lähteä siitä, että niiden kaikki lohkot ovat luomua. 
#Kysymys: kannattaako tämä menetelmä vertailtavuuden vuoksi toistaa myös gtk-datalle...

#HV 07032025

#Pakettien ja aineistojen sisäänotto ####

library(varhandle);library(here);library(tidyverse);library(openxlsx);library(stringr)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

library(readr)
Luomu17 <- read_delim("Data/Ravinnelaskennan_aineisto/Kasvulohkot_17_luomulla_tilatieto.csv", 
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"), 1:240)

rm.all.but(c("Viljavuusdata", "Luomu17"))

#Luomutilojen luettelo: uniikit tilatunnukset 
luomutilat<-unique(Luomu17$MAATILA_TUNNUS)

rm(Luomu17)

luomuData<-Viljavuusdata %>% filter(MAATILA_TUNNUS %in% luomutilat)  

#Aggregoidaan luomuala kasvi-tuotantosuuntatasolla. 

luomuData<-luomuData %>% group_by(Tuotantosuunta, KASVITUNNU_reclass, KASVINIMI_reclass) %>% summarise_at(c("Savet_ha", "Hiesut_ha", "Karkeat_ha", "Lieju_ha", "Multa_ha", "Turve_ha", "Maalajisumma"),sum)

colnames(luomuData)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Savet_ha_luomu", "Hiesut_ha_luomu", "Karkeat_ha_luomu", "Lieju_ha_luomu", "Multa_ha_luomu", "Turve_ha_luomu", "Maalajisumma_luomu")

#Vastaava aggregointi kokonaisesta Viljavuusdatasta, jotta voidaan laskea luomun suhteellinen osuus kutakin kasvi-tuotantosuunta kombinaatiota. 

kaikkiData<-Viljavuusdata %>% group_by(Tuotantosuunta, KASVITUNNU_reclass, KASVINIMI_reclass) %>% summarise_at(c("Savet_ha", "Hiesut_ha", "Karkeat_ha", "Lieju_ha", "Multa_ha", "Turve_ha", "Maalajisumma"),sum)
colnames(kaikkiData)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Savet_ha_kaikki", "Hiesut_ha_kaikki", "Karkeat_ha_kaikki", "Lieju_ha_kaikki", "Multa_ha_kaikki", "Turve_ha_kaikki", "Maalajisumma_kaikki")

#Eloperäinen ala on turve- ja multa-alan summa, kaikki muu on mineraalia. 

luomuData<-luomuData %>% mutate(Eloperaista_luomu = Turve_ha_luomu+Multa_ha_luomu,
                     Mineraalia_luomu = Maalajisumma_luomu-Eloperaista_luomu)

kaikkiData<-kaikkiData %>% mutate(Eloperaista_kaikki = Turve_ha_kaikki+Multa_ha_kaikki,
                                Mineraalia_kaikki = Maalajisumma_kaikki-Eloperaista_kaikki)

#Liitettävä siten, että kaikkiData framesta jäävät kaikki rivit ja luomuDatan arvoiksi niille tulee NA, jos kasvi-tuotantosuunta yhdistelmälle ei ole luomuviljelyä. 

yhdistettyData<-left_join(kaikkiData, luomuData, by=c("Tuotantosuunta","Kasvikoodi","Kasvinimi"))

yhdistettyData[is.na(yhdistettyData)]<-0

#Kuinka suuri osa kaikesta viljelyalasta on luomua?
#Entä mineraali- ja eloperäisestä maasta?

yhdistettyData<-yhdistettyData %>% mutate(Luomuosuus_kaikki = Maalajisumma_luomu/Maalajisumma_kaikki,
                                          Luomuosuus_elop = Eloperaista_luomu/Eloperaista_kaikki,
                                          Luomuosuus_mineraali = Mineraalia_luomu/Mineraalia_kaikki)

yhdistettyData[is.na(yhdistettyData)]<-0

Luomuosuudet<-createWorkbook()
addWorksheet(Luomuosuudet, "Luomuosuudet_viljelykasveille")
writeData(Luomuosuudet, "Luomuosuudet_viljelykasveille", yhdistettyData)
saveWorkbook(Luomuosuudet, file=here("Output/Ravinnedata/Luomuosuudet_viljelykasveille_viljavuusdata.xlsx"), overwrite =  T)

gc()
rm(list=ls())
