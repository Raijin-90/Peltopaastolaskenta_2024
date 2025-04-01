#Luomuosuuksien laskenta kustakin kasvista GTK-peltoaineistossa
#HV 07032025

#Pakettien ja aineistojen sisäänotto ####

library(varhandle);library(here);library(tidyverse);library(openxlsx);library(stringr)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:259)


library(readr)
Luomulohkot2017 <- read_delim("Data/Ravinnelaskennan_aineisto/Luomulohkot2017.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)


rm.all.but(c("GTKdata", "Luomulohkot2017"))

#Yhdistetään Peruslohkokoodi ja kasvulohkotunniste -> peltopolygonikohtainen tunnistettava ID jolla voidaan linkittää luomulohkot erittelevä statustieto peltoaloihin

GTKdata<-GTKdata %>% mutate(yhdistettyLohkokoodi = str_c(PLTUNNUS,KLTUNNUS))

Luomulohkot<-Luomulohkot2017 %>% mutate(yhdistettyLohkokoodi = str_c(PERUSLOHKOTUNNUS,KASVULOHKOTUNNUS))

Luomulohkot<-Luomulohkot %>% select(yhdistettyLohkokoodi, LUOMUN_VAIHE)

rm.all.but(c("GTKdata","Luomulohkot"))


#irrotellaan lohkot jotka luomudatassa. Otetaan vain ne jotka ovat luomuviljelyssä, eivät luomuviljelyn siirtymävaiheessa

luomuData<-inner_join(GTKdata, Luomulohkot, by="yhdistettyLohkokoodi")

luomuData <- luomuData %>% filter(LUOMUN_VAIHE == "4 Luomuviljelyssä")


rm.all.but(c("GTKdata","luomuData"))

#Aggregoidaan luomuala kasvi-tuotantosuuntatasolla

luomuData<-luomuData %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise_at(c("Maannossumma","Eloperaista","Mineraalia"),sum)
colnames(luomuData)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Maannossumma_luomu","Eloperaista_luomu","Mineraalia_luomu")

#Vastaava aggregointi kokonaisesta GTK-datasta, jotta voidaan laskea luomun suhteellinen osuus kutakin kasvi-tuotantosuunta kombinaatiota. 

kaikkiData<-GTKdata %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise_at(c("Maannossumma","Eloperaista","Mineraalia"),sum)
colnames(kaikkiData)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Maannossumma_kaikki","Eloperaista_kaikki","Mineraalia_kaikki")

#Liitettävä siten, että kaikkiData framesta jäävät kaikki rivit ja luomuDatan arvoiksi niille tulee NA, jos kasvi-tuotantosuunta yhdistelmälle ei ole luomuviljelyä. 

yhdistettyData<-left_join(kaikkiData, luomuData, by=c("Tuotantosuunta","Kasvikoodi","Kasvinimi"))

yhdistettyData[is.na(yhdistettyData)]<-0

yhdistettyData<-yhdistettyData %>% mutate(Luomuosuus_kaikki = Maannossumma_luomu/Maannossumma_kaikki,
                          Luomuosuus_elop = Eloperaista_luomu/Eloperaista_kaikki,
                          Luomuosuus_mineraali = Mineraalia_luomu/Mineraalia_kaikki)

yhdistettyData[is.na(yhdistettyData)]<-0

Luomuosuudet<-createWorkbook()
addWorksheet(Luomuosuudet, "Luomuosuudet_viljelykasveille")
writeData(Luomuosuudet, "Luomuosuudet_viljelykasveille", yhdistettyData)
saveWorkbook(Luomuosuudet, file=here("Output/Ravinnedata/Luomuosuudet_viljelykasveille_gtk.xlsx"), overwrite = T)


#VAIHTOEHTOINEN LASKENTATAPA:
#VASTAA SKRIPTIN "Luomuosuudet_viljeltalasta_viljavuus.R" menetelmää
#Koska viljavuusdatassa on turvauduttava tilakoodipohjaiseen luomulohkojen erotteluun ja tässä ei, on näitä datoja vaikea verrata jos käytetään eriäviä menetelmiä, 
#Sovitetaan myös GTK_dataan sama laskentatapa: eritellään luomulohkot ei lohokoodien, vaan tilatunnusten perusteella. Jos tilakoodi löytyy luomulohko-taulukon tilakoodien joukosta, kaikki sen lohkot ovat luomua. 

#Edellisestä poiketen tässä vaihtoehtoisessa laskennassa ei voida eritellä luomun vaihetietoa, koska tässä luomustatus määräytyy tilakoodin, ei lohkokoodien yhdistelmän perusteella. 


#Luomuosuuksien laskenta kustakin kasvista GTK-peltoaineistossa
#HV 07032025

#Pakettien ja aineistojen sisäänotto ####

library(varhandle);library(here);library(tidyverse);library(openxlsx);library(stringr)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:259)

library(readr)
Luomulohkot17 <- read_delim("Data/Ravinnelaskennan_aineisto/Kasvulohkot_17_luomulla_tilatieto.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

rm.all.but(c("GTKdata", "Luomulohkot17"))

#Luomutilojen luettelo: uniikit tilatunnukset 
luomutilat<-unique(Luomulohkot17$MAATILA_TUNNUS)

rm(Luomulohkot17)

luomuData<-GTKdata %>% filter(MAATILA_TUNNUS %in% luomutilat)  


rm.all.but(c("GTKdata","luomuData"))

#Aggregoidaan luomuala kasvi-tuotantosuuntatasolla

luomuData<-luomuData %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise_at(c("Maannossumma","Eloperaista","Mineraalia"),sum)
colnames(luomuData)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Maannossumma_luomu","Eloperaista_luomu","Mineraalia_luomu")

#Vastaava aggregointi kokonaisesta GTK-datasta, jotta voidaan laskea luomun suhteellinen osuus kutakin kasvi-tuotantosuunta kombinaatiota. 

kaikkiData<-GTKdata %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise_at(c("Maannossumma","Eloperaista","Mineraalia"),sum)
colnames(kaikkiData)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Maannossumma_kaikki","Eloperaista_kaikki","Mineraalia_kaikki")

#Liitettävä siten, että kaikkiData framesta jäävät kaikki rivit ja luomuDatan arvoiksi niille tulee NA, jos kasvi-tuotantosuunta yhdistelmälle ei ole luomuviljelyä. 

yhdistettyData<-left_join(kaikkiData, luomuData, by=c("Tuotantosuunta","Kasvikoodi","Kasvinimi"))

yhdistettyData[is.na(yhdistettyData)]<-0

yhdistettyData<-yhdistettyData %>% mutate(Luomuosuus_kaikki = Maannossumma_luomu/Maannossumma_kaikki,
                                          Luomuosuus_elop = Eloperaista_luomu/Eloperaista_kaikki,
                                          Luomuosuus_mineraali = Mineraalia_luomu/Mineraalia_kaikki)

yhdistettyData[is.na(yhdistettyData)]<-0

Luomuosuudet<-createWorkbook()
addWorksheet(Luomuosuudet, "Luomuosuudet_viljelykasveille")
writeData(Luomuosuudet, "Luomuosuudet_viljelykasveille", yhdistettyData)
saveWorkbook(Luomuosuudet, file=here("Output/Ravinnedata/Luomuosuudet_viljelykasveille_gtk_vertailtava_versio.xlsx"), overwrite = T)




