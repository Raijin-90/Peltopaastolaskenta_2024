library(tidyverse);library(here);library(varhandle)
#Hajontojen laskeminen tuotantosuuntien päästöille. 
#Aggregointi, GTK-data

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Suoritetaan GTK-datan aggregointiskriptistä alku, vaan ei itse aggregointia: siihen kohdistetaan muutoksia
source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:260)

#tuotantosuuntanimien korjaus

GTKdata<-GTKdata %>% mutate(Tuotantosuunta= case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                             Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                             Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~"Nurmet_laitumet_hakamaat",
                             Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~"Palkokasvit_pl_tarhaherne",
                             Tuotantosuunta == "Rypsi ja rapsi" ~"Rypsi_rapsi",
                             Tuotantosuunta == "Tattari ja kinoa" ~"Tattari_kinoa",
                             Tuotantosuunta == "Vihannekset ja juurekset" ~"Vihannekset_juurekset",
                             Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                             .default = Tuotantosuunta))


#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuunta- ja tilatasolla koko datamassasta

GTK_aggregointi_kaikki_maa<-
  aggregate(
    list(GTKdata$Maannossumma),
    by = list(
      GTKdata$MAATILA_TUNNUS, 
      GTKdata$Tuotantosuunta,
      GTKdata$KASVIKOODI_lohkodata_reclass,
      GTKdata$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_kaikki_maa) <-
  c("Tilatunnus","Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Peltoala_ha")


GTK_aggregointi_mineral <-
  aggregate(
    list(GTKdata$Mineraalia),
    by = list(
      GTKdata$MAATILA_TUNNUS, 
      GTKdata$Tuotantosuunta,
      GTKdata$KASVIKOODI_lohkodata_reclass,
      GTKdata$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral) <-
  c("Tilatunnus","Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Mineraalimaata")

GTK_aggregointi_elop <-
  aggregate(
    list(GTKdata$Eloperaista),
    by = list(
      GTKdata$MAATILA_TUNNUS, 
      GTKdata$Tuotantosuunta,
      GTKdata$KASVIKOODI_lohkodata_reclass,
      GTKdata$KASVINIMI_reclass),
    sum
  )
colnames(GTK_aggregointi_elop) <-
  c("Tilatunnus","Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "EloperäistäMaata")





#Näistä tehdään oma tuloste. Mitään ei vielä suodateta pois. 

GTK_alat<-createWorkbook()

addWorksheet(GTK_alat,"Mineraaliala")
writeData(GTK_alat,"Mineraaliala", GTK_aggregointi_mineral)
addWorksheet(GTK_alat, "Eloperäinen ala")
writeData(GTK_alat,"Eloperäinen ala", GTK_aggregointi_elop)
addWorksheet(GTK_alat,"Kaikki ala")
writeData(GTK_alat,"Kaikki ala",GTK_aggregointi_kaikki_maa)

saveWorkbook(GTK_alat, file=here("Output/AreaAggregates/Tasokorjaamaton_kokonaisala_tilat_gtk.xlsx"), overwrite = T)


#Lisäys 7/11/23.
#Muutetaan raivauksen päästön laskentatapaa.
#Sitä ei lasketa summaamalla viljelyn ja raivauksen päästöä raivatun lohkon tapauksessa. 
#Vaan niillä raivauskerroin sisältää jo raivaustoiminnasta ja viljelystä aiheutuvan päästön, joka lasketaan vain 1 kerran.  
#Otetaan raivatut lohkot irti GTKdatasta. Niiden päästö lasketaan erikseen.  

GTKdata_raivaamattomat<- filter(GTKdata, is.na(Raivattu))

#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuuntatasolla

GTK_aggregointi_mineral <-
  aggregate(
    list(GTKdata_raivaamattomat$Mineraalia),
    by = list(
      GTKdata_raivaamattomat$MAATILA_TUNNUS,
      GTKdata_raivaamattomat$Tuotantosuunta,
      GTKdata_raivaamattomat$KASVIKOODI_lohkodata_reclass,
      GTKdata_raivaamattomat$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral) <-
  c("Tilatunnus","Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Mineraalimaata")

GTK_aggregointi_elop <-
  aggregate(
    list(GTKdata_raivaamattomat$Eloperaista),
    by = list(
      GTKdata_raivaamattomat$MAATILA_TUNNUS,
      GTKdata_raivaamattomat$Tuotantosuunta,
      GTKdata_raivaamattomat$KASVIKOODI_lohkodata_reclass,
      GTKdata_raivaamattomat$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop) <-
  c("Tilatunnus",
    "Tuotantosuunta",
    "Kasvikoodi",
    "Kasvinimi",
    "EloperäistäMaata")


#RAIVIOT #### 


GTK_raiviot<-filter(GTKdata, !(is.na(GTKdata$Raivattu))) #2000 jälkeen raivatut lohkot irti


#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuuntatasolla

GTK_aggregointi_mineral_raiviot <-
  aggregate(
    list(GTK_raiviot$Mineraalia),
    by = list(
      GTK_raiviot$MAATILA_TUNNUS,
      GTK_raiviot$Tuotantosuunta,
      GTK_raiviot$KASVIKOODI_lohkodata_reclass,
      GTK_raiviot$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral_raiviot) <-
  c("Tilatunnus","Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Mineraalimaata")


GTK_aggregointi_elop_raiviot <-
  aggregate(
    list(GTK_raiviot$Eloperaista),
    by = list(
      GTK_raiviot$MAATILA_TUNNUS,
      GTK_raiviot$Tuotantosuunta,
      GTK_raiviot$KASVIKOODI_lohkodata_reclass,
      GTK_raiviot$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop_raiviot) <-
  c("Tilatunnus",
    "Tuotantosuunta",
    "Kasvikoodi",
    "Kasvinimi",
    "EloperäistäMaata")


print("GTK-datan aggregointi suoritettu.")

gc()

#Kasvitietojen täydennys kategoriatiedoilla ####
#Aggregointiskriptin lopputotteisiin liitetään luokkatiedot eri kasveista
GTK_aggregointi_mineral <-
  merge(GTK_aggregointi_mineral, Kasvikategoriat_avain, by = "Kasvikoodi")
GTK_aggregointi_elop <-
  merge(GTK_aggregointi_elop, Kasvikategoriat_avain, by = "Kasvikoodi")
GTK_aggregointi_mineral_raiviot <-
  merge(GTK_aggregointi_mineral_raiviot,
        Kasvikategoriat_avain,
        by = "Kasvikoodi")
GTK_aggregointi_elop_raiviot <-
  merge(GTK_aggregointi_elop_raiviot,
        Kasvikategoriat_avain,
        by = "Kasvikoodi")
rm(Kasvikategoriat_avain,GTK_raiviot)
gc()


#Aineiston jakaminen kasvityypeille


GTK_mineraali<-GTK_aggregointi_mineral
GTK_eloperainen<-GTK_aggregointi_elop
GTK_mineraali_raivio<-GTK_aggregointi_mineral_raiviot
GTK_eloperainen_raivio<-GTK_aggregointi_elop_raiviot

rm(GTK_aggregointi_elop,GTK_aggregointi_elop_raiviot,GTK_aggregointi_kaikki_maa,GTK_aggregointi_mineral,GTK_aggregointi_mineral_raiviot)


#Tiloittainen satoaineisto, Liitetään satokerroin.  

Satokertoimet <- read_excel(here("Data","Satokertoimet.xlsx"),sheet = "Kertoimet")
Satokertoimet[2]<-NULL

GTK_mineraali_sato<-merge(GTK_mineraali, Satokertoimet, by="Kasvikoodi")
GTK_eloperainen_sato<-merge(GTK_eloperainen, Satokertoimet, by="Kasvikoodi")
GTK_mineraali_raivio_sato<-merge(GTK_mineraali_raivio, Satokertoimet, by="Kasvikoodi")
GTK_eloperainen_raivio_sato<-merge(GTK_eloperainen_raivio, Satokertoimet, by="Kasvikoodi")


#Emissiodatan jaottelu

Cropland_korotettu_mineraalimaa<-filter(GTK_mineraali,
                                        GTK_mineraali$`Cropland/grassland` == "Cropland")

Grassland_korotettu_mineraalimaa<-filter(GTK_mineraali,
                                         GTK_mineraali$`Cropland/grassland` == "Grassland")

Cropland_korotettu_elop<-filter(GTK_eloperainen,
                                GTK_eloperainen$`Cropland/grassland` == "Cropland")

Grassland_korotettu_elop<-filter(GTK_eloperainen,
                                 GTK_eloperainen$`Cropland/grassland` == "Grassland")


Cropland_korotettu_mineraalimaa_raivio<-filter(GTK_mineraali_raivio,
                                               GTK_mineraali_raivio$`Cropland/grassland` == "Cropland")

Grassland_korotettu_mineraalimaa_raivio<-filter(GTK_mineraali_raivio,
                                                GTK_mineraali_raivio$`Cropland/grassland` == "Grassland")

Cropland_korotettu_elop_raivio<-filter(GTK_eloperainen_raivio,
                                       GTK_eloperainen_raivio$`Cropland/grassland` == "Cropland")

Grassland_korotettu_elop_raivio<-filter(GTK_eloperainen_raivio,
                                        GTK_eloperainen_raivio$`Cropland/grassland` == "Grassland")

#EMISSIOIDEN LASKENTA ####

#Määritellään päästökertoimet

source_lines(here("Skriptit/Uudet skriptit/CO2 intensiteetit.R"),7:65)


#Kerrotaan lasketut pinta-alat päästökertoimilla.
#Mineraalimaa. Näissä mukana ainoastaan raivaamattomat. 

Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% mutate(CO2_tn = viljely_CO2_cropland_mineral * Mineraalimaata)

Grassland_korotettu_mineraalimaa<-Grassland_korotettu_mineraalimaa %>% mutate(CO2_tn = viljely_CO2_grassland_mineral * Mineraalimaata)

#Mineraalimaa, raivio

Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio%>% mutate(CO2_tn = raivaus_CO2_cropland_mineral * Mineraalimaata)

Grassland_korotettu_mineraalimaa_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% mutate(CO2_tn = raivaus_CO2_grassland_mineral * Mineraalimaata)



#Eloperainen maa 

#Erillinen kerroin tämän kategorian annual cropsille, ja grassille (monivuotiset, sadolliset nurmikasvit). Ts. Croplandin sisällä olevat, sadolliset monivuotisnurmet kuten rehunurmet.  


Cropland_korotettu_elop<-Cropland_korotettu_elop %>% mutate(CO2_tn = case_when(`Yksi/monivuotinen` == "Monivuotinen" ~ viljely_CO2_cropland_elop_grass*EloperäistäMaata,
                                                      `Yksi/monivuotinen` == "Yksivuotinen" ~ viljely_CO2_cropland_elop_annual_crops*EloperäistäMaata))

Grassland_korotettu_elop<-Grassland_korotettu_elop %>% mutate(CO2_tn = viljely_CO2_grassland_elop * EloperäistäMaata)


#Eloperäinen maa, raivio 

Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% mutate(CO2_tn = raivaus_CO2_cropland_elop * EloperäistäMaata)

Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio %>% mutate(CO2_tn = raivaus_CO2_grassland_elop * EloperäistäMaata)






#EMISSIOIDEN SUMMAUS ####
#Emissioiden summaus, lasketaan tilan emissio yhteensä. 


#ELoperäinen maa

Emissio_cropland_elop<-Cropland_korotettu_elop %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_elop_cropland = sum(CO2_tn),
                                                                                                      EloperäistäMaata_cropland = sum(EloperäistäMaata))



Emissio_grassland_elop<-Grassland_korotettu_elop %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_elop_grassland = sum(CO2_tn),
                                                                                EloperäistäMaata_grassland = sum(EloperäistäMaata))




Emissio_cropland_elop_raivio<-Cropland_korotettu_elop_raivio %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_elop_cropland_raiv = sum(CO2_tn),
                                                                                                      EloperäistäMaata_cropland_raiv = sum(EloperäistäMaata))



Emissio_grassland_elop_raivio<-Grassland_korotettu_elop_raivio %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_elop_grassland_raiv = sum(CO2_tn),
                                                                                                        EloperäistäMaata_grassland_raiv = sum(EloperäistäMaata))


#Mineraalimaa


Emissio_cropland_min<-Cropland_korotettu_mineraalimaa %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_min_cropland = sum(CO2_tn),
                                                                                                      Mineraalimaata_cropland = sum(Mineraalimaata))



Emissio_grassland_min<-Grassland_korotettu_mineraalimaa %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_min_grassland = sum(CO2_tn),
                                                                                                        Mineraalimaata_grassland = sum(Mineraalimaata))




Emissio_cropland_min_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_min_cropland_raiv = sum(CO2_tn),
                                                                                                                   Mineraalimaata_cropland_raiv = sum(Mineraalimaata))



Emissio_grassland_min_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% group_by(Tuotantosuunta, Tilatunnus) %>% summarise(CO2_tn_min_grassland_raiv = sum(CO2_tn),
                                                                                                                      Mineraalimaata_grassland_raiv = sum(Mineraalimaata))

#Emissioiden yhdistäminen


mineraali_cropland<- merge(Emissio_cropland_min, Emissio_cropland_min_raivio, by=c("Tuotantosuunta", "Tilatunnus"), all=T)
mineraali_cropland[is.na(mineraali_cropland)]<-0

elop_cropland<- merge(Emissio_cropland_elop, Emissio_cropland_elop_raivio, by=c("Tuotantosuunta", "Tilatunnus"), all=T)
elop_cropland[is.na(elop_cropland)]<-0


mineraali_grassland<- merge(Emissio_grassland_min, Emissio_grassland_min_raivio, by=c("Tuotantosuunta", "Tilatunnus"), all=T)
mineraali_grassland[is.na(mineraali_grassland)]<-0

elop_grassland<- merge(Emissio_grassland_elop, Emissio_grassland_elop_raivio, by=c("Tuotantosuunta", "Tilatunnus"), all=T)
elop_grassland[is.na(elop_grassland)]<-0

#Tarkistetaan että lukuja ei putoa. Jos pinta-aloja puuttuu, puuttuu vastaavat päästötkin. 

sum(mineraali_cropland$Mineraalimaata_cropland)==
sum(Emissio_cropland_min$Mineraalimaata_cropland)

sum(mineraali_cropland$Mineraalimaata_cropland_raiv)==
sum(Emissio_cropland_min_raivio$Mineraalimaata_cropland_raiv)

sum(mineraali_grassland$Mineraalimaata_grassland)==
sum(Emissio_grassland_min$Mineraalimaata_grassland)

sum(mineraali_grassland$Mineraalimaata_grassland_raiv)==
sum(Emissio_grassland_min_raivio$Mineraalimaata_grassland_raiv)

sum(elop_cropland$EloperäistäMaata_cropland)==
sum(Emissio_cropland_elop$EloperäistäMaata_cropland)


sum(elop_cropland$EloperäistäMaata_cropland_raiv) ==
sum(Emissio_cropland_elop_raivio$EloperäistäMaata_cropland_raiv)

sum(elop_grassland$EloperäistäMaata_grassland) ==
sum(Emissio_grassland_elop$EloperäistäMaata_grassland)

sum(elop_grassland$EloperäistäMaata_grassland_raiv ) ==
sum(Emissio_grassland_elop_raivio$EloperäistäMaata_grassland_raiv)


sum(mineraali_cropland$Mineraalimaata_cropland) + sum(mineraali_cropland$Mineraalimaata_cropland_raiv) +
  sum(elop_cropland$EloperäistäMaata_cropland) + sum(elop_cropland$EloperäistäMaata_cropland_raiv) +  
  sum(mineraali_grassland$Mineraalimaata_grassland) + sum(mineraali_grassland$Mineraalimaata_grassland_raiv) +
  sum(elop_grassland$EloperäistäMaata_grassland) + sum(elop_grassland$EloperäistäMaata_grassland_raiv)
  
rm.all.but(c("mineraali_cropland","mineraali_grassland","elop_cropland","elop_grassland"))

#Tuotantosuuntaryhmien lisäys ####

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text",
                  "skip"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")

elop_cropland<-inner_join(elop_cropland,Tuotantosuuntaryhmat,by="Tuotantosuunta")
elop_grassland<-inner_join(elop_grassland,Tuotantosuuntaryhmat,by="Tuotantosuunta")
mineraali_cropland<-inner_join(mineraali_cropland,Tuotantosuuntaryhmat,by="Tuotantosuunta")
mineraali_grassland<-inner_join(mineraali_grassland,Tuotantosuuntaryhmat,by="Tuotantosuunta")

rm(Tuotantosuuntaryhmat)
#Alojen ja emissioiden summaus, raivattu ja vanha pelto yhdistetään

elop_cropland<-elop_cropland %>% group_by(Tuotantosuuntaryhmä, Tilatunnus) %>% summarise(CO2_tn_elop_cropland=sum(CO2_tn_elop_cropland),
                                                                          EloperäistäMaata_cropland = sum(EloperäistäMaata_cropland),
                                                                          CO2_tn_elop_cropland_raiv = sum(CO2_tn_elop_cropland_raiv),
                                                                          EloperäistäMaata_cropland_raiv = sum(EloperäistäMaata_cropland_raiv))

elop_cropland<-elop_cropland %>% mutate(Elop_cropland_ala = EloperäistäMaata_cropland+EloperäistäMaata_cropland_raiv,
                         Elop_cropland_CO2 = CO2_tn_elop_cropland+CO2_tn_elop_cropland_raiv) %>% select(Tuotantosuuntaryhmä, Tilatunnus, Elop_cropland_ala, Elop_cropland_CO2)


elop_grassland<-elop_grassland %>% group_by(Tuotantosuuntaryhmä, Tilatunnus) %>% summarise(CO2_tn_elop_grassland=sum(CO2_tn_elop_grassland),
                                                                                         EloperäistäMaata_grassland = sum(EloperäistäMaata_grassland),
                                                                                         CO2_tn_elop_grassland_raiv = sum(CO2_tn_elop_grassland_raiv),
                                                                                         EloperäistäMaata_grassland_raiv = sum(EloperäistäMaata_grassland_raiv))

elop_grassland<-elop_grassland %>% mutate(Elop_grassland_ala = EloperäistäMaata_grassland+EloperäistäMaata_grassland_raiv,
                                        Elop_grassland_CO2 = CO2_tn_elop_grassland+CO2_tn_elop_grassland_raiv) %>% select(Tuotantosuuntaryhmä, Tilatunnus, Elop_grassland_ala, Elop_grassland_CO2)



mineraali_cropland<-mineraali_cropland %>% group_by(Tuotantosuuntaryhmä, Tilatunnus) %>% summarise(CO2_tn_min_cropland=sum(CO2_tn_min_cropland),
                                                                                           Mineraalimaata_cropland = sum(Mineraalimaata_cropland),
                                                                                           CO2_tn_min_cropland_raiv = sum(CO2_tn_min_cropland_raiv),
                                                                                           Mineraalimaata_cropland_raiv = sum(Mineraalimaata_cropland_raiv))

mineraali_cropland<-mineraali_cropland %>% mutate(Mineraali_cropland_ala = Mineraalimaata_cropland+Mineraalimaata_cropland_raiv,
                                        Mineraali_cropland_CO2 = CO2_tn_min_cropland+CO2_tn_min_cropland_raiv) %>% select(Tuotantosuuntaryhmä, Tilatunnus, Mineraali_cropland_ala, Mineraali_cropland_CO2)


mineraali_grassland<-mineraali_grassland %>% group_by(Tuotantosuuntaryhmä, Tilatunnus) %>% summarise(CO2_tn_min_grassland=sum(CO2_tn_min_grassland),
                                                                                         Mineraalimaata_grassland = sum(Mineraalimaata_grassland),
                                                                                         CO2_tn_min_grassland_raiv = sum(CO2_tn_min_grassland_raiv),
                                                                                         Mineraalimaata_grassland_raiv = sum(Mineraalimaata_grassland_raiv))

mineraali_grassland<-mineraali_grassland %>% mutate(Mineraali_grassland_ala = Mineraalimaata_grassland+Mineraalimaata_grassland_raiv,
                                                  Mineraali_grassland_CO2 = CO2_tn_min_grassland+CO2_tn_min_grassland_raiv) %>% select(Tuotantosuuntaryhmä, Tilatunnus, Mineraali_grassland_ala, Mineraali_grassland_CO2)
#Lopullinen yhdistäminen

Cropland_yhdistetty <- merge(mineraali_cropland,elop_cropland,by=c("Tuotantosuuntaryhmä","Tilatunnus"), all=T)

Grassland_yhdistetty<-merge(mineraali_grassland,elop_grassland,by=c("Tuotantosuuntaryhmä","Tilatunnus"), all=T)

#Datan ehjyyden tarkistus
sum(Cropland_yhdistetty$Mineraali_cropland_ala) + sum(Cropland_yhdistetty$Elop_cropland_ala) +
  sum(Grassland_yhdistetty$Mineraali_grassland_ala) + sum(Grassland_yhdistetty$Elop_grassland_ala)

rm.all.but(c("Cropland_yhdistetty","Grassland_yhdistetty"))


Cropland_yhdistetty<-Cropland_yhdistetty %>% mutate(Ala_yht_cropland = Mineraali_cropland_ala+Elop_cropland_ala,
                               CO2_cropland_yht = Mineraali_cropland_CO2+Elop_cropland_CO2) %>% select(Tuotantosuuntaryhmä,Tilatunnus,Ala_yht_cropland,CO2_cropland_yht)

Grassland_yhdistetty<-Grassland_yhdistetty %>% mutate(Ala_yht_grassland = Mineraali_grassland_ala+Elop_grassland_ala,
                                                    CO2_grassland_yht = Mineraali_grassland_CO2+Elop_grassland_CO2) %>% select(Tuotantosuuntaryhmä,Tilatunnus,Ala_yht_grassland,CO2_grassland_yht)

#Liitos

Yhdistetty<-merge(Cropland_yhdistetty,Grassland_yhdistetty, by=c("Tuotantosuuntaryhmä","Tilatunnus"), all=T)

Yhdistetty[is.na(Yhdistetty)]<-0

rm.all.but("Yhdistetty")

Yhdistetty<-Yhdistetty %>% mutate(Hehtaarit_yhteensa = Ala_yht_cropland+Ala_yht_grassland,
                                  Emissio_yhteensa_tn = CO2_cropland_yht+CO2_grassland_yht)

sum(Yhdistetty$Hehtaarit_yhteensa)

#Lasketaan intensiteetti t CO2/ha

Yhdistetty<-Yhdistetty %>% mutate(Intensiteetti_t_CO2_ha = Emissio_yhteensa_tn/Hehtaarit_yhteensa)

#Lasketaan intensiteettien hajonta tuotantosuuntaryhmän mukaisesti

Hajonnat<-Yhdistetty %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Intensiteetin_hajonta = sd(Intensiteetti_t_CO2_ha))


#Lasketaan tuotantosuuntakohtainen emissiointensiteettien mediaani

Mediaanit<-Yhdistetty %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Intensiteetin_mediaani = median(Intensiteetti_t_CO2_ha))



library(openxlsx)
x<-createWorkbook()
addWorksheet(x,"Keskihajonnat")
addWorksheet(x, "Mediaanit")

writeData(x, "Keskihajonnat",Hajonnat)
writeData(x, "Mediaanit",Mediaanit)

saveWorkbook(x, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Tilatyyppien_intensiteetin_hajonta_gtk.xlsx"), overwrite = T)





rm(list=ls())
gc()
