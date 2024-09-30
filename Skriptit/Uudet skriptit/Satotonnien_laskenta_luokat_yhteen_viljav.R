#Lisäys 27/09/23: Dimensioita lisätty r. 29 eteenpäin jotta huomioi turkistilat. 
#Lasketaan tuotantosuunnittainen/tuotteittainen satoestimaatti päästö per satotonni-kertoimia varten.
#Tätä ei tarvitse tehdä erikseen raivatulle pellolle. 
library(here)
library(tidyverse)
library(readxl)
Satokertoimet <- read_excel(here("Data","Satokertoimet.xlsx"),sheet = "Kertoimet")

#Muutos 1/2024
#Raivattu pelto  joka lasketaan sen omasta kertoimesta Tämän laskemista varten ne eroteltiin.  
#Uusitussa laskentatavassa raivatut ovat vain kerran mukana, omassa framessaan. Täytyy huomioida sato myös niiden osalta. 

#Summataan satotonnien laskentaa varten raivattu ja raivaamaton ala. 
#Muuten raivatut lohkot jäävät ulos satolaskennasta kokonaan, sillä ne ovat mukana vain omassa framessaan. Koskee croplandia, grassland on sadontonta



Cropland_korotettu_mineraalimaa_sato<-merge(
  Cropland_korotettu_mineraalimaa,
  Cropland_korotettu_mineraalimaa_raivio,
  by = c(
    "Kasvikoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä"
  ),
  all = T
)

x<-colnames(Cropland_korotettu_mineraalimaa)
y<-colnames(Cropland_korotettu_mineraalimaa_raivio)
x %>% outersect(y) #Muuttunut dimensio raivauslohkojen uudelleenmäärittelyssä. Raivattuja ei kuulu "Öljypellavan" alle

Cropland_korotettu_mineraalimaa_sato[is.na(Cropland_korotettu_mineraalimaa_sato)]<-0



Cropland_korotettu_mineraalimaa_sato<-Cropland_korotettu_mineraalimaa_sato %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
                                           Hedelmät = Hedelmät.x+Hedelmät.y,
                                           Hevostilat=Hevostilat.x+Hevostilat.y,
                                           Hunajatuotanto=Hunajatuotanto.x+Hunajatuotanto.y,
                                           Lammas_ja_vuohitilat = `Lammas- ja vuohitilat.x`+`Lammas- ja vuohitilat.y`,
                                           Maitotilat=Maitotilat.x+Maitotilat.y,
                                           Mallasohra=Mallasohra.x+Mallasohra.y,
                                           Marjat=Marjat.x+Marjat.y,
                                           Maustekasvit=Maustekasvit.x+Maustekasvit.y,
                                           Munatilat=Munatilat.x+Munatilat.y,
                                           Muut_nautakarjatilat=`Muut nautakarjatilat.x`+`Muut nautakarjatilat.y`,
                                           Nurmet_laitumet_hakamaat=`Nurmet, laitumet, hakamaat.x`+`Nurmet, laitumet, hakamaat.y`,
                                           Palkokasvit_pl_tarhaherne=`Palkokasvit pl. tarhaherne.x`+`Palkokasvit pl. tarhaherne.y`,
                                           Peruna=Peruna.x+Peruna.y,
                                           Rypsi_rapsi = `Rypsi ja rapsi.x`+`Rypsi ja rapsi.y`,
                                           Siipikarjatilat=Siipikarjatilat.x+Siipikarjatilat.y,
                                           Sikatilat=Sikatilat.x+Sikatilat.y,
                                           Sokerijuurikas=Sokerijuurikas.x,Sokerijuurikas.y,
                                           Tarhaherne= Tarhaherne.x+Tarhaherne.y,
                                           Tattari_kinoa=`Tattari ja kinoa.x`+`Tattari ja kinoa.y`,
                                           Turkistilat = Turkistilat.x+Turkistilat.y,
                                           Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,
                                           Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,
                                           Yrtit = Yrtit.x+Yrtit.y,
                                           Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                           Öljypellava = Öljypellava)  #Öljypellavaa ei raivatuissa joten x+y-summailua ei tarvita. Otetaan mukaan vain kerran. 

Cropland_korotettu_mineraalimaa_sato<-Cropland_korotettu_mineraalimaa_sato %>% select(!(ends_with(".x"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
Cropland_korotettu_mineraalimaa_sato<-Cropland_korotettu_mineraalimaa_sato %>% select(!(ends_with(".y"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
#Sisältää myös ne, joille x+y summausta ei tarvinnut tehdä 

#Palautus alkuperäiseen järjestykseen sorttausvektorin perusteella. 
Nimet <- c(
  "Kasvikoodi",
  "Kasvinimi",
  "Cropland/grassland",
  "Yksi/monivuotinen",
  "Tuoteryhmä",
  "Energiakasvit",
  "Hedelmät",
  "Hevostilat",
  "Hunajatuotanto",
  "Lammas_ja_vuohitilat",
  "Maitotilat",
  "Mallasohra",
  "Marjat",
  "Maustekasvit",
  "Munatilat",
  "Muut_nautakarjatilat",
  "Nurmet_laitumet_hakamaat",
  "Palkokasvit_pl_tarhaherne",
  "Peruna",
  "Rypsi_rapsi",
  "Siipikarjatilat",
  "Sikatilat",
  "Sokerijuurikas",
  "Tarhaherne",
  "Tattari_kinoa",
  "Turkistilat",
  "Vihannekset_juurekset",
  "Viljat_pl_ohra",
  "Yrtit",
  "Öljyhamppu",
  "Öljypellava"
)

Cropland_korotettu_mineraalimaa_sato<-Cropland_korotettu_mineraalimaa_sato %>% select(all_of(Nimet))




Cropland_korotettu_elop_sato<-merge(
  Cropland_korotettu_elop,
  Cropland_korotettu_elop_raivio,
  by = c(
    "Kasvikoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä"
  ),
  all = T
)

x<-colnames(Cropland_korotettu_elop)
y<-colnames(Cropland_korotettu_elop_raivio)
x %>% outersect(y) #Muuttunut dimensio raivauslohkojen uudelleenmäärittelyssä. 

Cropland_korotettu_elop_sato[is.na(Cropland_korotettu_elop_sato)]<-0


Cropland_korotettu_elop_sato<-Cropland_korotettu_elop_sato %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
                                                                                      Hedelmät = Hedelmät.x+Hedelmät.y,
                                                                                      Hevostilat=Hevostilat.x+Hevostilat.y,
                                                                                      Hunajatuotanto=Hunajatuotanto,
                                                                                      Lammas_ja_vuohitilat = `Lammas- ja vuohitilat.x`+`Lammas- ja vuohitilat.y`,
                                                                                      Maitotilat=Maitotilat.x+Maitotilat.y,
                                                                                      Mallasohra=Mallasohra.x+Mallasohra.y,
                                                                                      Marjat=Marjat.x+Marjat.y,
                                                                                      Maustekasvit=Maustekasvit.x+Maustekasvit.y,
                                                                                      Munatilat=Munatilat.x+Munatilat.y,
                                                                                      Muut_nautakarjatilat=`Muut nautakarjatilat.x`+`Muut nautakarjatilat.y`,
                                                                                      Nurmet_laitumet_hakamaat=`Nurmet, laitumet, hakamaat.x`+`Nurmet, laitumet, hakamaat.y`,
                                                                                      Palkokasvit_pl_tarhaherne=`Palkokasvit pl. tarhaherne`,
                                                                                      Peruna=Peruna.x+Peruna.y,
                                                                                      Rypsi_rapsi = `Rypsi ja rapsi.x`+`Rypsi ja rapsi.y`,
                                                                                      Siipikarjatilat=Siipikarjatilat.x+Siipikarjatilat.y,
                                                                                      Sikatilat=Sikatilat.x+Sikatilat.y,
                                                                                      Sokerijuurikas=Sokerijuurikas.x,Sokerijuurikas.y,
                                                                                      Tarhaherne= Tarhaherne,
                                                                                      Tattari_kinoa=`Tattari ja kinoa`,
                                                                                      Turkistilat = Turkistilat.x+Turkistilat.y,
                                                                                      Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,
                                                                                      Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,
                                                                                      Yrtit = Yrtit,
                                                                                      Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                                                                      Öljypellava = Öljypellava)


Cropland_korotettu_elop_sato<-Cropland_korotettu_elop_sato %>% select(!(ends_with(".x"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
Cropland_korotettu_elop_sato<-Cropland_korotettu_elop_sato %>% select(!(ends_with(".y")))

Cropland_korotettu_elop_sato<-Cropland_korotettu_elop_sato %>% select(all_of(Nimet))




#Yhdistetään satokertoimet pinta-aloihin, tämä karsii ne joilla kerrointa ei ole (kaikki grasslandit ym.)

Cropland_korotettu_mineraalimaa_sato <-
  merge(Cropland_korotettu_mineraalimaa, Satokertoimet, by = "Kasvikoodi")
Grassland_korotettu_mineraalimaa_sato <-
  merge(Grassland_korotettu_mineraalimaa, Satokertoimet, by = "Kasvikoodi")
Cropland_korotettu_elop_sato <-
  merge(Cropland_korotettu_elop, Satokertoimet, by = "Kasvikoodi")
Grassland_korotettu_elop_sato <-
  merge(Grassland_korotettu_elop, Satokertoimet, by = "Kasvikoodi")


#Grassland-tyypissä ei ole satotietoa tuottavia kasveja. Siksi nämä framet menevät nollille. 

rm(Grassland_korotettu_elop_sato, Grassland_korotettu_mineraalimaa_sato)

gc()

Cropland_korotettu_mineraalimaa_sato[6:31] <-
  apply(Cropland_korotettu_mineraalimaa_sato[6:31], 2, function(x) {
    x * Cropland_korotettu_mineraalimaa_sato$Hehtaarisato_tonnia_ha
  })

Cropland_korotettu_elop_sato[6:31]<-Cropland_korotettu_elop_sato[6:31] <-
  apply(Cropland_korotettu_elop_sato[6:31], 2, function(x) {
    x * Cropland_korotettu_elop_sato$Hehtaarisato_tonnia_ha
  })

rm(Satokertoimet)
gc()

#Yhdistetään Mineraalimaan ja Eloperäisen maan sato ydeksi frameksi
#Sama myös päästöille ja euroille 

yhd_Cropland_korotettu_mineraalimaa_sato <-
  Cropland_korotettu_mineraalimaa_sato %>% select(1:5, 6:31)
yhd_Cropland_korotettu_elop_sato <-
  Cropland_korotettu_elop_sato %>% select(1:5, 6:31)

yhd_sato<-merge(yhd_Cropland_korotettu_mineraalimaa_sato, 
                yhd_Cropland_korotettu_elop_sato, 
                by=c("Kasvikoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"), all=T)

#Ennen mutatea nolla-arvot (jos kasvia vain toisella maalajilla) NA-arvojen tilalle

yhd_sato[is.na(yhd_sato)]<-0

rm(yhd_Cropland_korotettu_mineraalimaa_sato, yhd_Cropland_korotettu_elop_sato)

#Summataan sato mineraali- ja eloperäiseltä maalta  
yhd_sato<-yhd_sato %>%  mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
                                                 Hedelmät = Hedelmät.x+Hedelmät.y,
                                                 Hevostilat = Hevostilat.x+Hevostilat.y,
                                                 Hunajatuotanto = Hunajatuotanto.x+Hunajatuotanto.y,
                                                 Lammas_ja_vuohitilat= `Lammas- ja vuohitilat.x`+`Lammas- ja vuohitilat.y`,
                                                 Maitotilat = Maitotilat.x+Maitotilat.y,
                                                 Mallasohra = Mallasohra.x+Mallasohra.y,
                                                 Marjat = Marjat.x+Marjat.y,
                                                 Maustekasvit = Maustekasvit.x+Maustekasvit.y,
                                                 Munatilat = Munatilat.x+Munatilat.y,         
                                                 Muut_nautakarjatilat = `Muut nautakarjatilat.x`+`Muut nautakarjatilat.y`,            
                                                 Nurmet_laitumet_hakamaat = `Nurmet, laitumet, hakamaat.x`+`Nurmet, laitumet, hakamaat.y`, 
                                                 Palkokasvit_pl_tarhaherne = `Palkokasvit pl. tarhaherne.x`+`Palkokasvit pl. tarhaherne.y`,
                                                 Peruna = Peruna.x+Peruna.y, 
                                                 Rypsi_rapsi = `Rypsi ja rapsi.x`+`Rypsi ja rapsi.y`,
                                                 Siipikarjatilat = Siipikarjatilat.x+Siipikarjatilat.y,        
                                                 Sikatilat = Sikatilat.x+Sikatilat.y, 
                                                 Sokerijuurikas = Sokerijuurikas.x+Sokerijuurikas.y,
                                                 Tarhaherne = Tarhaherne.x+Tarhaherne.y,   
                                                 Tattari_kinoa = `Tattari ja kinoa.x`+`Tattari ja kinoa.y`,
                                                 Turkistilat = Turkistilat.x+Turkistilat.y,
                                                 Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,   
                                                 Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,                  
                                                 Yrtit = Yrtit.x+Yrtit.y,    
                                                 Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                                 Öljypellava =Öljypellava.x+Öljypellava.y) 



#Poistetaan .x ja .y-päätteet eli mineraali- ja elop. maan sadot, jätetään summa
#Aggregoidaan satotonnien tuoteryhmittäiset summat.

yhd_sato<-yhd_sato %>% select(!(ends_with(".x"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
yhd_sato<-yhd_sato %>% select(!(ends_with(".y")))


yhd_sato<-yhd_sato %>% pivot_longer(cols=6:length(yhd_sato), names_to = "Tuotantosuunta", values_to = "Satotonnia") 

Yhd_sato_tuotteittain<-yhd_sato %>% group_by(Kasvikoodi, Kasvinimi, Tuoteryhmä) %>% summarise(Satotonnia = sum(Satotonnia))

#Suodatetaan ulos yksittäisten viljojen sadot

Viljakoodit<-Lajit <- c("1310",
                        "1400",
                        "1120",
                        "1320",
                        "1110",
                        "1230",
                        "1220",
                        "1330",
                        "1410")

#Lajit herkkyystarkasteluun

#Lajivektori. Kaikki Croplandia
Kasvilajit<-c("1400",
              "4110",
              "5101",
              "5106",
              "4120")



                        
                        
Herkkyystark_sato_perusarvoilla<-yhd_sato %>% filter(Kasvikoodi %in% Kasvilajit)                         

library(openxlsx)
HT_perusarvo_sato<-createWorkbook() 
HT_perusarvo_sato  %>% addWorksheet("Sato_perusarvoilla")
writeData(HT_perusarvo_sato, "Sato_perusarvoilla",Herkkyystark_sato_perusarvoilla)
saveWorkbook(HT_perusarvo_sato, here("Output/Herkkyystarkastelu/Yhdistetty_RAC/Sato_perusarvoilla.xlsx"), overwrite = T)


Viljasato<-yhd_sato %>% filter(Kasvikoodi %in% Viljakoodit)


yhd_sato<-yhd_sato %>% group_by(Tuoteryhmä) %>% summarise(Satotonnia = sum(Satotonnia))


print("Yhteenlaskettu sato croplandilta tuotettu")



