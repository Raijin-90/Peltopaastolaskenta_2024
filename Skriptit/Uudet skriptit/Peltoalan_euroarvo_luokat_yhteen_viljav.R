library(openxlsx)
library(dplyr)
library(here)
#Lisäys 27.9.23 HV: Nimivektori ja dimensiot muutettu siten, että ottaa huomioon turkistilat (+1 tuotantosuunta, 27 saraketta)

#HEHTAARIHINTOJEN SISÄÄNOTTO, REHUVILJOJEN JA KASVINVILJELYN VILJOJEN TÄYDENNYS HINTADATAAN ####
#Hehtaarihinnat tuotteille sisään
library(readxl)
Hehtaarihinnat <-
  read_excel(
    here("Data", "Kasvitilojen_hehtaarituotto.xlsx"),sheet = "Hinnat_sadot",
    col_types = c(
      "skip","skip",
      "numeric",
      "text",
      "skip","skip",
      "numeric",
      "skip",
      "skip","skip",
      "skip"
    )
  )

#Suodatetaan hinnattomat pois

Hehtaarihinnat<- Hehtaarihinnat  %>% filter(!is.na(`€/ha`))
colnames(Hehtaarihinnat)<-c("Kasvikoodi","Kasvinimi","Hehtaarihinta_e_ha")

#Eritellään hehtaarihintoihin rehu- ja elintarvikeviljat
#Erittelytaulu on tehty valmiiksi. 
#Hintatiedot ovat LUKEn tilastojen Viljan hintanoteeraukset Suomessa (e/1 000 kg) pohjalta.
#Ellei tilastossa ole sopivaa kasviluokkaa, on käytetty olemassa olevaa arvoa (= parent-kasviluokan arvo, erittelemättömänä)

library(readxl)
Viljaerittelyt_hinta <- read_excel(here("Data","Viljaerittelyt_hinta.xlsx"), 
                                   sheet = "Edits",
                                   range = "A1:F15")
Viljaerittelyt_hinta<-select(Viljaerittelyt_hinta, 3,2,6)
colnames(Viljaerittelyt_hinta)<-c("Kasvikoodi","Kasvinimi","Hehtaarihinta_e_ha")

#Yhdistetään eritellyt viljat osaksi hintadataa

Hintadata<-rbind(Hehtaarihinnat, Viljaerittelyt_hinta)
Hintadata<-select(Hintadata,1,3)

rm(Hehtaarihinnat, Viljaerittelyt_hinta)

#Seuraavaksi lasketaan viljelyalojen ja hintadatan tulona viljelyalan monetaarinen arvo. 
#Yhdistetään pinta-ala-aggregaatteihin hehtaarihinnat kasvikoodien mukaan.

#Muutos 01/24: Lasketaan myös raivio-framen lohkoista. Yhdistetään lopussa raivatut raivaamattomiin. Johtuen siitä, että raivatut lohkot ovat mukana vain omassa framessaan.  

#Ei tarvitse tehdä grasslandille: laitumet yms. ovat sadottomia, ei hintaa. 


#Raiviolohkojen ja raivaamattomien alojen yhdistäminen. Ellei tätä tee, jäävät raivatut lohkot tyystin ulos.Mutta päästölaskennassa niiden täytyy olla erillään, sillä niille on oma kerroin. 
#Viljavuusdatan raivatuissa lohkoissa tapahtuu sama dimensiomuutos kuin satopuolella 

outersect(colnames(Cropland_korotettu_elop), colnames(Cropland_korotettu_elop_raivio))


Cropland_korotettu_elop_hinta<-merge(
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

#Raivattu ja raivaamaton ala yhdistetään

Cropland_korotettu_elop_hinta<-Cropland_korotettu_elop_hinta %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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

Cropland_korotettu_elop_hinta[is.na(Cropland_korotettu_elop_hinta)]<-0

Cropland_korotettu_elop_hinta<-Cropland_korotettu_elop_hinta %>% select(!(ends_with(".x"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
Cropland_korotettu_elop_hinta<-Cropland_korotettu_elop_hinta %>% select(!(ends_with(".y")))



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

Cropland_korotettu_elop_hinta<-Cropland_korotettu_elop_hinta %>% select(all_of(Nimet))


#Hintadata kiinni
Cropland_korotettu_elop_hinta <-
  merge(Cropland_korotettu_elop, Hintadata, by = "Kasvikoodi")


#Mineraalimaa: raivattu ja raivaamaton ala yhteen


outersect(colnames(Cropland_korotettu_mineraalimaa), colnames(Cropland_korotettu_mineraalimaa_raivio))


Cropland_korotettu_mineraalimaa_hinta<-merge(
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

Cropland_korotettu_mineraalimaa_hinta[is.na(Cropland_korotettu_mineraalimaa_hinta)]<-0

#Summataan raivattu ja raivaamaton ala
Cropland_korotettu_mineraalimaa_hinta<-Cropland_korotettu_mineraalimaa_hinta %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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

Cropland_korotettu_mineraalimaa_hinta<-Cropland_korotettu_mineraalimaa_hinta %>% select(!(ends_with(".x"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
Cropland_korotettu_mineraalimaa_hinta<-Cropland_korotettu_mineraalimaa_hinta %>% select(!(ends_with(".y"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
#Sisältää myös ne, joille x+y summausta ei tarvinnut tehdä 

Cropland_korotettu_mineraalimaa_hinta<-Cropland_korotettu_mineraalimaa_hinta %>% select(all_of(Nimet))


#Hintadata kiinni
Cropland_korotettu_mineraalimaa_hinta <-
  merge(Cropland_korotettu_mineraalimaa, Hintadata, by = "Kasvikoodi")


#Kerrotaan pinta-alat hehtaarihinnoilla

Cropland_korotettu_elop_hinta[6:31] <-
  apply(Cropland_korotettu_elop_hinta[6:31], 2, function(x) {
    x * Cropland_korotettu_elop_hinta$Hehtaarihinta_e_ha
  })
Cropland_korotettu_elop_hinta<-select(Cropland_korotettu_elop_hinta, 1:5,6:31)

Cropland_korotettu_mineraalimaa_hinta[6:31]<-
  apply(Cropland_korotettu_mineraalimaa_hinta[6:31], 2, function(x) {
    x * Cropland_korotettu_mineraalimaa_hinta$Hehtaarihinta_e_ha
  })
Cropland_korotettu_mineraalimaa_hinta<-select(Cropland_korotettu_mineraalimaa_hinta, 1:5,6:31)


Nimet<-c(
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
  "Öljypellava")

colnames(Cropland_korotettu_mineraalimaa_hinta)<-Nimet

colnames(Cropland_korotettu_elop_hinta)<-Nimet

rm(Hintadata)

#Summataan eloperäisille ja mineraalille lasketut hinnat 
Yhdistetyt_pellon_euroarvot <-
  merge(
    Cropland_korotettu_mineraalimaa_hinta,
    Cropland_korotettu_elop_hinta,
    by = c(
      "Kasvikoodi",
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuoteryhmä"
    ),
    all = T
  )
Yhdistetyt_pellon_euroarvot[is.na(Yhdistetyt_pellon_euroarvot)]<-0

Yhdistetyt_pellon_euroarvot<-Yhdistetyt_pellon_euroarvot %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
       Hedelmät = Hedelmät.x+Hedelmät.y,
       Hevostilat = Hevostilat.x+Hevostilat.y,
       Hunajatuotanto = Hunajatuotanto.x+Hunajatuotanto.y,
       Lammas_ja_vuohitilat= Lammas_ja_vuohitilat.x+Lammas_ja_vuohitilat.y,
       Maitotilat = Maitotilat.x+Maitotilat.y,
       Mallasohra = Mallasohra.x+Mallasohra.y,
       Marjat = Marjat.x+Marjat.y,
       Maustekasvit = Maustekasvit.x+Maustekasvit.y,
       Munatilat = Munatilat.x+Munatilat.y,         
       Muut_nautakarjatilat = Muut_nautakarjatilat.x+Muut_nautakarjatilat.y,            
       Nurmet_laitumet_hakamaat = Nurmet_laitumet_hakamaat.x+Nurmet_laitumet_hakamaat.y, 
       Palkokasvit_pl_tarhaherne = Palkokasvit_pl_tarhaherne.x+Palkokasvit_pl_tarhaherne.y,
       Peruna = Peruna.x+Peruna.y, 
       Rypsi_rapsi = Rypsi_rapsi.x+Rypsi_rapsi.y,
       Siipikarjatilat = Siipikarjatilat.x+Siipikarjatilat.y,        
       Sikatilat = Sikatilat.x+Sikatilat.y, 
       Sokerijuurikas = Sokerijuurikas.x+Sokerijuurikas.y,
       Tarhaherne = Tarhaherne.x+Tarhaherne.y,   
       Tattari_kinoa = Tattari_kinoa.x+Tattari_kinoa.y,
       Turkistilat = Turkistilat.x+Turkistilat.y,
       Vihannekset_juurekset = Vihannekset_juurekset.x+Vihannekset_juurekset.y,   
       Viljat_pl_ohra = Viljat_pl_ohra.x+Viljat_pl_ohra.y,                  
       Yrtit = Yrtit.x+Yrtit.y,    
       Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
       Öljypellava =Öljypellava.x+Öljypellava.y)


Yhdistetyt_pellon_euroarvot<-Yhdistetyt_pellon_euroarvot %>% select(!(ends_with(".x"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 
Yhdistetyt_pellon_euroarvot<-Yhdistetyt_pellon_euroarvot %>% select(!(ends_with(".y"))) #Ainoastaan tulokset eli ne, joilla x. tai .y päätettä ei ole. 


Euroarvot_tuotteille <- Yhdistetyt_pellon_euroarvot %>% 
  pivot_longer(cols =6:length(Yhdistetyt_pellon_euroarvot), names_to = "Tuotantosuunta", values_to="Euroa") %>% 
  group_by(Kasvikoodi,Kasvinimi, Tuoteryhmä) %>%
  summarise(Euroa_per_tuote = sum(Euroa))

#Lisäys 01/24: Herkkyystarkastelu (erillinen skripti) tarvitsee vakiokertoimella (keskiarvokerroin) lasketut euromäärät tietyistä tuotteista, 
# Ilman maalajin tai tuotantosuuntien erittelyä.
#Lasketaan nämä.

Lajit <- c("1400",
           "4110",
           "5101",
           "5106",
           "4120")




Herkkyystark_hinnat<-Yhdistetyt_pellon_euroarvot %>% 
  filter(Kasvikoodi %in% Lajit) %>% 
  pivot_longer(cols = 6:length(Yhdistetyt_pellon_euroarvot), names_to = "Tuotantosuunta", values_to = "Hinta") %>%
  group_by(Kasvikoodi) %>%
  summarise(Hinta_EUR=sum(Hinta))


#Tallwnnetaan välitulos

Herkkyystark_hinnat_peruskertoimesta<-createWorkbook() 
addWorksheet(Herkkyystark_hinnat_peruskertoimesta,"Hinta keskiarvokertoimesta")
writeData(Herkkyystark_hinnat_peruskertoimesta,"Hinta keskiarvokertoimesta", Herkkyystark_hinnat)
saveWorkbook(Herkkyystark_hinnat_peruskertoimesta, file=here("Output/Herkkyystarkastelu/Yhdistetty_RAC/Hinnat_perusarvoill_RAC.xlsx"), overwrite = T)


