
#Lisäys 27/09/23: Turkistilat huomioitu skriptissä, dimensioita muutettu, tuotantosuuntia 27

# DATA SISÄÄN ####
library(readxl)
library(dplyr)
library(tidyr)
library(here)
Viljavuus_Kaikki <-
  read_excel(here("Output/AreaAggregates","Viljavuusdata_aggregointi_multavuudesta.xlsx"),
             sheet = "Raivatut")



Kasvikategoriat_avain <- read_excel(here("Data","Kasvikategoriat_avain.xlsx"), 
                                    col_types = c("text", "text", "text", 
                                                  "numeric", "skip"))



Viljavuus_Kaikki <-
  merge(Viljavuus_Kaikki, Kasvikategoriat_avain, by = "Kasvikoodi")
rm(Kasvikategoriat_avain)

#PINTA-ALOJEN SYÖTTÖ TASOKORJAUSTA VARTEN CRF TAULUSTA #########

#MINERAALIMAAN EROTTAMINEN AINEISTOSTA ####

Viljavuus_Kaikki_min <-
  filter(Viljavuus_Kaikki,
         Viljavuus_Kaikki$`Maalaji multavuuden perusteella` == "Mineraali")
Viljavuus_Kaikki_min <-
  spread(Viljavuus_Kaikki_min, Tuotantosuunta, `ala ha`) #matriisiksi

Viljavuus_Kaikki_min[7:length(Viljavuus_Kaikki_min)] <-
  replace(Viljavuus_Kaikki_min[7:length(Viljavuus_Kaikki_min)], is.na(Viljavuus_Kaikki_min[7:length(Viljavuus_Kaikki_min)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi


#ELOPERÄISEN MAAN EROTTAMINEN AINEISTOSTA ####

Viljavuus_Kaikki_elop <-
  filter(Viljavuus_Kaikki,
         Viljavuus_Kaikki$`Maalaji multavuuden perusteella` == "Eloperäinen")
Viljavuus_Kaikki_elop <-
  spread(Viljavuus_Kaikki_elop, Tuotantosuunta, `ala ha`) #matriisiksi

Viljavuus_Kaikki_elop[7:length(Viljavuus_Kaikki_elop)] <-
  replace(Viljavuus_Kaikki_elop[7:length(Viljavuus_Kaikki_elop)], is.na(Viljavuus_Kaikki_elop[7:length(Viljavuus_Kaikki_elop)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi


Cropland_korotettu_mineraalimaa_raivio<-filter(Viljavuus_Kaikki_min,
                                               Viljavuus_Kaikki_min$`Cropland/grassland` == "Cropland")


Grassland_korotettu_mineraalimaa_raivio<-filter(Viljavuus_Kaikki_min,
                                                Viljavuus_Kaikki_min$`Cropland/grassland` == "Grassland")



Cropland_korotettu_elop_raivio<-filter(Viljavuus_Kaikki_elop,
                                       Viljavuus_Kaikki_elop$`Cropland/grassland` == "Cropland")


Grassland_korotettu_elop_raivio<-filter(Viljavuus_Kaikki_elop,
                                        Viljavuus_Kaikki_elop$`Cropland/grassland` == "Grassland")


#Multavuusmuuttujia ei enää tässä vaiheessa tarvita koska datat on jo jaettu eloperäisiin ja mineraaleihin. Jos nämä jättää, muista että tuotantosuunnat alkavat indeksistä 7 eikä 6

Cropland_korotettu_elop_raivio$`Maalaji multavuuden perusteella`<-NULL
Grassland_korotettu_elop_raivio$`Maalaji multavuuden perusteella`<-NULL
Cropland_korotettu_mineraalimaa_raivio$`Maalaji multavuuden perusteella`<-NULL
Grassland_korotettu_mineraalimaa_raivio$`Maalaji multavuuden perusteella`<-NULL

#NaN arvot nollille

Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)] <-
  replace(Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)], is.na(Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi


Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)] <-
  replace(Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)], is.na(Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi

Cropland_korotettu_mineraalimaa_raivio[6:length(Cropland_korotettu_mineraalimaa_raivio)] <-
  replace(Cropland_korotettu_mineraalimaa_raivio[6:length(Cropland_korotettu_mineraalimaa_raivio)], is.na(Cropland_korotettu_mineraalimaa_raivio[6:length(Cropland_korotettu_mineraalimaa_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi

Grassland_korotettu_mineraalimaa_raivio[6:length(Grassland_korotettu_mineraalimaa_raivio)] <-
  replace(Grassland_korotettu_mineraalimaa_raivio[6:length(Grassland_korotettu_mineraalimaa_raivio)], is.na(Grassland_korotettu_mineraalimaa_raivio[6:length(Grassland_korotettu_mineraalimaa_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi


#Pinta-alojen tallennus



Grassland_mineraalimaa_alat_raivio<-Grassland_korotettu_mineraalimaa_raivio
Grassland_elop_alat_raivio<-Grassland_korotettu_elop_raivio
Cropland_mineraalimaa_alat_raivio<-Cropland_korotettu_mineraalimaa_raivio
Cropland_elop_alat_raivio<-Cropland_korotettu_elop_raivio



#Tuotantosuuntanimien korjaus
Nimet<-c("Kasvikoodi",
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
         "Öljyhamppu")
         #"Öljypellava"#)


colnames(Grassland_mineraalimaa_alat_raivio)<-Nimet

colnames(Cropland_mineraalimaa_alat_raivio)<-Nimet


#Tuotantosuuntanimien korjaus
Nimet<-c("Kasvikoodi",
         "Kasvinimi",
         "Cropland/grassland",
         "Yksi/monivuotinen",
         "Tuoteryhmä",
         "Energiakasvit",
         "Hedelmät",
         "Hevostilat",
         #"Hunajatuotanto",
         "Lammas_ja_vuohitilat",
         "Maitotilat",
         "Mallasohra",
         "Marjat",
         "Maustekasvit",
         "Munatilat",
         "Muut_nautakarjatilat",
         "Nurmet_laitumet_hakamaat",
         #"Palkokasvit_pl_tarhaherne",
         "Peruna",
         "Rypsi_rapsi",
         "Siipikarjatilat",
         "Sikatilat",
         "Sokerijuurikas",
         #"Tarhaherne",
         #"Tattari_kinoa",
         "Turkistilat",
         "Vihannekset_juurekset",
         "Viljat_pl_ohra",
         #"Yrtit",
         "Öljyhamppu")
#"Öljypellava"#)


colnames(Cropland_elop_alat_raivio)<-Nimet

colnames(Grassland_elop_alat_raivio)<-Nimet



print("Viljavuusdatan raivioiden tasokorjaus suoritettu")



