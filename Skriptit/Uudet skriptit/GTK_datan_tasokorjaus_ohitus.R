
#Tässä versiossa jätetään tasokorjaus tekemättä, mutta muilta osin dimensiot ja rakenne ovat samat. 
#Tätä käytetään intensiteettitarkasteluissa. 

#Lisäys 27.9.2023: Muutettu dimensioita ottamaan huomioon turkistilojen mukaanotto omana tuotantosuuntanaan. 
#Lisäys 11/23: Skriptejä muutettu siten, että raivatut lohkot ovat mukana vain kerran, omalla lehdellään, tuplalaskennan välttämiseksi. 

#GTK-DATAN ESIKÄSITTELY ####
#Aja skripti "GTK-datan aggregointi" ennen tätä
library(here)
library(openxlsx)
library(here)
library(tidyverse)
library(tidyr)

source(here("Skriptit/Uudet skriptit","GTK_datan_aggregointi.R"))

#Tilatarkkuuden laskenta:
#source(here("Skriptit/Uudet skriptit","Tilatason_emissiot.R"))



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







#Data matriisimuotoiseksi, NA:n muutos nolliksi
GTK_mineraali <-
  spread(GTK_aggregointi_mineral,
         Tuotantosuunta,
         Mineraalimaata) #Matriisiksi
GTK_mineraali[6:length(GTK_mineraali)] <-
  replace(GTK_mineraali[6:length(GTK_mineraali)], is.na(GTK_mineraali[6:length(GTK_mineraali)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_mineral)

GTK_eloperainen <-
  spread(GTK_aggregointi_elop,
         Tuotantosuunta,
         EloperäistäMaata) #matriisiksi
GTK_eloperainen[6:length(GTK_eloperainen)] <-
  replace(GTK_eloperainen[6:length(GTK_eloperainen)], is.na(GTK_eloperainen[6:length(GTK_eloperainen)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_elop)

GTK_mineraali_raivio<-
  spread(GTK_aggregointi_mineral_raiviot, 
         Tuotantosuunta,
         Mineraalimaata)
GTK_mineraali_raivio[6:length(GTK_mineraali_raivio)] <-
  replace(GTK_mineraali_raivio[6:length(GTK_mineraali_raivio)], is.na(GTK_mineraali_raivio[6:length(GTK_mineraali_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_mineral_raiviot)

GTK_eloperainen_raivio<-
  spread(GTK_aggregointi_elop_raiviot, 
         Tuotantosuunta,
         EloperäistäMaata)
GTK_eloperainen_raivio[6:length(GTK_eloperainen_raivio)] <-
  replace(GTK_eloperainen_raivio[6:length(GTK_eloperainen_raivio)], is.na(GTK_eloperainen_raivio[6:length(GTK_eloperainen_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_elop_raiviot)


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

#NA-arvojen muuttaminen nolliksi ####
#NA-arvojen poisto, oikeasti ovat nollia. Nollan voi kertoa päästökertoimella ja saada nollan, NA antaa virheen. 

Cropland_korotettu_elop$Öljypellava[is.na(Cropland_korotettu_elop$Öljypellava)]<-0
Cropland_korotettu_elop_raivio$Öljypellava[is.na(Cropland_korotettu_elop_raivio$Öljypellava)]<-0
Grassland_korotettu_elop_raivio$Öljypellava[is.na(Grassland_korotettu_elop_raivio$Öljypellava)]<-0
Grassland_korotettu_elop_raivio$Yrtit[is.na(Grassland_korotettu_elop_raivio$Yrtit)]<-0


#Alat talteen 

Grassland_mineraalimaa_alat <- Grassland_korotettu_mineraalimaa
Grassland_elop_alat <- Grassland_korotettu_elop
Cropland_mineraalimaa_alat <- Cropland_korotettu_mineraalimaa
Cropland_elop_alat <- Cropland_korotettu_elop


Grassland_mineraalimaa_alat_raivio<-Grassland_korotettu_mineraalimaa_raivio
Grassland_elop_alat_raivio<-Grassland_korotettu_elop_raivio
Cropland_mineraalimaa_alat_raivio<-Cropland_korotettu_mineraalimaa_raivio
Cropland_elop_alat_raivio<-Cropland_korotettu_elop_raivio

sum(colSums(Cropland_mineraalimaa_alat[6:length(Cropland_mineraalimaa_alat)]))+
sum(colSums(Cropland_mineraalimaa_alat_raivio[6:length(Cropland_mineraalimaa_alat_raivio)]))+
sum(colSums(Cropland_elop_alat[6:length(Cropland_elop_alat)]))+
sum(colSums(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)]))


#Lisäaggregointia pinta-aloista


w<-sum(colSums(Cropland_mineraalimaa_alat[6:length(Cropland_mineraalimaa_alat)])) 

q<-Cropland_elop_alat %>% pivot_longer(cols=6:length(Cropland_elop_alat),names_to = "Tuotantosuunta", values_to = "Hehtaaria") %>% group_by(`Yksi/monivuotinen`) %>% summarise(Hehtaaria = sum(Hehtaaria))

e<-sum(colSums(Grassland_mineraalimaa_alat[6:length(Grassland_mineraalimaa_alat)])) 

r<-sum(colSums(Grassland_elop_alat[6:length(Grassland_elop_alat)])) 


s<-sum(colSums(Cropland_mineraalimaa_alat_raivio[6:length(Cropland_mineraalimaa_alat_raivio)])) 


t<-sum(colSums(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)])) 


u<-sum(colSums(Grassland_mineraalimaa_alat_raivio[6:length(Grassland_mineraalimaa_alat_raivio)])) 

s<-sum(colSums(Grassland_elop_alat_raivio[6:length(Grassland_elop_alat_raivio)])) 



#Tuotantosuuntanimien korjaus
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


colnames(Grassland_mineraalimaa_alat) <- Nimet

colnames(Cropland_mineraalimaa_alat) <- Nimet

colnames(Cropland_elop_alat) <- Nimet

colnames(Grassland_elop_alat) <- Nimet


colnames(Grassland_mineraalimaa_alat_raivio)<-Nimet

colnames(Cropland_mineraalimaa_alat_raivio)<-Nimet

colnames(Cropland_elop_alat_raivio)<-Nimet

colnames(Grassland_elop_alat_raivio)<-Nimet




sum(Cropland_mineraalimaa_alat[6:length(Cropland_mineraalimaa_alat)])+
sum(Cropland_mineraalimaa_alat_raivio[6:length(Cropland_mineraalimaa_alat_raivio)])+
sum(Cropland_elop_alat[6:length(Cropland_elop_alat)])+
sum(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)])+
sum(Grassland_mineraalimaa_alat[6:length(Grassland_mineraalimaa_alat)])+
sum(Grassland_mineraalimaa_alat_raivio[6:length(Grassland_mineraalimaa_alat_raivio)])+
sum(Grassland_elop_alat[6:length(Grassland_elop_alat)])+
  sum(Grassland_elop_alat_raivio[6:length(Grassland_elop_alat_raivio)])




Lohkoja<-GTKdata %>% select(1,2) 
Lohkoja<-unique(Lohkoja)


print("GTK-datan tasokorjaus suoritettu")


source(here("Skriptit/Uudet skriptit", "Satotonnien_laskenta_luokat_yhteen.R"))
print("Satotonnien laskenta suoritettu")

print("Lasketaan peltoalan euromääräinen arvo")
source(here("Skriptit/Uudet skriptit", "Peltoalan_euroarvo_luokat_yhteen.R"))

print("Euromääräiset arvot peltoalalle laskettu")
print("Ala seuraavaksi päästölaskenta")
