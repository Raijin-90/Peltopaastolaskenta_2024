#Viljelyalojen osittain automatisoitu päivitys. 
#Tarvitset korottamattomat viljelyalat tuote-tuotantosuuntamuotoisena framena, kategorioittain. 
#Lisäksi tarvitset NIR:stä tiedon, paljoko inventaari sisältää kutakin kategoriaa 

#TULOSTETYÖKIRJAN LUONTI  ####
#TEE VAIN KERRAN

library(openxlsx)
#Kunkin kategorian nimi irti, näistä tehdään tulosteen välilehdet
Lehdet<-excel_sheets("D:/Peltopaastolaskenta_2024/Viljelyalapäivitys_2025_taitto_tasokorjaus.xlsx")
Lehdet<-Lehdet[4:length(Lehdet)]

Output<-createWorkbook()

for (i in seq_along(Lehdet)){
  
  addWorksheet(Output, Lehdet[i])
} 









######################################
#LASKENTA ####
#Data sisään. 

library(here);library(readxl);library(tidyverse)

#Myös grasslandin rakenne on erilainen, koska tuotteita on vain 1 ("Muu")

x <- read_excel("D:/Peltopaastolaskenta_2024/Viljelyalapäivitys_2025_taitto_tasokorjaus.xlsx", 
                sheet = "CropAnnOrgRaiv", skip=1, range="A2:AF200")

#Kullekin kategorialle on laskettu valmiiksi erotus, paljonko NIR:n totaaliala eroaa datan vastaavasta. 
#Viljelyalaa korotetaan/lasketaan sen verran per kategoria. Lopputuloksena ala on sama kuin NIR:ssä. 

Jaettavaa <- read_excel("D:/Peltopaastolaskenta_2024/Tasokorotettavat_maarat.xlsx", 
                                      sheet = "Data")

Jaettavaa<- Jaettavaa$Tasokorjattava_hehtaarimäärä[Jaettavaa$Category == "CropAnnOrgRaiv"]



#Säädä haluamasi parametrit. Tarvitset viljelyalat datassa, sekä inventaarin alat samaa kategoriaa. Näistä saa erotuksen, paljonko datan aloja pitää korottaa tai laskea jotta saadaan sama ala kuin NIR:ssä.  
#Skripti laskee erotuksen datan ja NIR:n viljelyalan välille, ja jakaa sen sarakesuunnassa tuotantosuunnile viljelyalan suhteessa. 
#Sitten tämä sarakkeittain korjattu viljelyala jaetaan tuotteille ko. sarakkeessa, tuotantosuuntakohtaisen viljelyalajakauman perusteella. 

#Lasketaan inventaarin ja datan viljelyalan erotuksesta kullekin tuotantosuunnalle menevä osa 
#Alan jakauma tuotantosuunnittain datassa. Summataan alat tuotantosuunnittain, jaetaan datan pinta-alalla yhteensä --> jakauma %  

Jakauma<-colSums(x[3:length(x)])/
  sum(colSums(x[3:length(x)]))

#Jakauman perusteella ositetaan kuinka paljon jaettavasta alasta allokoituu mitäkin tuotantosuuntaa kohti

Jakauma_ts<-(Jakauma*Jaettavaa)


#Tasokorjatut sarakesummat: summataan nämä korotusalat alkuperäisiin sarakesummiin, saadaan sama totaali kuin NIRssä

Korotetut_alat<-colSums(x[3:length(x)])+Jakauma_ts 

rm(Jaettavaa,Jakauma_ts)

#Korotettu ala jaetaan tuotteille viljelyalan suhteessa.
#Tätä varten kunkin tuotantosuunnan hehtaarit muutetaan %-jakaumaksi
# 0/0 tilanne --> NaN. Vaihdetaan nollat


jakauma<-function(x){x/sum(x)}

x[3:length(x)]<-apply(x[3:length(x)],2,jakauma)

x[is.na(x)]<-0

Korotetut_alat<-as.vector(Korotetut_alat)

#Kerrotaan kutakin tuotantosuuntakolumnia (3-->loppuun) Korotetut alat-vektorin vastaavalla luvulla

x[3:length(x)]<-sweep(x[3:length(x)],2, Korotetut_alat, FUN = "*")

#Sarake- ja rivisummien tarkistus. 

sum(colSums(x[3:length(x)]))
sum(rowSums(x[3:length(x)]))

#KIRJOITTAMINEN JA TULOSTUS
#TÄNNE VAIHDETTAVA, MILLE LEHDELLE MIKÄKIN KATEGORIA KIRJOITETAANc

writeData(Output,"CropAnnOrgRaiv",x)

saveWorkbook(Output, file=here("Tasokorjaukset_testi.xlsx"))
