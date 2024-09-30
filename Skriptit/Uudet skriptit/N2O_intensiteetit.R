
# Lisäys 27.9.2023: Muutettu skriptin dimensioita siten, että ottaa huomioon myös turkistilat. Ts. tuotantosuuntien määrä (sarakkeiden määrä) kasvoi yhdellä. 
# Muutos 11/23: Raivatut lohkot nyt mukana ainoastaan raivattujen omalla lehdellä, eivät enää kerran viljellyssä ja toisen kerran raivatussa.  
library(readxl)
library(dplyr)
library(here)

#PÄÄSTÖKERROINTEN MÄÄRITTELY ####
#Syötetään päästökertoimet. 
#Huomaa, että desimaalitarkkuuden ero voi muuttaa tulosta suhteessa verrokkiin. Jos inventaarin luku on tarkkuutta 24.1538 ja sinulla 24.15, tulee pientä heittoa verrokkiin. 

# Päästöt ovat N2O-N, eli muutetaan vielä N2O:ksi kertomalla osamäärällä 44/28. (Tuomas Mattilan ohjei)
#Tämä rivillä 51
#Sama kerroin sekä raivaukselle että viljelylle. 


N2O_Viljely_raivaus_Cropland_perennial_elop<-9.5 #kg N2O-N-ha/yr. Tarkistettu: vastaa inventaarin s. 280 päästökertoimia. Eritellään yksi- ja monivuotiset. Sama kerroin viljellylle ja raivaukselle. 


N2O_Viljely_raivaus_Cropland_annual_elop<- 13 #kg N2O-N-ha/yr. Tarkistettu: vastaa inventaarin s. 280 päästökertoimia. Eritellään yksi- ja monivuotiset. Sama kerroin viljellylle ja raivaukselle. 


#Muutos 3.11.23: Aiemmin tässä käytetty inventaarin arvo sivulta 281 "EF for Grassland 5.7" on viljelemättömän ruohikkomaan (esim. hylätty pelto) arvo. Ei sovellu nurmen ja laidunten viljelylle, jota grassland tässä tarkoittaa. 
#LUKEn Tarja Silfver suositteli käyttämään sen sijaan nurmenviljelylle ja laitumille kertoimia 9.5 (perennials) ja 13 (annuals), jotka soveltuvat myös raivaukselle. Kts. Menetelmäohjeet\Kertoimet inventaarissa. 
#
N2O_Viljely_raivaus_Grassland_annual_elop<- 13 #kg N2O-N-ha/yr. Tarkistettu: vastaa inventaarin s. 280 päästökertoimia. Sama kerroin raivatulle ja viljellyllä. Käytetään Croplandin kertoimia, eritellään perennat ja yksivuotiset. Sivulla esitetty grassland-kerroin 5.7 on hylätyn maan kerroin, mutta meidän grasslandimme ovat viljelyssä. Tästä syystä käytetty croplandin kertoimia.  
N2O_Viljely_raivaus_Grassland_perennial_elop<- 9.5  #kg N2O-N-ha/yr. Tarkistettu: vastaa inventaarin s. 280 päästökertoimia. Sama kerroin raivatulle ja viljellyllä. Sivulla esitetty grassland-kerroin 5.7 on hylätyn maan kerroin, mutta meidän grasslandimme ovat viljelyssä. Tästä syystä käytetty croplandin kertoimia.  



#PÄÄSTÖJEN LASKENTA. ####
#Kerrotaan lasketut pinta-alat päästökertoimilla. Oikea kerroin määritettävä kohtaan { x * TÄHÄN PÄÄSTÖKERROIN} 
#N2On osalta tämä koskee vain eloperäisiä maita

#CROPLAND - VILJELTY

#Erillinen kerroin tämän kategorian annual cropsille (satoa tuottava yksivuotinen kasvusto), ja grassille (monivuotiset, sadolliset nurmikasvit). 
#Otetaan kumpikin erilleen, lasketaan päästö, ja liitetään takaisin yhteen. 



a<-filter(Cropland_korotettu_elop, Cropland_korotettu_elop$`Yksi/monivuotinen` == "Monivuotinen")

a[6:length(a)] <-
  apply(a[6:length(a)], 2, function(x) {
    x * N2O_Viljely_raivaus_Cropland_perennial_elop
  })

b<- filter(Cropland_korotettu_elop, Cropland_korotettu_elop$`Yksi/monivuotinen` == "Yksivuotinen")

b[6:length(b)] <-
  apply(b[6:length(b)], 2, function(x) {
    x * N2O_Viljely_raivaus_Cropland_annual_elop
  })

#Yhdistetään takaisin
Cropland_korotettu_elop<-rbind(a,b)

#Muunntetaan N2O-N yksikköön N2O kertomalla päästö muuntokertoimella
Muuntokerroin<-44/28

Cropland_korotettu_elop[6:length(Cropland_korotettu_elop)] <-
  apply(Cropland_korotettu_elop[6:length(Cropland_korotettu_elop)], 2, function(x){x*Muuntokerroin})

#Tallennetaan päästökilot ennen niiden jakamista. Tätä käytetään euroihin suhteutettujen kerrointen laskennassa. 


#GRASSLAND - VILJELTY, PERENNIAL 
#Lisäys 3.11.23: Käytetty kerroin korjattu ylhäällä, perennial/annual erittely mahdollista. 

a<-filter(Grassland_korotettu_elop, Grassland_korotettu_elop$`Yksi/monivuotinen` == "Monivuotinen")


a[6:length(a)]<-
  apply(a[6:length(a)], 2, function(x) {
    x * N2O_Viljely_raivaus_Grassland_perennial_elop
  })

#GRASSLAND - VILJELTY, ANNUAL 

b<- filter(Grassland_korotettu_elop, Grassland_korotettu_elop$`Yksi/monivuotinen` == "Yksivuotinen")

b[6:length(b)]<-
  apply(b[6:length(b)], 2, function(x) {
    x * N2O_Viljely_raivaus_Grassland_annual_elop
  })

#Yhdistetään takaisin
Grassland_korotettu_elop<-rbind(a,b)

#Muunntetaan N2O-N yksikköön N2O kertomalla päästö muuntokertoimella
Muuntokerroin<-44/28

Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)] <-
  apply(Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)], 2, function(x){x*Muuntokerroin})




#ELOPERÄINEN RAIVIO ####
#CROPLAND

a<-filter(Cropland_korotettu_elop_raivio, Cropland_korotettu_elop_raivio$`Yksi/monivuotinen` == "Monivuotinen")

a[6:length(a)] <-
  apply(a[6:length(a)], 2, function(x) {
    x * N2O_Viljely_raivaus_Cropland_perennial_elop
  })

b<- filter(Cropland_korotettu_elop_raivio, Cropland_korotettu_elop_raivio$`Yksi/monivuotinen` == "Yksivuotinen")

b[6:length(b)] <-
  apply(b[6:length(b)], 2, function(x) {
    x * N2O_Viljely_raivaus_Cropland_annual_elop
  })

#Yhdistetään takaisin
Cropland_korotettu_elop_raivio<-rbind(a,b)

Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)] <-
  apply(Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)], 2, function(x){x*Muuntokerroin})



#GRASSLAND - RAIVIO

a<-filter(Grassland_korotettu_elop_raivio, Grassland_korotettu_elop_raivio$`Yksi/monivuotinen` == "Monivuotinen")


a[6:length(a)]<-
  apply(a[6:length(a)], 2, function(x) {
    x * N2O_Viljely_raivaus_Grassland_perennial_elop
  })

b<- filter(Grassland_korotettu_elop_raivio, Grassland_korotettu_elop_raivio$`Yksi/monivuotinen` == "Yksivuotinen")

b[6:length(b)] <-
  apply(b[6:length(b)], 2, function(x) {
    x * N2O_Viljely_raivaus_Grassland_annual_elop
  })

#Yhdistetään takaisin
Grassland_korotettu_elop_raivio<-rbind(a,b)

Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)] <-
  apply(Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)], 2, function(x){x*Muuntokerroin})


#Raivauksen päästöjen erittely omaan tulostiedostoonsa. Ei intensiteeteiksi muuttoa.

sum(colSums(Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)]))

sum(colSums(Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)]))


a<-Cropland_korotettu_elop_raivio
b<-Grassland_korotettu_elop_raivio

a<-a %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "N2O_crop_elop_raiv")
b<-b %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "N2O_grass_elop_raiv")

a<-a %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Crop_elop_N2O = sum(N2O_crop_elop_raiv))
b<-b %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Grass_elop_N2O = sum(N2O_grass_elop_raiv))

colnames(a)[3]<-"Elop_N2O"
colnames(b)[3]<-"Elop_N2O"

x<-rbind(a, b)

Raivaus_paasto_N2O<-createWorkbook()
addWorksheet(Raivaus_paasto_N2O, "N2O")
writeData(Raivaus_paasto_N2O, "N2O", x)

rm("a","b","x")


#Vastaava erittely raivaamattomalle emissiolle
#Ei koske mineraalimaata

a<-Cropland_korotettu_elop
b<-Grassland_korotettu_elop

a<-a %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "N2O_crop_elop")
b<-b %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "N2O_grass_elop")

a<-a %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Crop_elop_N2O = sum(N2O_crop_elop))
b<-b %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Grass_elop_N2O = sum(N2O_grass_elop))

colnames(a)[3]<-"Elop_N2O"
colnames(b)[3]<-"Elop_N2O"

x<-rbind(a,b)

Raivaamaton_paasto_N2O<-createWorkbook()
addWorksheet(Raivaamaton_paasto_N2O, "N2O")
writeData(Raivaamaton_paasto_N2O, "N2O", x)



#RAIVAUKSEN JA VILJELYN PÄÄSTÖJEN SUMMAAMINEN TUOTANTOSUUNNALLE/TUOTTEELLE ####
#Yhdistetään viljelypäästöjen frameihin raivauksesta aiheutuva päästö. 
#Frameen tulee 2 kpl kutakin tuotantosuuntaa, x ja y, joista toinen on raivauksen päästö ja toinen viljelyn. Nämä summataan. 

Grassland_korotettu_elop_raivio<-
  select(Grassland_korotettu_elop_raivio,
         1:5,
         6:length(Grassland_korotettu_elop_raivio))
Cropland_korotettu_elop_raivio <-
  select(Cropland_korotettu_elop_raivio,
         1:5,
         6:length(Cropland_korotettu_elop_raivio))

#Merge eloperäiset
Grassland_elop<-  merge(
  Grassland_korotettu_elop,
  Grassland_korotettu_elop_raivio,
  by = c("Kasvikoodi", "Kasvinimi", "Cropland/grassland", "Yksi/monivuotinen", "Tuoteryhmä"),
  all = T
)
Grassland_elop[is.na(Grassland_elop)]<-0

Cropland_elop <-
  merge(
    Cropland_korotettu_elop,
    Cropland_korotettu_elop_raivio,
    by = c("Kasvikoodi", "Kasvinimi", "Cropland/grassland", "Yksi/monivuotinen", "Tuoteryhmä"),
    all = T
  )
Cropland_elop[is.na(Cropland_elop)]<-0


rm(
  Cropland_korotettu_elop,
  Cropland_korotettu_elop_raivio,
  Grassland_korotettu_elop,
  Grassland_korotettu_elop_raivio)


#Summataan: x on viljelyn ja y raivauksen päästö, luotu uusi muuttuja näiden summa
#Mutate across ei nyt onnistu koska jokaiselle tuotanetosuunnalle tehdään eri operaatio. 

Cropland_elop<-Cropland_elop %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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
                                        Turkistilat=Turkistilat.x+Turkistilat.y,
                                        Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,   
                                        Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,                  
                                        Yrtit = Yrtit.x+Yrtit.y,    
                                        Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                        Öljypellava =Öljypellava.x+Öljypellava.y)


Grassland_elop<-Grassland_elop %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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
                                          Turkistilat=Turkistilat.x+Turkistilat.y,
                                          Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,   
                                          Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,                  
                                          Yrtit = Yrtit.x+Yrtit.y,    
                                          Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                          Öljypellava =Öljypellava.x+Öljypellava.y)


#Poistetaan x- ja y-muuttujat, jätetään vain summa. 

Cropland_elop[6:57]<-NULL
Grassland_elop[6:57]<-NULL


#Tallennetaan päästökilot, niitä käytetään euroihin suhteutetun päästökerrointen laskennassa. 

Cropland_elop_paastotonnit<-Cropland_elop
Grassland_elop_paastotonnit<-Grassland_elop

Cropland_elop_paastojakauma<-Cropland_elop
Grassland_elop_paastojakauma<-Grassland_elop


#Eritellään yksittäisten tuotteiden päästökilot

Paastokilot_N2O_tuotteille<-Cropland_elop_paastotonnit %>% pivot_longer(cols = 6:length(Cropland_elop_paastotonnit), names_to = "Tuotantosuunta", values_to =   "Paastokilot_N2O")
Paastokilot_N2O_tuotteille<-Paastokilot_N2O_tuotteille %>% group_by(Kasvikoodi,Kasvinimi,Tuoteryhmä) %>% summarise(Paastokilot_N2O = sum(Paastokilot_N2O))


Tuotteiden_eurot_sadot<-merge(Euroarvot_tuotteille, Yhd_sato_tuotteittain, by=c("Kasvikoodi", "Kasvinimi", "Tuoteryhmä"), all=T)

Paastot_tuotteille_N2O<-merge(Paastokilot_N2O_tuotteille, Tuotteiden_eurot_sadot, by=c("Kasvikoodi","Kasvinimi","Tuoteryhmä"), all=T)


#Eritellään viljapäästö

Paastotonnit_vilja_N2O <- Cropland_elop %>% filter(Kasvikoodi %in% Viljakoodit)


#Samaten irrotetaan joukko herkkyystarkastelun tuotteita, jotka kattavat joukon erityyppistä tuotantoa

#Lajivektori. Kaikki Croplandia
Kasvilajit<-c("1400",
              "4110",
              "5101",
              "5106",
              "4120")


Herkkyystarkastelun_paastot_N2O<- Cropland_elop %>% filter(Kasvikoodi %in% Kasvilajit)








#Croplandille tehdään myös tuotteittainen, tuotantosuunnat häivyttävä aggregointi tuotekohtaisten sato/eurokerrointen laskemiseen. 
Cropland_yhdistetyt_paastot_tuoteryhmät<-Cropland_elop

Cropland_yhdistetyt_paastot_tuoteryhmät<-Cropland_yhdistetyt_paastot_tuoteryhmät %>% 
  pivot_longer(cols = 6:length(Cropland_yhdistetyt_paastot_tuoteryhmät), names_to = "Tuotantosuunta", values_to = "Paasto_N2O_kg") %>%
  group_by(Tuoteryhmä) %>%
  summarise(Paasto_N2O_kg = sum(Paasto_N2O_kg))

#Summataan croplandin ja grasslandin päästökilot

Grassland_elop<- Grassland_elop %>% pivot_longer(cols=6:length(Grassland_elop), names_to = "Tuotantosuunta", values_to = "N2O_kg_grassl") %>%
  group_by(Tuotantosuunta) %>% summarise(N2O_kg_grassl = sum(N2O_kg_grassl))



Cropland_elop<-Cropland_elop  %>% pivot_longer(cols=6:length(Cropland_elop),names_to = "Tuotantosuunta", values_to = "N2O_kg_cropl") %>%
  group_by(Tuotantosuunta) %>% summarise(N2O_kg_cropl = sum(N2O_kg_cropl))

N2O_kokonaispaastot<-merge(Grassland_elop, Cropland_elop, by="Tuotantosuunta")

#Lisäys  08/23: ne voidaan myös aggregoida tuotantosuuntatasolle, jotta näkee miten päästökakku jakautuu niiden kesken.
#tUOTANTOSUUNTARYHMÄT, MUUTTAMINEN PITKÄÄN MUOTOON
#Tehdään ETOL tasolla. Alkuperäisille oma avaimensa, joka alla kommentoituna pois. 

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(here("Data",
                  "Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
             sheet = "Tuotantosuunnat ryhmittäin",
             col_types = c("text",
                           "text")
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")

library(usefun)
outersect(N2O_kokonaispaastot$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)

N2O_kokonaispaastot<-merge(N2O_kokonaispaastot, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))


N2O_kokonaispaastot<-N2O_kokonaispaastot %>% 
  mutate(N2O_kg_yhteensa = N2O_kg_grassl + N2O_kg_cropl) %>%
  group_by(Tuotantosuuntaryhmä) %>% 
  summarise(N2O_kg_yhteensa = sum(N2O_kg_yhteensa))



#PINTA-ALOJEN SUMMAUS ####
#Nämä emissiot vain orgaaniselle alalle
#Summataan pinta-alat (raivattu+viljelty) intensiteettilaskuja varten
#Cropland
#Eloperäinen

a <-
  Cropland_elop_alat %>% pivot_longer(
    cols = 6:length(Cropland_elop_alat),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_crop_elop"
  )



b <-
  Cropland_elop_alat_raivio %>% pivot_longer(
    cols = 6:length(Cropland_elop_alat_raivio),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_crop_elop_raivio"
  )

Cropland_elop_yhteisala <-
  merge(
    a,
    b,
    by = c(
      "Kasvikoodi",
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuotantosuunta",
      "Tuoteryhmä"
    ),
    all = T
  )
Cropland_elop_yhteisala[is.na(Cropland_elop_yhteisala)] <- 0

Cropland_elop_yhteisala$Hehtaaria_yhteensa_elop <-
  Cropland_elop_yhteisala$Hehtaaria_crop_elop + Cropland_elop_yhteisala$Hehtaaria_crop_elop_raivio

outersect(Cropland_elop_yhteisala$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)


Cropland_elop_yhteisala<-merge(Cropland_elop_yhteisala, Tuotantosuuntaryhmat, by="Tuotantosuunta", all=T)

Cropland_elop_yhteisala <-
  Cropland_elop_yhteisala %>% mutate(Cropland_elop_ha_yhteensa = Hehtaaria_crop_elop_raivio +Hehtaaria_crop_elop) %>% 
  group_by(Tuotantosuuntaryhmä) %>%
  summarise(Cropland_elop_yhteensa = sum(Cropland_elop_ha_yhteensa))

#Sisältää sekä raivatun että raivaamattoman alan

#Grassland
#Eloperäinen

a <-
  Grassland_elop_alat %>% pivot_longer(
    cols = 6:length(Grassland_elop_alat),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_grass_elop"
  )



b <-
  Grassland_elop_alat_raivio %>% pivot_longer(
    cols = 6:length(Grassland_elop_alat_raivio),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_grass_elop_raivio"
  )

Grassland_elop_yhteisala <-
  merge(
    a,
    b,
    by = c(
      "Kasvikoodi",
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuotantosuunta",
      "Tuoteryhmä"
    ),
    all = T
  )
Grassland_elop_yhteisala[is.na(Grassland_elop_yhteisala)] <- 0

Grassland_elop_yhteisala$Hehtaaria_yhteensa_elop <-
  Grassland_elop_yhteisala$Hehtaaria_grass_elop + Grassland_elop_yhteisala$Hehtaaria_grass_elop_raivio


outersect(Grassland_elop_yhteisala$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)
Grassland_elop_yhteisala<-merge(Grassland_elop_yhteisala, Tuotantosuuntaryhmat, by="Tuotantosuunta", all=T)

Grassland_elop_yhteisala <-
  Grassland_elop_yhteisala %>% mutate(Grassland_elop_ha_yhteensa = Hehtaaria_grass_elop +Hehtaaria_grass_elop_raivio) %>% 
  group_by(Tuotantosuuntaryhmä) %>%
  summarise(Grassland_elop_yhteensa = sum(Grassland_elop_ha_yhteensa))


#Yhteenlaskettu ala
#Lopuksi summataan kokonaisala croplandia ja grasslandia...

Yhteenlaskettu_peltoala<-merge(Grassland_elop_yhteisala,
                               Cropland_elop_yhteisala, by="Tuotantosuuntaryhmä", all=T)



Yhteenlaskettu_peltoala$Ala_yhteensa<-Yhteenlaskettu_peltoala$Grassland_elop_yhteensa+Yhteenlaskettu_peltoala$Grassland_elop_yhteensa


#Vastaako summailtu hehtaarimäärä alkuperäisiä?
sum(Yhteenlaskettu_peltoala$Grassland_elop_yhteensa)+sum(Yhteenlaskettu_peltoala$Cropland_elop_yhteensa)

sum(colSums(Cropland_elop_alat[6:length(Cropland_elop_alat)]))+ 
  sum(colSums(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)]))+ 
  sum(colSums(Grassland_elop_alat[6:length(Grassland_elop_alat)]))+  
  sum(colSums(Grassland_elop_alat_raivio[6:length(Grassland_elop_alat_raivio)]))  

#Kokonaispäästöjen ja yhteenlasketun alan yhdistäminen
Tulos<-merge(Yhteenlaskettu_peltoala, N2O_kokonaispaastot, by="Tuotantosuuntaryhmä", all=T)

library(here)
library(openxlsx)


#Satopohjaiset tuotekertoimet ####

#Cropland-kokonaissadoin (raivatut, raivaamattomat, elop ja mineral)
#Summaus yhdistettyihin päästöihin näistä kategorioista, aggregoituna tuoteryhmille

#Tarvitaan satoskriptin tuottama yhd_sato frame, jossa elop ja mineral croplandin (raivattu ja raivaamaton)
#Yhteenlaskettu sato tuoteryhmittäin. 


#Tähän kiinni tässä skriptissä tuotettava "Cropland_yhdistetyt_paastot_tuoteryhmät"

Satokertoimet<-merge(Cropland_yhdistetyt_paastot_tuoteryhmät,yhd_sato, by="Tuoteryhmä", all=T)

#Kategoria "Kesannot,laitumet,yms." sisältää Siirtonurmen, joka kyllä tuottaa sadon eli kerätään tuote, mutta satokerrointa sille ei ole. 
Satokertoimet<-Satokertoimet %>% filter(complete.cases(.))


#YKSITTÄISTEN VILJOJEN KERTOIMET
#Yhdistetään viljan N2O-päästökilot ja satomäärä 

Paastotonnit_vilja_N2O <-
  Paastotonnit_vilja_N2O %>% pivot_longer(
    cols = 6:length(Paastotonnit_vilja_N2O),
    names_to = "Tuotantosuunta",
    values_to = "Paastokilot_N2O"
  )

Paastotonnit_vilja_N2O<-merge(Paastotonnit_vilja_N2O, Tuotantosuuntaryhmat, by="Tuotantosuunta", all=T)

Paastotonnit_vilja_N2O<-Paastotonnit_vilja_N2O %>% group_by(Kasvikoodi,Kasvinimi,Tuotantosuuntaryhmä) %>% summarise(Paastokilot_N2O = sum(Paastokilot_N2O) )


Viljasato<-merge(Viljasato, Tuotantosuuntaryhmat,  by="Tuotantosuunta")
Viljasato<-Viljasato %>% group_by(Kasvinimi, Kasvikoodi, Tuotantosuuntaryhmä) %>% summarise(Satotonnia = sum(Satotonnia))

Viljasato_paastot_N2O<- merge(Paastotonnit_vilja_N2O, Viljasato, by=c("Kasvikoodi","Kasvinimi","Tuotantosuuntaryhmä"),all=T)



#Europohjaiset kertoimet

#Tarvitaan euromääräskriptissä tuotettu Yhdistetyt_pellon_euroarvot, jossa cropland-tuotteiden eurosummat, sekö yllä mainittu Cropland_yhdistetyt_paastot_tuoteryhmät

Yhdistetyt_pellon_euroarvot <-
  Yhdistetyt_pellon_euroarvot %>% pivot_longer(
    cols = 6:length(Yhdistetyt_pellon_euroarvot),
    names_to = "Tuotantosuunta",
    values_to = "Euroa"
  )

Yhdistetyt_pellon_euroarvot <-
  Yhdistetyt_pellon_euroarvot %>% group_by(Tuoteryhmä) %>% summarise(Euroa =
                                                                       sum(Euroa))

Eurokertoimet <-
  merge(
    Cropland_yhdistetyt_paastot_tuoteryhmät,
    Yhdistetyt_pellon_euroarvot,
    by = "Tuoteryhmä",
    all = T
  )

Eurokertoimet <- Eurokertoimet %>% filter(complete.cases(.))



#Tallennetaan molemmat
library(here)
library(openxlsx)

Tuotekertoimet_tallennus<-createWorkbook()
Tuotekertoimet_tallennus %>% addWorksheet("Satokertoimet")
Tuotekertoimet_tallennus %>% addWorksheet("Eurokertoimet")
Tuotekertoimet_tallennus %>% addWorksheet("Satokertoimet_vilja")
Tuotekertoimet_tallennus %>% addWorksheet("Herkkyystarkastelu_paastot")
Tuotekertoimet_tallennus %>% addWorksheet("Yksittaiset_tuotteet")

Tuotekertoimet_tallennus %>% writeData("Satokertoimet", Satokertoimet)
Tuotekertoimet_tallennus %>% writeData("Eurokertoimet", Eurokertoimet)
Tuotekertoimet_tallennus %>% writeData("Satokertoimet_vilja", Viljasato_paastot_N2O)
Tuotekertoimet_tallennus %>% writeData("Herkkyystarkastelu_paastot",Herkkyystarkastelun_paastot_N2O )
Tuotekertoimet_tallennus %>% writeData("Yksittaiset_tuotteet", Paastot_tuotteille_N2O)




library(here)
library(openxlsx)
write.xlsx(Tulos, here("Output/Yksinkertaistettu_intensiteettilaskenta/N2O_gtk.xlsx"), overwrite = T)
Tuotekertoimet_tallennus %>% saveWorkbook(here("Output/Yksinkertaistettu_intensiteettilaskenta/N2O_tuotekertoimet_gtk.xlsx"), overwrite = T)
saveWorkbook(Raivaus_paasto_N2O, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauspaasto_N2O_gtk.xlsx"),overwrite = T)
saveWorkbook(Raivaamaton_paasto_N2O, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamaton_paasto_N2O_gtk.xlsx"),overwrite = T)




#N2O Laskenta suoritettu


rm(list=ls())
gc()