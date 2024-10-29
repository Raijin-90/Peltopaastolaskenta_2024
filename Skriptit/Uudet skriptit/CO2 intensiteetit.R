#Lisäys 27.9.2023: Skriptiin dimensioita muutettu ottamaan huomioon turkistilojen lisäys tuotantosuuntien joukkoon. 

library(here)
library(readxl)
library(tidyverse)

#PÄÄSTÖKERROINTEN MÄÄRITTELY ####
#Syötetään päästökertoimet. 
#Noudetaan crf taulusta 2017. Poikkeuksena cropland remaining cropland (organic soils), jossa Perennial/annual jaottelu. Nämä NIR-raportista s. 330, LULUCF-luvun alta.  
#Sisältää "Net carbon stock change in soils", "Living biomass" ja "Dead organic matter" kategoriat.  

#Nielut ja päästöt huomioidaan molemmat. 


#CROPLAND REMAINING CROPLAND, MINERAL SOILS 


viljely_CO2_cropland_mineral<-0.24

## CROPLAND REMAINING CROPLAND, ORGANIC SOILS ####

viljely_CO2_cropland_elop_annual_crops <-28.97 #Tarkistettu. Noudetaan inventaarin raportista s. 330, vastaa arvoa 7.9 t C ha-1 muunnettuna CO2:ksi. 
#Eloperäisen croplandin perennat ja yksivuotiset lasketaan omilla kertiomillaan, jotka NIR raportissa. 



viljely_CO2_cropland_elop_grass<-20.90 #Tarkistettu, Vastaa inventaarin raportin s. 330 arvoa 5.7 t C ha-1 kun muunnetaan CO2:ksi. ( x 3.67)
# kts. inventaari s. 330. 
# For calculating CO2 emissions from cropland remaining as cropland on organic soils, the emission factors are 5.7 t C ha-1 for grass and 7.9 t C ha-1 for annual crops (IPCC 2014b). Muista muuntaa CO2:ksi...
# Vaikka  käyttää termiä "grasses", nyt on puhe CROPLANDIN SISÄLLÄ olevista monivuotisista SADOLLISISTA nurmista, ts. rehunurmista, ja marja-hedelmätuotteista, jotka kuuluvat croplandiin.
# Croplands-kasvit ovat lähestulkoon aina annual, eli niillä on viljelykierto. Ainoastaan monivuotiset rehunurmet ja hedelmät/marjat ovat poikkeuksia, ja tämä kerroin on niille. On myös pienempi, kuin annualien (yllä), kuten kuuluu. 


#GRASSLAND REMAINING GRASSLAND, MINERAL

viljely_CO2_grassland_mineral<- -0.69 #CRF taulusta, päästönielu
  
## GRASSLAND REMAINING GRASSLAND, ORGANIC SOILS. ####
viljely_CO2_grassland_elop<-12.15 # CRF-taulusta. 

#Vastaa Maljanen 2010 kerrointa (3.5 t C ha-1 a-1) inventaarin sivulla 334, joka muunnettu CO2:ksi kertomalla likiarvolla 3.67 (ts. osamäärällä 44/12)
#Inventaari lähtee siitä, että sen "grassland" on voittopuolisesti hylättyä ruohikkomaata. 
#Vastannee hyvin meidän luokitustamme, sillä meidän grassland-kategoriamme sisältää sadottomat ruohikkomaat kuten laitumet, kesannot yms, joita tuskin maanmuokataan merkittävästi. 




## LAND CONVERTED TO CROP/GRASSLAND ####
#Raivauksen CO2

#Näitä käytetään ainoastaan raivaus-frameen, jossa ei ole mukana muita kuin raivatut lohkot. Vastaavasti viljelyframe ei sisällä raivattuja lainkaan. 

raivaus_CO2_cropland_mineral<-5.76 #Tarkistettu. Noudetaan CRF taulusta. Yksi- ja monivuotisia kasveja ei eritellä.  


raivaus_CO2_grassland_mineral<-0.41
#Tarkistettu, noudetaan CRF-taulusta. Yksi ja monivuotisia kasveja ei eritellä. 



raivaus_CO2_cropland_elop<-28.88



raivaus_CO2_grassland_elop<-13.79
 #Sama kerroin kuin viljelty grassland eloperäisillä mailla. 
#Yksi ja monivuotisia ei eritellä. Noudetaan CRF-taulusta. 






#PÄÄSTÖJEN LASKENTA. CO2. . ####
#Kerrotaan lasketut pinta-alat päästökertoimilla. Oikea kerroin määritettävä kohtaan { x * TÄHÄN PÄÄSTÖKERROIN} 
#Mineraalimaa. Näissä mukana ainoastaan raivaamattomat. 
Cropland_korotettu_mineraalimaa[6:length(Cropland_korotettu_mineraalimaa)] <-
  apply(Cropland_korotettu_mineraalimaa[6:length(Cropland_korotettu_mineraalimaa)], 2, function(x) {
    x * viljely_CO2_cropland_mineral
  })

Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)]<-
  apply(Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)], 2, function(x) {
    x * viljely_CO2_grassland_mineral
  })

#Mineraalimaa, raivio

Cropland_korotettu_mineraalimaa_raivio[6:length(Cropland_korotettu_mineraalimaa_raivio)] <-
  apply(Cropland_korotettu_mineraalimaa_raivio[6:length(Cropland_korotettu_mineraalimaa_raivio)], 2, function(x) {
    x * raivaus_CO2_cropland_mineral
  })

Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)]<-
  apply(Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)], 2, function(x) {
    x * raivaus_CO2_grassland_mineral
  })

#Eloperainen maa 

#Erillinen kerroin tämän kategorian annual cropsille, ja grassille (monivuotiset, sadolliset nurmikasvit). Ts. Croplandin sisällä olevat, sadolliset monivuotisnurmet kuten rehunurmet.  
#Otetaan kumpikin erilleen, lasketaan päästö, ja liitetään takaisin yhteen. 

a<-filter(Cropland_korotettu_elop, Cropland_korotettu_elop$`Yksi/monivuotinen` == "Monivuotinen")

a[6:length(a)] <-
  apply(a[6:length(a)], 2, function(x) {
    x * viljely_CO2_cropland_elop_grass
  })

b<- filter(Cropland_korotettu_elop, Cropland_korotettu_elop$`Yksi/monivuotinen` == "Yksivuotinen")

b[6:length(b)] <-
  apply(b[6:length(b)], 2, function(x) {
    x * viljely_CO2_cropland_elop_annual_crops
  })

#Yhdistetään takaisin
Cropland_korotettu_elop<-rbind(a,b)


Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)]<-
  apply(Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)], 2, function(x) {
    x * viljely_CO2_grassland_elop
  })


#Eloperäinen maa, raivio 

Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)] <-
  apply(Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)], 2, function(x) {
    x * raivaus_CO2_cropland_elop
    
  })
Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)]<-
  apply(Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)], 2, function(x) {
    x * raivaus_CO2_grassland_elop
  })



#Raivauksen päästöjen erittely omaan tulostiedostoonsa. Ei intensiteeteiksi muuttoa.

a<-Cropland_korotettu_elop_raivio
b<-Grassland_korotettu_elop_raivio
c<-Cropland_korotettu_mineraalimaa_raivio
d<-Grassland_korotettu_mineraalimaa_raivio


a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "CO2_crop_elop_raiv")
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "CO2_grass_elop_raiv")
c<-c %>% pivot_longer(cols = 6:length(c), names_to = "Tuotantosuunta", values_to = "CO2_crop_min_raiv")
d<-d %>% pivot_longer(cols = 6:length(d), names_to = "Tuotantosuunta", values_to = "CO2_grass_min_raiv")

a<-a %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Crop_elop_CO2 = sum(CO2_crop_elop_raiv))
b<-b %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Grass_elop_CO2 = sum(CO2_grass_elop_raiv))
c<-c %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Crop_min_CO2 = sum(CO2_crop_min_raiv))
d<-d %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Grass_min_CO2 = sum(CO2_grass_min_raiv))

#Croplandit yhteen, niissä samat kasvityypit. Grasslandia ei voi mergetä suoriltaan, siellä eri tuotteita vaikka tuotantosuunnat samoja
x<-merge(a,c, by=c("Tuotantosuunta","Tuoteryhmä"), all=T)
colnames(x)[3:4]<-c("CO2_elop", "CO2_mineral") #Rbindiä varten muutetaan nimet samaksi. Tietoa siitä onko päästö crop- vai grasslandilta ei jatkossa tarvita

#grasslandit yhteen

y<-merge(b,d, by=c("Tuotantosuunta","Tuoteryhmä"),all=T)
colnames(y)[3:4]<-c("CO2_elop", "CO2_mineral")


#Merge ei käy, koska y:n kasvityyppiä ei ole x:ssä. Rbind toimii nyt, koska muuttujanimet samoja

z<-rbind(x,y)

Raivaus_paasto_CO2<-createWorkbook()
addWorksheet(Raivaus_paasto_CO2, "CO2")
writeData(Raivaus_paasto_CO2, "CO2", z)

rm("a","b","c","d","x","y","z")


#Vastaavasti eritellään raivaamattomat
a<-Cropland_korotettu_elop
b<-Grassland_korotettu_elop
c<-Cropland_korotettu_mineraalimaa
d<-Grassland_korotettu_mineraalimaa


a<-a %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "CO2_crop_elop")
b<-b %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "CO2_grass_elop")
c<-c %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "CO2_crop_min")
d<-d %>% pivot_longer(cols = 6:31, names_to = "Tuotantosuunta", values_to = "CO2_grass_min")

a<-a %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Crop_elop_CO2 = sum(CO2_crop_elop))
b<-b %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Grass_elop_CO2 = sum(CO2_grass_elop))
c<-c %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Crop_min_CO2 = sum(CO2_crop_min))
d<-d %>% group_by(Tuotantosuunta, Tuoteryhmä) %>% summarise(Grass_min_CO2 = sum(CO2_grass_min))

#Croplandit yhteen, niissä samat kasvityypit. Grasslandia ei voi mergetä suoriltaan, siellä eri tuotteita vaikka tuotantosuunnat samoja
x<-merge(a,c, by=c("Tuotantosuunta","Tuoteryhmä"), all=T)
colnames(x)[3:4]<-c("CO2_elop", "CO2_mineral") #Rbindiä varten muutetaan nimet samaksi. Tietoa siitä onko päästö crop- vai grasslandilta ei jatkossa tarvita

#grasslandit yhteen

y<-merge(b,d, by=c("Tuotantosuunta","Tuoteryhmä"),all=T)
colnames(y)[3:4]<-c("CO2_elop", "CO2_mineral")


#Merge ei käy, koska y:n kasvityyppiä ei ole x:ssä. Rbind toimii nyt, koska muuttujanimet samoja

z<-rbind(x,y)


sum(a$Crop_elop_CO2)+sum(b$Grass_elop_CO2)+sum(c$Crop_min_CO2)+sum(d$Grass_min_CO2
                                                                   )
sum(z$CO2_elop)+sum(z$CO2_mineral)

Raivaamaton_paasto_CO2<-createWorkbook()
addWorksheet(Raivaamaton_paasto_CO2, "CO2")
writeData(Raivaamaton_paasto_CO2, "CO2", z)


rm("a","b","c","d","x","y")



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
Grassland_korotettu_mineraalimaa_raivio<-
  select(Grassland_korotettu_mineraalimaa_raivio,
         1:5,
         6:length(Grassland_korotettu_mineraalimaa_raivio))
Cropland_korotettu_mineraalimaa_raivio <-
  select(Cropland_korotettu_mineraalimaa_raivio,
         1:5,
         6:length(Cropland_korotettu_mineraalimaa_raivio))

#Merge eloperäiset
Grassland_elop<-  merge(
  Grassland_korotettu_elop,
  Grassland_korotettu_elop_raivio,
  by = c("Kasvikoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
  all = T
)
Grassland_elop[is.na(Grassland_elop)]<-0
sum(colSums(Grassland_elop[6:31]))+
  sum(colSums(Grassland_elop[32:length(Grassland_elop)]))

Cropland_elop <-
  merge(
    Cropland_korotettu_elop,
    Cropland_korotettu_elop_raivio,
    by = c("Kasvikoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
    all = T
  )
Cropland_elop[is.na(Cropland_elop)]<-0
sum(colSums(Cropland_elop[6:31]))+
  sum(colSums(Cropland_elop[32:length(Cropland_elop)]))

#merge mineraali
Grassland_mineral<-  merge(
  Grassland_korotettu_mineraalimaa,
  Grassland_korotettu_mineraalimaa_raivio,
  by = c("Kasvikoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
  all = T
)
Grassland_mineral[is.na(Grassland_mineral)]<-0

sum(colSums(Grassland_mineral[6:31]))+
  sum(colSums(Grassland_mineral[32:length(Grassland_mineral)]))

Cropland_mineral <-
  merge(
    Cropland_korotettu_mineraalimaa,
    Cropland_korotettu_mineraalimaa_raivio,
    by = c("Kasvikoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
    all = T
  )
Cropland_mineral[is.na(Cropland_mineral)]<-0
sum(colSums(Cropland_mineral[6:31]))+
  sum(colSums(Cropland_mineral[32:length(Cropland_mineral)]))

#Tärkeät framet ovat Cropland/Grassland elop ja -mineral. 

#Summataan: x on viljelyn ja y raivauksen päästö, luotu uusi muuttuja näiden summa
#Mutate across ei nyt onnistu koska jokaiselle tuotantosuunnalle tehdään eri operaatio. 

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
                                        Turkistilat = Turkistilat.x+Turkistilat.y,
                                        Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,   
                                        Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,                  
                                        Yrtit = Yrtit.x+Yrtit.y,    
                                        Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                        Öljypellava =Öljypellava.x+Öljypellava.y)

Cropland_mineral<-Cropland_mineral %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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
                                          Turkistilat = Turkistilat.x+Turkistilat.y,
                                          Vihannekset_juurekset = `Vihannekset ja juurekset.x`+`Vihannekset ja juurekset.y`,   
                                          Viljat_pl_ohra = `Viljat pl. ohra.x`+`Viljat pl. ohra.y`,                  
                                          Yrtit = Yrtit.x+Yrtit.y,    
                                          Öljyhamppu = Öljyhamppu.x+Öljyhamppu.y,
                                          Öljypellava =Öljypellava.x+Öljypellava.y)

Grassland_mineral<-Grassland_mineral %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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

#Poistetaan x- ja y-muuttujat, jätetään vain summa. 

Cropland_mineral[6:57]<-NULL
Cropland_elop[6:57]<-NULL
Grassland_mineral[6:57]<-NULL
Grassland_elop[6:57]<-NULL #dimensiot muutettu huomioimaan turkistilojen mukaanotto


sum(Cropland_mineral[6:length(Cropland_mineral)])+sum(Cropland_elop[6:length(Cropland_elop)])

#Kokonaispäästöjen laskenta ####
#Yhdistetään päästöt eloperäiseltä ja mineraalimaalta. Raivaus on laskettu jo mukaan. 

Cropland_yhdistetyt_paastot <-
  merge(
    Cropland_mineral,
    Cropland_elop,
    by = c(
      "Kasvikoodi",
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuoteryhmä"
    ),
    all = T
  )

Cropland_yhdistetyt_paastot[is.na(Cropland_yhdistetyt_paastot)]<-0
#Check sums
sum(colSums(Cropland_yhdistetyt_paastot[6:length(Cropland_yhdistetyt_paastot)])) == sum(Cropland_mineral[6:length(Cropland_mineral)])+sum(Cropland_elop[6:length(Cropland_elop)])

#Sato- ja europohjaiset intensiteetit tarvitsevat croplandin päästöt, mutta ei grasslandia


#Mineraali ja eloperäisen croplandin päästöjen summa
Cropland_yhdistetyt_paastot<- Cropland_yhdistetyt_paastot %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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
Cropland_yhdistetyt_paastot<- Cropland_yhdistetyt_paastot %>% select(1:5,58:length(Cropland_yhdistetyt_paastot))



#Hajontalukujen laskemista (tuoteryhmän sisältämien yksittäisten kerrointen hajonta) varten lasketaan päästotonnit tuotteittain, tuoteryhmittäin summaten


Paastot_tuotteille <-
  Cropland_yhdistetyt_paastot %>% pivot_longer(
    cols = 6:length(Cropland_yhdistetyt_paastot),
    names_to = "Tuotantosuunta",
    values_to = "CO2_tonnia"
  ) %>% group_by(Kasvikoodi, Kasvinimi, Tuoteryhmä) %>%
  summarise(CO2_tonnia = sum(CO2_tonnia))


#Yhdistetään tuotteen CO2-päästöt, sadot ja euromäärä. Näiden oheen tarvitsee liittää vielä N2O, ja muuntaa se CO2eq:ksi

Tuotteiden_eurot_sadot<-merge(Euroarvot_tuotteille, Yhd_sato_tuotteittain, by=c("Kasvikoodi", "Kasvinimi", "Tuoteryhmä"), all=T)

Paastot_tuotteille<-merge(Paastot_tuotteille, Tuotteiden_eurot_sadot, by=c("Kasvikoodi","Kasvinimi","Tuoteryhmä"), all=T)




#Irrotetaan yksittäisten viljojen CO2 emissio

Viljapaastot_CO2<-Cropland_yhdistetyt_paastot %>% filter(Kasvikoodi %in% Viljakoodit)

#Samaten irrotetaan joukko herkkyystarkastelun tuotteita, jotka kattavat joukon erityyppistä tuotantoa

#Lajivektori. Kaikki Croplandia
Kasvilajit<-c("1400",
              "4110",
              "5101",
              "5106",
              "4120")


Herkkyystarkastelun_paastot<- Cropland_yhdistetyt_paastot %>% filter(Kasvikoodi %in% Kasvilajit)



#Tälle tehdään myös tuotteittainen, tuotantosuunnat häivyttävä aggregointi
Cropland_yhdistetyt_paastot_tuoteryhmät<-Cropland_yhdistetyt_paastot

Cropland_yhdistetyt_paastot_tuoteryhmät<-Cropland_yhdistetyt_paastot_tuoteryhmät %>% 
  pivot_longer(cols = 6:length(Cropland_yhdistetyt_paastot_tuoteryhmät), names_to = "Tuotantosuunta", values_to = "Paasto_CO2_t") %>%
  group_by(Tuoteryhmä) %>%
  summarise(Paasto_CO2_t = sum(Paasto_CO2_t))


#Mineraali ja eloperäisen grasslandin päästöjen summa

Grassland_yhdistetyt_paastot<-
  merge(
    Grassland_mineral,
    Grassland_elop,
    by = c(
      "Kasvikoodi",
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuoteryhmä"
    ),
    all = T
  )
Grassland_yhdistetyt_paastot[is.na(Grassland_yhdistetyt_paastot)]<-0
sum(colSums(Grassland_yhdistetyt_paastot[6:length(Grassland_yhdistetyt_paastot)])) == sum(Grassland_mineral[6:length(Grassland_mineral)])+sum(Grassland_elop[6:length(Grassland_elop)])


Grassland_yhdistetyt_paastot<- Grassland_yhdistetyt_paastot %>% mutate(Energiakasvit = Energiakasvit.x+Energiakasvit.y,
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
Grassland_yhdistetyt_paastot<- Grassland_yhdistetyt_paastot %>% select(1:5,58:length(Grassland_yhdistetyt_paastot))


rm(
  Cropland_korotettu_elop,
  Cropland_korotettu_elop_raivio,
  Grassland_korotettu_elop,
  Grassland_korotettu_elop_raivio,
  Cropland_korotettu_mineraalimaa,
  Cropland_korotettu_mineraalimaa_raivio,
  Grassland_korotettu_mineraalimaa,
  Grassland_korotettu_mineraalimaa_raivio
)
rm(Cropland_elop,
   Cropland_mineral,
   Grassland_elop,
   Grassland_mineral 
   )


#Lisäys  08/23: päästöt voidaan myös aggregoida tuotantosuuntatasolle, jotta näkee miten päästökakku jakautuu niiden kesken.
#tUOTANTOSUUNTARYHMÄT, MUUTTAMINEN PITKÄÄN MUOTOON
#Tämän voi tehdä etol-yhteensopivasti tai originaalilla, jolle oma erillinen avain. Alla, kommentoitu pois. 

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



Cropland_yhdistetyt_paastot <- Cropland_yhdistetyt_paastot %>% 
  pivot_longer(cols = 6:length(Cropland_yhdistetyt_paastot), names_to = "Tuotantosuunta", values_to = "Paastotonnit_CO2")

library(usefun)
outersect(Cropland_yhdistetyt_paastot$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)

Cropland_yhdistetyt_paastot <-
  merge(
    Cropland_yhdistetyt_paastot,
    Tuotantosuuntaryhmat,
    by = "Tuotantosuunta",
    all = T
  )


Grassland_yhdistetyt_paastot <- Grassland_yhdistetyt_paastot %>% 
  pivot_longer(cols = 6:length(Grassland_yhdistetyt_paastot), names_to = "Tuotantosuunta", values_to = "Paastotonnit_CO2")


outersect(Grassland_yhdistetyt_paastot$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)


Grassland_yhdistetyt_paastot <-
  merge(
    Grassland_yhdistetyt_paastot,
    Tuotantosuuntaryhmat,
    by = "Tuotantosuunta",
    all = T
  )



Cropland_yhdistetyt_paastot<-Cropland_yhdistetyt_paastot %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Paastotonnit_CO2 = sum(Paastotonnit_CO2))

Grassland_yhdistetyt_paastot<-Grassland_yhdistetyt_paastot %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Paastotonnit_CO2 = sum(Paastotonnit_CO2))

#Cropland- ja grassland-päästöjen summaus
#Summataan nämä kokonaispäästöiksi. Mukaan on laskettu cropland ja grassland, sekä raivattu että raivaamaton, molemmat maalajit 

CO2_kokonaispäästöt<- merge(Cropland_yhdistetyt_paastot, Grassland_yhdistetyt_paastot, by=c("Tuotantosuuntaryhmä"), all=T)
CO2_kokonaispäästöt$CO2_yhteensa<-CO2_kokonaispäästöt$Paastotonnit_CO2.x+CO2_kokonaispäästöt$Paastotonnit_CO2.y
CO2_kokonaispäästöt<-CO2_kokonaispäästöt %>% select(1,4)



#PINTA-ALOJEN SUMMAUS ####
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

Cropland_elop_yhteisala[is.na(Cropland_elop_yhteisala)] <- 0

Cropland_elop_yhteisala$Hehtaaria_yhteensa_elop <-
  Cropland_elop_yhteisala$Hehtaaria_crop_elop + Cropland_elop_yhteisala$Hehtaaria_crop_elop_raivio


outersect(Cropland_elop_yhteisala$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)

#Summien tarkistus
sum(Cropland_elop_yhteisala$Hehtaaria_crop_elop) == sum(colSums(Cropland_elop_alat[6:length(Cropland_elop_alat)]))
sum(Cropland_elop_yhteisala$Hehtaaria_crop_elop_raivio) == sum(colSums(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)]))

#Mineraali
a <-
  Cropland_mineraalimaa_alat %>% pivot_longer(
    cols = 6:length(Cropland_mineraalimaa_alat),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_crop_min"
  )
b <-
  Cropland_mineraalimaa_alat_raivio %>% pivot_longer(
    cols = 6:length(Cropland_mineraalimaa_alat_raivio),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_crop_min_raivio"
  )

Cropland_min_yhteisala <-
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
Cropland_min_yhteisala[is.na(Cropland_min_yhteisala)] <- 0

Cropland_min_yhteisala$Hehtaaria_yhteensa_min <-
  Cropland_min_yhteisala$Hehtaaria_crop_min + Cropland_min_yhteisala$Hehtaaria_crop_min_raivio

#Summien tarkistus
sum(Cropland_min_yhteisala$Hehtaaria_crop_min_raivio) == sum(colSums(Cropland_mineraalimaa_alat_raivio[6:length(Cropland_mineraalimaa_alat_raivio)]))
sum(Cropland_min_yhteisala$Hehtaaria_crop_min) == sum(colSums(Cropland_mineraalimaa_alat[6:length(Cropland_mineraalimaa_alat)]))

outersect(Cropland_min_yhteisala$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)

#Grassland

a <-
  Grassland_elop_alat %>% pivot_longer(
    cols = 6:length(Cropland_elop_alat),
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

#Summien tarkistus
sum(Grassland_elop_yhteisala$Hehtaaria_grass_elop) == sum(colSums(Grassland_elop_alat[6:length(Grassland_elop_alat)]))
sum(Grassland_elop_yhteisala$Hehtaaria_grass_elop_raivio) == sum(colSums(Grassland_elop_alat_raivio[6:length(Grassland_elop_alat_raivio)]))

outersect(Grassland_elop_yhteisala$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)


#Mineraali
a <-
  Grassland_mineraalimaa_alat %>% pivot_longer(
    cols = 6:length(Grassland_mineraalimaa_alat),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_grass_min"
  )
b <-
  Grassland_mineraalimaa_alat_raivio %>% pivot_longer(
    cols = 6:length(Grassland_mineraalimaa_alat_raivio),
    names_to = "Tuotantosuunta",
    values_to = "Hehtaaria_grass_min_raivio"
  )

Grassland_min_yhteisala <-
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
Grassland_min_yhteisala[is.na(Grassland_min_yhteisala)] <- 0

Grassland_min_yhteisala$Hehtaaria_yhteensa_min <-
  Grassland_min_yhteisala$Hehtaaria_grass_min + Grassland_min_yhteisala$Hehtaaria_grass_min_raivio

#Summien tarkistus
sum(Grassland_min_yhteisala$Hehtaaria_grass_min_raivio) == sum(colSums(Grassland_mineraalimaa_alat_raivio[6:length(Grassland_mineraalimaa_alat_raivio)]))
sum(Grassland_min_yhteisala$Hehtaaria_grass_min) == sum(colSums(Grassland_mineraalimaa_alat[6:length(Grassland_mineraalimaa_alat)]))


#Kokonaisaloiksi summaus:
#Äsken yhdistettiin raivattu ja raivaamaton kutakin maalajia, nyt maalajit yhdistetään. 
#totaaliin sisältyy raivattu ja raivaamaton ala kutakin maalajia ja kasvityyppiä
#Grassland

Grassland_kokonaisala<-merge(Grassland_min_yhteisala,
                             Grassland_elop_yhteisala, by= c(
                               "Kasvikoodi",
                               "Kasvinimi",
                               "Cropland/grassland",
                               "Yksi/monivuotinen",
                               "Tuotantosuunta",
                               "Tuoteryhmä"
                             ),
                             all = T
)


Grassland_kokonaisala<-merge(Grassland_kokonaisala, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"), all=T)
Grassland_kokonaisala[is.na(Grassland_kokonaisala)]<-0


Grassland_kokonaisala$Grassland_hehtaaria<-Grassland_kokonaisala$Hehtaaria_yhteensa_min+Grassland_kokonaisala$Hehtaaria_yhteensa_elop


Grassland_kokonaisala<-Grassland_kokonaisala %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Grassland_hehtaaria = sum(Grassland_hehtaaria))


#Cropland
Cropland_kokonaisala<-merge(Cropland_min_yhteisala,
                            Cropland_elop_yhteisala, by= c(
                              "Kasvikoodi",
                              "Kasvinimi",
                              "Cropland/grassland",
                              "Yksi/monivuotinen",
                              "Tuotantosuunta",
                              "Tuoteryhmä"
                            ),
                            all = T
)

#Tallennetaan croplandin kokonaisala tuotetasolla tehtävään herkkyystarkasteluun
Cropland_kokonaisala_herkkyyst<-Cropland_kokonaisala



Cropland_kokonaisala<-merge(Cropland_kokonaisala, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"), all=T)
Cropland_kokonaisala[is.na(Cropland_kokonaisala)]<-0

Cropland_kokonaisala$Cropland_hehtaaria <-
  Cropland_kokonaisala$Hehtaaria_yhteensa_min + Cropland_kokonaisala$Hehtaaria_yhteensa_elop


Cropland_kokonaisala<-Cropland_kokonaisala %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Cropland_hehtaaria = sum(Cropland_hehtaaria))


#Yhteenlaskettu ala
#Lopuksi summataan kokonaisala croplandia ja grasslandia...

Yhteenlaskettu_peltoala<-merge(Cropland_kokonaisala,
                               Grassland_kokonaisala, by="Tuotantosuuntaryhmä", all=T)
Yhteenlaskettu_peltoala$Ala_yhteensa<-Yhteenlaskettu_peltoala$Cropland_hehtaaria+Yhteenlaskettu_peltoala$Grassland_hehtaaria



#Vastaako summailtu hehtaarimäärä alkuperäisiä?
sum(Yhteenlaskettu_peltoala$Cropland_hehtaaria)+sum(Yhteenlaskettu_peltoala$Grassland_hehtaaria)

sum(colSums(Cropland_elop_alat[6:length(Cropland_elop_alat)]))+ 
  sum(colSums(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)]))+ 
  sum(colSums(Cropland_mineraalimaa_alat[6:length(Cropland_mineraalimaa_alat)]))+ 
  sum(colSums(Cropland_mineraalimaa_alat_raivio[6:length(Cropland_mineraalimaa_alat_raivio)]))+
  sum(colSums(Grassland_mineraalimaa_alat[6:length(Grassland_mineraalimaa_alat)]))+
  sum(colSums(Grassland_mineraalimaa_alat_raivio[6:length(Grassland_mineraalimaa_alat_raivio)]))+
  sum(colSums(Grassland_elop_alat[6:length(Grassland_elop_alat)]))+  
  sum(colSums(Grassland_elop_alat_raivio[6:length(Grassland_elop_alat_raivio)]))  




#Kokonaispäästöjen ja yhteenlasketun alan yhdistäminen
Tulos<-merge(Yhteenlaskettu_peltoala, CO2_kokonaispäästöt, by="Tuotantosuuntaryhmä", all=T)





#Satopohjaiset tuotekertoimet ####

#Cropland-kokonaissadoin (raivatut, raivaamattomat, elop ja mineral)
#Summaus yhdistettyihin päästöihin näistä kategorioista, aggregoituna tuoteryhmille

#Tarvitaan satoskriptin tuottama yhd_sato frame, jossa elop ja mineral croplandin (raivattu ja raivaamaton)
#Yhteenlaskettu sato tuoteryhmittäin. 


#Tähän kiinni tässä skriptissä tuotettava "Cropland_yhdistetyt_paastot_tuoteryhmät"





Satokertoimet<-merge(Cropland_yhdistetyt_paastot_tuoteryhmät,yhd_sato, by="Tuoteryhmä", all=T)

#Kategoria "Kesannot,laitumet,yms." sisältää Siirtonurmen, joka kyllä tuottaa sadon eli kerätään tuote, mutta satokerrointa sille ei ole. 
Satokertoimet<-Satokertoimet %>% filter(complete.cases(.))

Satokertoimet<- Satokertoimet %>% mutate(Kerroin_t_CO2_tn= Paasto_CO2_t/Satotonnia)


#Europohjaiset kertoimet #### 
#kaikille tuoteryhmille

#Tarvitaan euromääräskriptissä tuotettu Yhdistetyt_pellon_euroarvot, jossa cropland-tuotteiden eurosummat, sekö yllä mainittu Cropland_yhdistetyt_paastot_tuoteryhmät

Yhdistetyt_pellon_euroarvot <-
  Yhdistetyt_pellon_euroarvot %>% pivot_longer(
    cols = 6:length(Yhdistetyt_pellon_euroarvot),
    names_to = "Tuotantosuunta",
    values_to = "Euroa"
  )
Yhdistetyt_pellon_euroarvot <-
  Yhdistetyt_pellon_euroarvot %>% group_by(Tuoteryhmä) %>% summarise(Euroa = sum(Euroa))
Yhdistetyt_pellon_euroarvot <-
  Yhdistetyt_pellon_euroarvot %>% mutate(Tuhatta_euroa = Euroa / 1000)


Eurokertoimet<-merge(Cropland_yhdistetyt_paastot_tuoteryhmät, Yhdistetyt_pellon_euroarvot, by="Tuoteryhmä", all=T)

Eurokertoimet<-Eurokertoimet %>% filter(complete.cases(.))

Eurokertoimet<- Eurokertoimet %>% mutate(Kerroin_t_CO2_kEUR = Paasto_CO2_t/Tuhatta_euroa)

#Viljojen yksittäiset kertoimet
#Yhdistetaan yksittaisten viljojen sadot ja CO2-paastot



Viljapaastot_CO2 <-
  Viljapaastot_CO2 %>% pivot_longer(
    cols = 6:length(Viljapaastot_CO2),
    values_to = "Paastotonnit_CO2",
    names_to = "Tuotantosuunta"
  )

Viljapaastot_CO2 <-
  merge(Viljapaastot_CO2,
        Tuotantosuuntaryhmat,
        by = "Tuotantosuunta",
        all = T)

Viljapaastot_CO2 <-
  Viljapaastot_CO2 %>% group_by(Kasvinimi, Kasvikoodi, Tuotantosuuntaryhmä) %>% summarise(Paastotonnit_CO2 = sum(Paastotonnit_CO2))


Viljasato<-merge(Viljasato, Tuotantosuuntaryhmat, by="Tuotantosuunta", all=T)

Viljasato<-Viljasato %>% group_by(Kasvinimi, Kasvikoodi, Tuotantosuuntaryhmä) %>% summarise(Satotonnia = sum(Satotonnia))

Viljasato_paastot<- merge(Viljapaastot_CO2, Viljasato, by=c("Kasvikoodi","Kasvinimi","Tuotantosuuntaryhmä"),all=T)



#Tallennetaan molemmat
library(here)
library(openxlsx)

Tuotekertoimet_tallennus<-createWorkbook()
Tuotekertoimet_tallennus %>% addWorksheet("Satokertoimet")
Tuotekertoimet_tallennus %>% addWorksheet("Eurokertoimet")
Tuotekertoimet_tallennus %>% addWorksheet("Satokertoimet_vilja")
Tuotekertoimet_tallennus %>% addWorksheet("Herkkyystarkastelu_paastot")
Tuotekertoimet_tallennus %>% addWorksheet("Herkkyystarkastelu_alat")
Tuotekertoimet_tallennus %>% addWorksheet("Yksittaiset_tuotteet")


Tuotekertoimet_tallennus %>% writeData("Satokertoimet", Satokertoimet)
Tuotekertoimet_tallennus %>% writeData("Eurokertoimet", Eurokertoimet)
Tuotekertoimet_tallennus %>% writeData("Satokertoimet_vilja", Viljasato_paastot)
Tuotekertoimet_tallennus %>% writeData("Herkkyystarkastelu_paastot", Herkkyystarkastelun_paastot)
Tuotekertoimet_tallennus %>% writeData("Herkkyystarkastelu_alat", Cropland_kokonaisala_herkkyyst)
Tuotekertoimet_tallennus %>% writeData("Yksittaiset_tuotteet", Paastot_tuotteille)


library(here)
library(openxlsx)
write.xlsx(Tulos, here("Output/Yksinkertaistettu_intensiteettilaskenta/CO2_gtk.xlsx"), overwrite = T)
Tuotekertoimet_tallennus %>% saveWorkbook(here("Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_gtk.xlsx"), overwrite = T)
saveWorkbook(Raivaus_paasto_CO2, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivauspaasto_CO2_gtk.xlsx"),overwrite = T)
saveWorkbook(Raivaamaton_paasto_CO2, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Raivaamaton_paasto_CO2_gtk.xlsx"),overwrite = T)




#CO2 Laskenta suoritettu

rm(list=ls())
gc()