library(here);library(tidyverse);library(gt)

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                                                        sheet = "Tuotantosuunnat ryhmittäin")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-c("Tuotantosuunta")



#Koostetaan viljelyalasupplementti gtk-aineistosta maalajin ja tuotantosuunnan erottelulla. 
#Tarkoitus on mahdollistaa sen vertailu, kuinka eloperäisen maan osuus vaihtelee eri tuotantosuuntien kesken. 

source(here("Skriptit/Uudet skriptit/GTK_datan_tasokorjaus_ohitus.R"))


#Cropland-luokat

#Mineraali cropland
Cropland_mineraalimaa_alat <-
  Cropland_mineraalimaa_alat %>% pivot_longer(
    cols = 6:length(Cropland_mineraalimaa_alat),
    names_to = "Tuotantosuunta",
    values_to = "mineraaliala_cropl"
  ) 
Cropland_mineraalimaa_alat<-Cropland_mineraalimaa_alat %>% select(1,2,6,7)

sum(Cropland_mineraalimaa_alat$mineraaliala_cropl)


Cropland_mineraalimaa_alat_raivio <-
  Cropland_mineraalimaa_alat_raivio %>% pivot_longer(
    cols = 6:length(Cropland_mineraalimaa_alat_raivio),
    names_to = "Tuotantosuunta",
    values_to = "mineraaliala_cropl_raiv"
  ) 
Cropland_mineraalimaa_alat_raivio<-Cropland_mineraalimaa_alat_raivio %>% select(1,2,6,7)

sum(Cropland_mineraalimaa_alat_raivio$mineraaliala_cropl_raiv)

Mineraaliala_Cropland <-
  merge(
    Cropland_mineraalimaa_alat,
    Cropland_mineraalimaa_alat_raivio,
    by = c("Kasvikoodi", "Kasvinimi", "Tuotantosuunta"), all=T
  )

Mineraaliala_Cropland[is.na(Mineraaliala_Cropland)]<-0 #Joissain tapauksissa raivattua alaa ei ole, tulee NA

Mineraaliala_Cropland$Mineral_cropland_yht<-Mineraaliala_Cropland$mineraaliala_cropl+Mineraaliala_Cropland$mineraaliala_cropl_raiv


sum(Mineraaliala_Cropland$Mineral_cropland_yht)

sum(Cropland_mineraalimaa_alat_raivio$mineraaliala_cropl_raiv)+sum(Cropland_mineraalimaa_alat$mineraaliala_cropl)




#Eloperäinen cropland

Cropland_elop_alat <-
  Cropland_elop_alat %>% pivot_longer(
    cols = 6:length(Cropland_elop_alat),
    names_to = "Tuotantosuunta",
    values_to = "elop_ala_cropl"
  ) 
Cropland_elop_alat<-Cropland_elop_alat %>% select(1,2,6,7)

sum(Cropland_elop_alat$elop_ala_cropl)

Cropland_elop_alat_raivio <-
  Cropland_elop_alat_raivio %>% pivot_longer(
    cols = 6:length(Cropland_elop_alat_raivio),
    names_to = "Tuotantosuunta",
    values_to = "elop_ala_cropl_raiv"
  ) 
Cropland_elop_alat_raivio<-Cropland_elop_alat_raivio %>% select(1,2,6,7)

sum(Cropland_elop_alat_raivio$elop_ala_cropl_raiv)

Elop_ala_cropland <-
  merge(
    Cropland_elop_alat,
    Cropland_elop_alat_raivio,
    by = c("Kasvikoodi", "Kasvinimi", "Tuotantosuunta"), all=T
  )

Elop_ala_cropland[is.na(Elop_ala_cropland)]<-0 #Joissain tapauksissa raivattua alaa ei ole, tulee NA

Elop_ala_cropland$Elop_cropland_yht<-Elop_ala_cropland$elop_ala_cropl+Elop_ala_cropland$elop_ala_cropl_raiv

sum(Elop_ala_cropland$Elop_cropland_yht)

sum(Cropland_elop_alat_raivio$elop_ala_cropl_raiv)+sum(Cropland_elop_alat$elop_ala_cropl)

#Yhdistäminen

Cropland <- merge(Mineraaliala_Cropland, Elop_ala_cropland, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"),  all=T)

Cropland[is.na(Cropland)]<-0

Cropland$Cropland_yhteensa <- Cropland$Mineral_cropland_yht+Cropland$Elop_cropland_yht



sum(Cropland$Cropland_yhteensa)

sum(Cropland_mineraalimaa_alat_raivio$mineraaliala_cropl_raiv) + sum(Cropland_mineraalimaa_alat$mineraaliala_cropl) +
  sum(Cropland_elop_alat$elop_ala_cropl) + sum(Cropland_elop_alat_raivio$elop_ala_cropl_raiv)


#Cropland erikseen, siten, että saadaan yksittäisten tuotteiden alat per tuotantosuunta ja kategoria

Tuote_tuotantosuunta_crops<-Cropland %>% group_by(Kasvikoodi, Kasvinimi, Tuotantosuunta) %>% summarise(Viljelty_mineraali= sum(mineraaliala_cropl),
                                                                           Raivattu_mineraali = sum(mineraaliala_cropl_raiv),
                                                                           Viljelty_eloperainen = sum(elop_ala_cropl),
                                                                           Raivattu_eloperainen = sum(elop_ala_cropl_raiv))


Tuote_tuotantosuunta_crops<-merge(Tuote_tuotantosuunta_crops, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta", all=T)


Tuote_tuotantosuunta_crops<-Tuote_tuotantosuunta_crops %>% group_by(ETOL,Kasvikoodi,Kasvinimi) %>% summarise(Viljelty_mineraali= sum(Viljelty_mineraali),
                                                                      Raivattu_mineraali = sum(Raivattu_mineraali),
                                                                      Viljelty_eloperainen = sum(Viljelty_eloperainen),
                                                                      Raivattu_eloperainen = sum(Raivattu_eloperainen))


Tuote_tuotantosuunta_crops<-Tuote_tuotantosuunta_crops %>% mutate(Mineraali_yhteensa =Viljelty_mineraali+Raivattu_mineraali,
                                      Eloperainen_yhteensa =Viljelty_eloperainen+Raivattu_eloperainen,
                                      Kokonaisala =Mineraali_yhteensa+Eloperainen_yhteensa)

sum(Tuote_tuotantosuunta_crops$Kokonaisala)


write.xlsx(Tuote_tuotantosuunta_crops, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Tuote_tuotantosuunta_alat_crops_gtk.xlsx "), overwrite = T)




#Grasses
#Mineraali grassland

Grassland_mineraalimaa_alat<-Grassland_mineraalimaa_alat %>% pivot_longer(
  cols = 6:length(Grassland_mineraalimaa_alat),
  names_to = "Tuotantosuunta",
  values_to = "mineraaliala_grass"
) 
Grassland_mineraalimaa_alat<-Grassland_mineraalimaa_alat %>% select(1,2,6,7)

sum(Grassland_mineraalimaa_alat$mineraaliala_grass)


Grassland_mineraalimaa_alat_raivio<-Grassland_mineraalimaa_alat_raivio %>% pivot_longer(
  cols = 6:length(Grassland_mineraalimaa_alat_raivio),
  names_to = "Tuotantosuunta",
  values_to = "mineraaliala_grass_raiv"
) 
Grassland_mineraalimaa_alat_raivio<-Grassland_mineraalimaa_alat_raivio %>% select(1,2,6,7)

Grassland_mineraalimaa_alat_raivio[is.na(Grassland_mineraalimaa_alat_raivio)]<-0 #Joissain tapauksissa raivattua alaa ei ole, tulee NA

sum(Grassland_mineraalimaa_alat_raivio$mineraaliala_grass_raiv)


Grassmin<-  merge(
  Grassland_mineraalimaa_alat,
  Grassland_mineraalimaa_alat_raivio,
  by = c("Kasvikoodi", "Kasvinimi", "Tuotantosuunta"), all=T
)


Grassmin[is.na(Grassmin)]<-0

Grassmin$Mineral_grassland<-Grassmin$mineraaliala_grass+Grassmin$mineraaliala_grass_raiv

sum(Grassmin$Mineral_grassland)
sum(Grassland_mineraalimaa_alat$mineraaliala_grass)+sum(Grassland_mineraalimaa_alat_raivio$mineraaliala_grass_raiv)



#Eloperäinen grassland


Grassland_elop_alat<-Grassland_elop_alat %>% pivot_longer(
  cols = 6:length(Grassland_elop_alat),
  names_to = "Tuotantosuunta",
  values_to = "elop_ala_grass"
) 
Grassland_elop_alat<-Grassland_elop_alat %>% select(1,2,6,7)

sum(Grassland_elop_alat$elop_ala_grass)



Grassland_elop_alat_raivio<-Grassland_elop_alat_raivio %>% pivot_longer(
  cols = 6:length(Grassland_elop_alat_raivio),
  names_to = "Tuotantosuunta",
  values_to = "elop_ala_grass_raiv"
) 
Grassland_elop_alat_raivio<-Grassland_elop_alat_raivio %>% select(1,2,6,7)

Grassland_elop_alat_raivio[is.na(Grassland_elop_alat_raivio)]<-0 #Joissain tapauksissa raivattua alaa ei ole, tulee NA

sum(Grassland_elop_alat_raivio$elop_ala_grass_raiv)



Grassorg<-  merge(
  Grassland_elop_alat,
  Grassland_elop_alat_raivio,
  by = c("Kasvikoodi", "Kasvinimi", "Tuotantosuunta"), all=T
)

Grassorg[is.na(Grassorg)]<-0

Grassorg$Elop_grassland<-Grassorg$elop_ala_grass+Grassorg$elop_ala_grass_raiv

sum(Grassorg$Elop_grassland)


Grassland<- merge(Grassmin, Grassorg, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"),  all=T)

Grassland[is.na(Grassland)]<-0

Grassland$Grassland_yhteensa<-Grassland$Mineral_grassland+Grassland$Elop_grassland

sum(Grassland$Grassland_yhteensa)

sum(Grassmin$Mineral_grassland)+sum(Grassorg$Elop_grassland)




sum(Grassland$Grassland_yhteensa)+sum(Cropland$Cropland_yhteensa)


#Aggregointi

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                                                        sheet = "Tuotantosuunnat ryhmittäin")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-c("Tuotantosuunta")


z<-merge(Cropland, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta", all=T)

x<-merge(Grassland, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta", all=T)




z<-z %>% group_by(ETOL) %>% summarise(Viljeltyä_mineraalia_cropland = sum(mineraaliala_cropl),
                                      Raivattua_mineraalia_cropland = sum(mineraaliala_cropl_raiv),
                                      Mineraalimaata_yhteensa_cropland = sum(Mineral_cropland_yht),
                                      Viljeltyä_eloperaista_cropland = sum(elop_ala_cropl),
                                      Raivattua_eloperäistä_cropland = sum(elop_ala_cropl_raiv),
                                      Eloperaista_yhteensa_cropland = sum(Elop_cropland_yht),
                                      Croplandia_yhteensa = sum(Cropland_yhteensa)
                                   )


sum(Cropland_elop_alat$elop_ala_cropl)+sum(Cropland_elop_alat_raivio$elop_ala_cropl_raiv)
sum(z$Eloperaista_yhteensa_cropland)


sum(Cropland_mineraalimaa_alat$mineraaliala_cropl)+sum(Cropland_mineraalimaa_alat_raivio$mineraaliala_cropl_raiv)
sum(z$Mineraalimaata_yhteensa_cropland)





x<-x %>% group_by(ETOL) %>% summarise(Viljeltyä_mineraalia_grassland = sum(mineraaliala_grass),
                                      Raivattua_mineraalia_grassland = sum(mineraaliala_grass_raiv),
                                      Mineraalimaata_yhteensa_grassland = sum(Mineral_grassland),
                                      Viljeltyä_eloperaista_grassland = sum(elop_ala_grass),
                                      Raivattua_eloperäistä_grassland = sum(elop_ala_grass_raiv),
                                      Eloperaista_yhteensa_grassland = sum(Elop_grassland),
                                     Grasslandia_yhteensa = sum(Grassland_yhteensa)
)



sum(Grassland_elop_alat$elop_ala_grass)+sum(Grassland_elop_alat_raivio$elop_ala_grass_raiv)
sum(x$Eloperaista_yhteensa_grassland)

sum(Grassland_mineraalimaa_alat$mineraaliala_grass)+sum(Grassland_mineraalimaa_alat_raivio$mineraaliala_grass_raiv)
sum(x$Mineraalimaata_yhteensa_grassland)


#Yhdistetään 
 
Supplement<-merge(z, x, by="ETOL")


Supplement$Mineraalia_yhteensa<-Supplement$Mineraalimaata_yhteensa_cropland+Supplement$Mineraalimaata_yhteensa_grassland

Supplement$Eloperaista_yhteensa<-Supplement$Eloperaista_yhteensa_cropland+Supplement$Eloperaista_yhteensa_grassland

Supplement$Kokonaisala<-Supplement$Eloperaista_yhteensa+Supplement$Mineraalia_yhteensa

Supplement$MineraaliPros<-(Supplement$Mineraalia_yhteensa/Supplement$Kokonaisala)*100

Supplement$ElopPros<-(Supplement$Eloperaista_yhteensa/Supplement$Kokonaisala)*100


sum(Supplement$Mineraalimaata_yhteensa_cropland+Supplement$Mineraalimaata_yhteensa_grassland)

sum(Supplement$Eloperaista_yhteensa_cropland+Supplement$Eloperaista_yhteensa_grassland)



sum(Supplement$Mineraalimaata_yhteensa_cropland+Supplement$Mineraalimaata_yhteensa_grassland)+

sum(Supplement$Eloperaista_yhteensa_cropland+Supplement$Eloperaista_yhteensa_grassland)





write.xlsx(Supplement, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/GTK_viljelyalat_supplement.xlsx"), overwrite = T)
