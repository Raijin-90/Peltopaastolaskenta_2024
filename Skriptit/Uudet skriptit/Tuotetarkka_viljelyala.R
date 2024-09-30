library(here);library(tidyverse);library(gt)

#Koostetaan viljelyalasupplementti gtk-aineistosta maalajin ja tuotantosuunnan erottelulla

source(here("Skriptit/Uudet skriptit/GTK_datan_tasokorjaus_ohitus.R"))


source(here("Skriptit/Uudet skriptit/Viljavuus_tasokorjaus_ohitus.R"))


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


#Cropland-ala yksittäisten tuotteiden tarkkuudella7

Cropland<-Cropland %>% group_by(Kasvikoodi,Kasvinimi) %>% summarise(Mineraaliala=sum(Mineral_cropland_yht),
                                                          Eloperainen=sum(Elop_cropland_yht))

Cropland<-Cropland %>% mutate(Ala_yhteensa = Mineraaliala+Eloperainen)

library(here)
write.xlsx(Cropland, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Cropland_ala_kasveittain_gtk.xlsx"))

