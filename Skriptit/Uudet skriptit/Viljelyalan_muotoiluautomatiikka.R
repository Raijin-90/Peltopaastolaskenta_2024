library(tidyverse);library(here);library(usefun)


source(here("Skriptit/Biodiversiteettilaskenta/GTK_ala_biodiversiteettiluokittain_ettl.R"))


#Taiton testaus
#16 freimiä samaan listaan

Data_list<-list(
  Grassland_korotettu_mineraalimaa_raivio_moniv,
  Grassland_korotettu_mineraalimaa_raivio_yksiv,
  Cropland_korotettu_mineraalimaa_raivio_moniv,
  Grassland_korotettu_elop_moniv,
  Cropland_korotettu_elop_moniv,
  Grassland_korotettu_elop_yksiv,
  Cropland_korotettu_mineraalimaa_moniv,
  Cropland_korotettu_mineraalimaa_yksiv,
  Cropland_korotettu_elop_raivio_yksiv,
  Cropland_korotettu_mineraalimaa_raivio_yksiv,
  Grassland_korotettu_mineraalimaa_moniv,
  Cropland_korotettu_elop_raivio_moniv,
  Grassland_korotettu_mineraalimaa_yksiv,
  Grassland_korotettu_elop_raivio_moniv,
  Grassland_korotettu_elop_raivio_yksiv,
  Cropland_korotettu_elop_yksiv
)


#nimetään listaelementit lyhyemmin: ei tarvise jättää cropland-grassland-perennial-annual - jakoa kenttiin, se tiedetään jo nimestä 
names(Data_list)<-c("GrassPerMinRaiv",
                    "GrassAnnMinRaiv",
                    "CropPerMinRaiv",
                    "GrassPerOrg",
                    "CropPerOrg",
                    "GrassAnnOrg",
                    "CropPerMin",
                    "CropAnnMin",
                    "CropAnnOrgRaiv",
                    "CropAnnMinRaiv",
                    "GrassPerMin",
                    "CropPerOrgRaiv",
                    "GrassAnnMin",
                    "GrassPerOrgRaiv",
                    "GrassAnnOrgRaiv",
                    "CropAnnOrg")  
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    )

rm.all.but("Data_list")





#Määritellään puuttuvat etol-koodit, jotka pitää lisätä dataan. 

#Täysi listavektori kaikista etol-koodeista alkutuotannossa, järjestettynä oikein.
#Tämän perusteella katsotaan mitä datasta puuttuu ja mitä ei. Se on myös sorttivektori columnien järjestämiseen.  
#Sorttausfunktio perustuu tähän vektoriin, siksi mukaan myös muut sarakkeet

Kaikki_etolkoodit<-c(
  "ETOL",
  "ETTL",
  "ETTL Nimike",
  "Yksi/monivuotinen",
  "Cropland/grassland",
  "Soveltuva_biodiv_kerroin",
  "Skaalattu_kokonaisdiversiteetti",
  "0111a",
  "0111b",
  "0111c",
  "0111d",
  "0111e",
  "0111f",
  "0111g",
  "0111h",
  "0111i",
  "0111k",
  "0111l",
  "01131",
  "01132",
  "01133",
  "01134",
  "01191",
  "0124",
  "0125",
  "0130a",
  "0130b",
  "0141",
  "0142",
  "0143",
  "0145",
  "0146",
  "01471",
  "01472",
  "01491",
  "01492",
  "01499")

#Ne etol-koodit jotka datassa on  

Datan_etolit<-colnames(Data_list[[1]][8:length(Data_list[[1]])])


Lisattavat<-outersect(Datan_etolit, Kaikki_etolkoodit)

#Luodaan funktio, joka muuttaa x:n kolumnin nr. 8 nimeksi "Hehtaaria" ja kääntää pitkän muodon. Lisää puuttuvat etolit. 
  customFunction <- function(x) {
    rename(x, "Hehtaaria" = 8) %>%  pivot_wider(
      names_from = "ETOL_koodi",
      values_from = "Hehtaaria",
      values_fill = 0
    ) 
    
  }

#Map käy läpi data_listin jokaisen alalistan, ja toteuttaa siellä yllä määritellyn kustomfunktion
Data_list<-map(Data_list,\(x) customFunction(x)) 

addColumns<-function(x){
  cbind(
    x,
    `0111i` = 0,
    `01132` = 0,
    `01191` = 0,
    `0130a` = 0,
    `0130b` = 0,
    `01492` = 0) }

Data_list<-map(Data_list,\(x) addColumns(x))

#Sortataan täydennetyn sarakeluettelon perusteella 

Data_list<-map(Data_list, ~ relocate(.x, any_of(Kaikki_etolkoodit)))





